# --- (Keep existing @import tags and function definition) ---
#' @import shiny
#' @importFrom glue glue
#' @importFrom rlang inform warn abort is_list `%||%` check_installed is_scalar_character
#' @importFrom yaml read_yaml
#' @importFrom fs path_norm dir_exists file_exists path
#' @importFrom utils packageVersion browseURL askYesNo # Added askYesNo
#' @importFrom tools file_path_sans_ext
#' @importFrom jsonlite fromJSON toJSON # Added jsonlite imports explicitly

df_shiny_assess <- function(launch.browser = TRUE, ...) {

  # --- Check suggested packages ---
  rlang::check_installed("shiny", reason = "to run the interactive assessment app.")
  copy_available <- rlang::is_installed("rclipboard")

  # --- UI Definition ---
  ui <- fluidPage(
    title = "DataFindR Paper Assessment",
    if (copy_available) rclipboard::rclipboardSetup(),

    sidebarLayout(
      sidebarPanel(
        width = 4,
        h4("1. Project & Paper"),
        textInput("metawoRldPath", "metawoRld Project Path:", value = ".", placeholder = "Path to project root"),
        textInput("identifier", "DOI or PMID:", placeholder = "e.g., 10.1234/abc or 12345678"),
        actionButton("fetchMeta", "Fetch Title/Abstract", icon = icon("cloud-download-alt")),
        hr(),
        h4("Optional: Manual Input/Edit"),
        textAreaInput("manualTitle", "Title:", rows = 3),
        textAreaInput("manualAbstract", "Abstract:", rows = 10),
        hr(),
        h4("2. Assess Relevance"),
        actionButton("runAssessment", "Run LLM Assessment", icon = icon("rocket"), class = "btn-primary"),
        p(em("Requires API key (e.g., OPENAI_API_KEY) set in .Renviron & R restart.")),
        selectInput("llmService", "LLM Service", choices = c("openai", "google"), selected = "google"),
        textInput("llmModel", "LLM Model", value = "gemini-2.5-flash-preview-04-17")
      ),

      mainPanel(
        width = 8,
        h4("Fetched / Manual Paper Info:"),
        strong("Title:"),
        verbatimTextOutput("displayTitle", placeholder = TRUE),
        strong("Abstract:"),
        verbatimTextOutput("displayAbstract", placeholder = TRUE),
        hr(),
        h4("Generated Assessment Prompt:"),
        uiOutput("promptSection"), # Use uiOutput for button placement
        hr(),
        h4("Assessment Result (Live / From Cache / Manual):"),
        verbatimTextOutput("assessmentResult", placeholder = TRUE),

        # --- NEW: Manual Assessment Input/Save ---
        hr(),
        h4("Manual Assessment JSON Input/Edit:"),
        textAreaInput("manualAssessmentJson", label=NULL, rows = 10, width="100%",
                      placeholder = 'Paste or edit assessment JSON here, e.g.,\n{\n  "decision": "Include",\n  "score": 0.9,\n  "rationale": "Manual assessment based on full text."\n}'),
        actionButton("saveManualAssessment", "Save Manual Assessment to Cache", icon = icon("save"), class = "btn-warning")
        # --- End NEW Section ---
      )
    )
  )

  # --- Server Logic ---
  server <- function(input, output, session) {

    # --- Reactive Values ---
    rv <- reactiveValues(
      metadata = NULL,
      assessment = NULL,    # Holds the *currently displayed* assessment (from API or cache or manual save)
      project_valid = FALSE,
      criteria = NULL
    )

    # --- (Keep existing Project Validation observe block) ---
    observe({
      req(input$metawoRldPath)
      proj_path <- fs::path_norm(input$metawoRldPath)
      config_path <- fs::path(proj_path, "_metawoRld.yml")

      if (fs::dir_exists(proj_path) && fs::file_exists(config_path)) {
        tryCatch({
          project_config <- yaml::read_yaml(config_path)
          incl_crit <- project_config$inclusion_criteria %||% character(0)
          excl_crit <- project_config$exclusion_criteria %||% character(0)

          if (length(incl_crit) == 0 && length(excl_crit) == 0) {
            showNotification("Project config found, but no inclusion/exclusion criteria defined.", type = "warning", duration=10, id="proj_warn")
            rv$project_valid <- FALSE
            rv$criteria <- NULL
          } else {
            removeNotification("proj_warn") # Remove warning if criteria found later
            showNotification("metawoRld project and criteria loaded.", type = "message", duration=5, id="proj_msg")
            rv$project_valid <- TRUE
            rv$criteria <- list(inclusion = incl_crit, exclusion = excl_crit)
          }
        }, error = function(e) {
          showNotification(paste("Error reading project config:", e$message), type = "error", duration=10)
          rv$project_valid <- FALSE
          rv$criteria <- NULL
        })
      } else {
        removeNotification("proj_msg")
        removeNotification("proj_warn")
        rv$project_valid <- FALSE
        rv$criteria <- NULL
      }
    })

    # --- Check Cache on Identifier Change ---
    # NEW: Automatically check cache when identifier changes and populate fields
    observe({
      req(input$identifier)
      req(input$metawoRldPath)
      req(rv$project_valid) # Only check if project is valid

      # Check assessment cache first
      cached_assessment <- .check_cache(input$identifier, type = "assessment", metawoRld_path = input$metawoRldPath)
      if(!is.null(cached_assessment) && is.list(cached_assessment)) {
        # Basic validation
        if(all(c("decision", "score", "rationale") %in% names(cached_assessment))) {
          rv$assessment <- cached_assessment
          showNotification("Loaded assessment from cache.", type = "message", duration = 5)
        } else {
          rv$assessment <- NULL # Clear if invalid structure
        }
      } else {
        rv$assessment <- NULL # Clear if not found
      }

      # Check metadata cache (or clear if assessment was found, maybe not necessary)
      cached_metadata <- .check_cache(input$identifier, type = "metadata", metawoRld_path = input$metawoRldPath)
      if(!is.null(cached_metadata) && is.list(cached_metadata)) {
        if(!is.null(cached_metadata$title) && !is.null(cached_metadata$abstract)){
          rv$metadata <- cached_metadata
          # Only update text areas if they are currently empty or match previous fetched
          current_title <- trimws(input$manualTitle)
          current_abstract <- trimws(input$manualAbstract)
          if (current_title == "" || (!is.null(rv$metadata$title) && current_title == rv$metadata$title)) {
            updateTextAreaInput(session, "manualTitle", value = cached_metadata$title %||% "")
          }
          if (current_abstract == "" || (!is.null(rv$metadata$abstract) && current_abstract == rv$metadata$abstract)) {
            updateTextAreaInput(session, "manualAbstract", value = cached_metadata$abstract %||% "")
          }
          showNotification("Loaded metadata from cache.", type = "message", duration = 5)
        } else {
          rv$metadata <- NULL
        }
      } else {
        rv$metadata <- NULL
      }

    })


    # --- (Keep existing Fetch Metadata observeEvent block) ---
    observeEvent(input$fetchMeta, {
      req(input$identifier)
      req(input$metawoRldPath)
      # Use a fixed email or prompt user? For simplicity, use NULL initially.
      email_addr <- NULL # Or Sys.getenv("MY_EMAIL") etc.

      # Clear previous results except identifier/path
      rv$metadata <- NULL
      rv$assessment <- NULL
      # Don't clear manual fields automatically on fetch? Or maybe we should? Let's clear them.
      updateTextAreaInput(session, "manualTitle", value = "")
      updateTextAreaInput(session, "manualAbstract", value = "")
      updateTextAreaInput(session, "manualAssessmentJson", value="")


      showNotification(paste("Fetching metadata for:", input$identifier), id = "fetchNotify", duration = NULL)
      tryCatch({
        # Fetch regardless of cache (user clicked fetch)
        meta <- df_fetch_metadata(input$identifier, email = email_addr)
        if (!is.null(meta)) {
          rv$metadata <- meta
          updateTextAreaInput(session, "manualTitle", value = meta$title %||% "")
          updateTextAreaInput(session, "manualAbstract", value = meta$abstract %||% "")
          # Save fetched metadata to cache
          .save_to_cache(identifier = input$identifier, data = meta, type = "metadata", metawoRld_path = input$metawoRldPath)
          showNotification("Metadata fetched and cached successfully.", type = "message", duration = 5)
        } else {
          showNotification(paste("Could not fetch metadata for:", input$identifier), type = "warning", duration = 10)
        }
      }, error = function(e) {
        showNotification(paste("Metadata fetch failed:", e$message), type = "error", duration = 10)
      }, finally = {
        removeNotification("fetchNotify")
      })
    })

    # --- (Keep existing Gather Title/Abstract reactives) ---
    gathered_title <- reactive({
      manual <- input$manualTitle
      fetched <- rv$metadata$title %||% ""
      if (!is.null(manual) && nzchar(trimws(manual))) {
        return(trimws(manual))
      } else {
        return(fetched)
      }
    })

    gathered_abstract <- reactive({
      manual <- input$manualAbstract
      fetched <- rv$metadata$abstract %||% ""
      if (!is.null(manual) && nzchar(trimws(manual))) {
        return(trimws(manual))
      } else {
        return(fetched)
      }
    })

    # --- (Keep existing Display Fetched/Manual Info outputs) ---
    output$displayTitle <- renderText({ gathered_title() %||% "N/A" })
    output$displayAbstract <- renderText({ gathered_abstract() %||% "N/A" })


    # --- (Keep existing Generate Assessment Prompt reactive) ---
    assessment_prompt_reactive <- reactive({
      req(rv$project_valid, rv$criteria)
      req(gathered_title(), gathered_abstract())
      .generate_assessment_prompt(
        title = gathered_title(),
        abstract = gathered_abstract(),
        inclusion_criteria = rv$criteria$inclusion,
        exclusion_criteria = rv$criteria$exclusion
      )
    })

    # --- (Keep existing Display Prompt and Copy Button output) ---
    output$promptSection <- renderUI({
      prompt_text <- assessment_prompt_reactive()
      req(prompt_text)
      elements <- list(
        tags$textarea(id = "promptDisplay", readonly = "readonly", rows = 15, style = "width:100%; font-family: monospace;", prompt_text)
      )
      if (copy_available) {
        elements <- append(elements, list(
          rclipboard::rclipButton(
            "copyBtn", "Copy Prompt", clipText = prompt_text,
            icon = icon("copy"), class="btn-sm btn-outline-secondary" ) ))
      } else {
        elements <- append(elements, list(p(em("Install 'rclipboard' package to enable copy button."))))
      }
      return(tagList(elements))
    })

    # --- (Keep existing Run Assessment observeEvent block) ---
    # Modify slightly to update the manual JSON field after running
    observeEvent(input$runAssessment, {
      req(input$identifier)
      req(input$metawoRldPath)
      req(rv$project_valid)

      rv$assessment <- NULL # Clear previous result before running
      updateTextAreaInput(session, "manualAssessmentJson", value="") # Clear manual json too

      withProgress(message = 'Running LLM Assessment...', value = 0, {
        tryCatch({
          incProgress(0.5)
          assessment_res <- df_assess_relevance(
            identifier = input$identifier,
            metawoRld_path = input$metawoRldPath,
            service = input$llmService,
            model = input$llmModel,
            email = NULL
          )
          rv$assessment <- assessment_res # Update reactive value

          incProgress(1)
          showNotification("Assessment complete.", type = "message", duration = 5)
        }, error = function(e) {
          incProgress(1)
          showNotification(paste("Assessment Failed:", e$message), type = "error", duration = NULL)
        })
      })
    })

    # --- Display Assessment Result ---
    # (Keep existing renderText block, it displays rv$assessment)
    output$assessmentResult <- renderText({
      res <- rv$assessment
      if (is.null(res)) {
        return("Assessment not yet run or loaded from cache.")
      } else {
        # Format for display
        json_display <- tryCatch({
          jsonlite::toJSON(res, auto_unbox = TRUE, pretty = TRUE)
        }, error = function(e) {"Error formatting result for display"})
        return(json_display)
        # Or use the nicer formatting from before:
        # paste(...)
      }
    })

    # --- NEW: Update Manual JSON field when rv$assessment changes ---
    observe({
      res <- rv$assessment
      if(is.null(res)){
        # Optionally clear manual field if assessment becomes null? Depends on desired UX.
        # updateTextAreaInput(session, "manualAssessmentJson", value="")
      } else {
        json_to_display <- tryCatch({
          # Remove any extra fields added internally before display/edit
          res_to_edit <- res[c("decision", "score", "rationale")]
          jsonlite::toJSON(res_to_edit, auto_unbox = TRUE, pretty = TRUE)
        }, error = function(e) {"Error formatting result for editing"})
        updateTextAreaInput(session, "manualAssessmentJson", value = json_to_display)
      }
    })


    # --- NEW: Save Manual Assessment ---
    observeEvent(input$saveManualAssessment, {
      req(input$identifier)
      req(input$metawoRldPath)
      req(input$manualAssessmentJson)

      json_string <- input$manualAssessmentJson
      identifier <- input$identifier
      proj_path <- input$metawoRldPath

      if (!nzchar(trimws(json_string))) {
        showNotification("Manual Assessment JSON input is empty.", type = "warning", duration = 5)
        return()
      }

      # Validate JSON structure
      parsed_json <- NULL
      validation_passed <- FALSE
      tryCatch({
        parsed_json <- jsonlite::fromJSON(json_string, simplifyVector = FALSE)
        required_fields <- c("decision", "score", "rationale")
        if (is.list(parsed_json) && all(required_fields %in% names(parsed_json))) {
          validation_passed <- TRUE
        } else {
          showNotification("Manual JSON is missing required fields: 'decision', 'score', 'rationale'.", type = "error", duration = 10)
        }
      }, error = function(e) {
        showNotification(paste("Invalid JSON format:", e$message), type = "error", duration = 10)
      })

      if (validation_passed) {
        # Add timestamp/model info? Maybe indicate it was manual?
        parsed_json$assessment_timestamp <- Sys.time()
        parsed_json$assessment_model <- "manual"
        parsed_json$assessment_service <- "manual"

        # Save to cache (this will overwrite existing cache file)
        save_path <- .save_to_cache(
          identifier = identifier,
          data = parsed_json,
          type = "assessment",
          metawoRld_path = proj_path
        )

        if (!is.null(save_path)) {
          showNotification("Manual assessment saved to cache successfully.", type = "message", duration = 5)
          # Update the main reactive value to reflect the saved manual assessment
          rv$assessment <- parsed_json
        } else {
          showNotification("Failed to save manual assessment to cache.", type = "error", duration = 10)
        }
      }
    })


  } # end server

  # --- Run the App ---
  shinyApp(ui = ui, server = server, options = list(launch.browser = launch.browser, ...))
}
