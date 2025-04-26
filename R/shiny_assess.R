#' Launch Shiny App for Assessing Paper Relevance
#'
#' Launches a simple Shiny application to assist with the assessment phase of
#' the DataFindR workflow for a single paper. It allows fetching metadata,
#' viewing the assessment prompt, triggering an LLM assessment via
#' `DataFindR::df_assess_relevance`, and viewing the results.
#'
#' @param launch.browser Logical, passed to `shiny::runApp`. Whether to launch
#'   the app in the default browser.
#' @param ... Additional arguments passed to `shiny::runApp` (e.g., `port`, `host`).
#'
#' @details
#' Before running the app:
#' \itemize{
#'   \item Ensure the `metawoRld` package is installed and accessible.
#'   \item Ensure the target `metawoRld` project directory exists and contains a valid `_metawoRld.yml`.
#'   \item If triggering API calls, ensure the relevant API key (e.g., `OPENAI_API_KEY`) is set in your environment (see `.get_api_key` documentation).
#'   \item The `rclipboard` package is needed for the 'Copy Prompt' button.
#' }
#'
#' @return Does not return a value; runs the Shiny application.
#' @export
#'
#' @import shiny
#' @importFrom glue glue
#' @importFrom rlang inform warn abort is_list `%||%` check_installed is_scalar_character
#' @importFrom yaml read_yaml
#' @importFrom fs path_norm dir_exists file_exists path
#' @importFrom utils packageVersion browseURL # For rclipboard fallback
#' @importFrom tools file_path_sans_ext # Not strictly needed here, but good practice
df_shiny_assess <- function(launch.browser = TRUE, ...) {

  # --- Check suggested packages ---
  rlang::check_installed("shiny", reason = "to run the interactive assessment app.")
  copy_available <- rlang::is_installed("rclipboard")

  # --- UI Definition ---
  ui <- fluidPage(
    title = "DataFindR Paper Assessment",
    if (copy_available) rclipboard::rclipboardSetup(), # Setup for copy button

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
        # Simplification: Use fixed service/model or get from config later
        selectInput("llmService", "LLM Service", choices = c("openai"), selected = "openai"),
        textInput("llmModel", "LLM Model", value = "gpt-3.5-turbo")
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
        h4("Assessment Result:"),
        verbatimTextOutput("assessmentResult", placeholder = TRUE)
      )
    )
  )

  # --- Server Logic ---
  server <- function(input, output, session) {

    # --- Reactive Values ---
    rv <- reactiveValues(
      metadata = NULL,      # Stores result from df_fetch_metadata
      assessment = NULL,    # Stores result from df_assess_relevance
      project_valid = FALSE,
      criteria = NULL
    )

    # --- Validate Project Path and Load Criteria ---
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
            showNotification("Project config found, but no inclusion/exclusion criteria defined.", type = "warning", duration=10)
            rv$project_valid <- FALSE
            rv$criteria <- NULL
          } else {
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
        # Use removeNotification to avoid buildup if user types path character by character
        removeNotification("proj_msg")
        rv$project_valid <- FALSE
        rv$criteria <- NULL
        # Optionally show a persistent warning if path seems invalid?
      }
    })


    # --- Fetch Metadata ---
    observeEvent(input$fetchMeta, {
      req(input$identifier)
      # Use a fixed email or prompt user? For simplicity, use NULL initially.
      email_addr <- NULL # Or Sys.getenv("MY_EMAIL") etc.

      # Clear previous results
      rv$metadata <- NULL
      rv$assessment <- NULL
      updateTextAreaInput(session, "manualTitle", value = "")
      updateTextAreaInput(session, "manualAbstract", value = "")

      showNotification(paste("Fetching metadata for:", input$identifier), id = "fetchNotify", duration = NULL)
      tryCatch({
        meta <- df_fetch_metadata(input$identifier, email = email_addr)
        if (!is.null(meta)) {
          rv$metadata <- meta
          updateTextAreaInput(session, "manualTitle", value = meta$title %||% "")
          updateTextAreaInput(session, "manualAbstract", value = meta$abstract %||% "")
          showNotification("Metadata fetched successfully.", type = "message", duration = 5)
        } else {
          showNotification(paste("Could not fetch metadata for:", input$identifier), type = "warning", duration = 10)
        }
      }, error = function(e) {
        showNotification(paste("Metadata fetch failed:", e$message), type = "error", duration = 10)
      }, finally = {
        removeNotification("fetchNotify")
      })
    })

    # --- Gather Title/Abstract (prefer manual edits) ---
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

    # --- Display Fetched/Manual Info ---
    output$displayTitle <- renderText({ gathered_title() %||% "N/A" })
    output$displayAbstract <- renderText({ gathered_abstract() %||% "N/A" })

    # --- Generate Assessment Prompt ---
    assessment_prompt_reactive <- reactive({
      # Requires valid project path and criteria loaded
      req(rv$project_valid, rv$criteria)
      # Requires title/abstract (even if empty)
      req(gathered_title(), gathered_abstract())

      # Use internal prompt generator
      .generate_assessment_prompt(
        title = gathered_title(),
        abstract = gathered_abstract(),
        inclusion_criteria = rv$criteria$inclusion,
        exclusion_criteria = rv$criteria$exclusion
      )
    })

    # --- Display Prompt and Copy Button ---
    output$promptSection <- renderUI({
      prompt_text <- assessment_prompt_reactive()
      req(prompt_text)

      elements <- list(
        tags$textarea(id = "promptDisplay", readonly = "readonly", rows = 15, style = "width:100%; font-family: monospace;", prompt_text)
      )

      if (copy_available) {
        elements <- append(elements, list(
          rclipboard::rclipButton(
            "copyBtn",
            "Copy Prompt",
            clipText = prompt_text, # Use the reactive value directly
            icon = icon("copy"),
            class="btn-sm btn-outline-secondary"
          )
        ))
      } else {
        elements <- append(elements, list(p(em("Install 'rclipboard' package to enable copy button."))))
      }
      return(tagList(elements))
    })


    # --- Run Assessment ---
    observeEvent(input$runAssessment, {
      req(input$identifier)
      req(input$metawoRldPath)
      req(rv$project_valid) # Ensure project is valid before trying

      rv$assessment <- NULL # Clear previous result

      # Use withProgress for better feedback
      withProgress(message = 'Running LLM Assessment...', value = 0, {
        tryCatch({
          incProgress(0.5) # Increment progress
          # Call DataFindR's main assessment function
          # It handles caching internally (both metadata and assessment)
          assessment_res <- df_assess_relevance(
            identifier = input$identifier,
            metawoRld_path = input$metawoRldPath,
            service = input$llmService,
            model = input$llmModel,
            # Add email/key args if needed/configured
            email = NULL # Or get from secure config
          )
          rv$assessment <- assessment_res
          incProgress(1) # Complete progress
          showNotification("Assessment complete.", type = "message", duration = 5)
        }, error = function(e) {
          incProgress(1) # Complete progress even on error
          showNotification(paste("Assessment Failed:", e$message), type = "error", duration = NULL) # Keep error visible
        }) # end tryCatch
      }) # end withProgress
    })

    # --- Display Assessment Result ---
    output$assessmentResult <- renderText({
      res <- rv$assessment
      if (is.null(res)) {
        return("Assessment not yet run or failed.")
      } else {
        # Nicer formatting
        paste(
          glue::glue("Decision:   {res$decision %||% 'N/A'}"),
          glue::glue("Score:      {sprintf('%.2f', res$score %||% NA)}"),
          glue::glue("Rationale:  {res$rationale %||% 'N/A'}"),
          glue::glue("Timestamp:  {format(res$assessment_timestamp %||% NA)}"),
          glue::glue("Model:      {res$assessment_model %||% 'N/A'}"),
          sep = "\n"
        )
      }
    })

  } # end server

  # --- Run the App ---
  shinyApp(ui = ui, server = server, options = list(launch.browser = launch.browser, ...))
}
