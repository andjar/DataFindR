#' Launch Shiny App for Extracting Study Data
#'
#' Launches a Shiny application to manage the data extraction phase. It helps
#' identify studies pending extraction, generate prompts, optionally run LLM
#' extraction, allow manual JSON input/editing, and save results to the cache.
#'
#' @param launch.browser Logical, passed to `shiny::runApp`. Whether to launch
#'   the app in the default browser.
#' @param ... Additional arguments passed to `shiny::runApp` (e.g., `port`, `host`).
#'
#' @details
#' Before running the app:
#' \itemize{
#'   \item Ensure `metawoRld` and `DataFindR` packages are installed and accessible.
#'   \item Ensure the target `metawoRld` project directory exists and contains a valid `_metawoRld.yml`.
#'   \item Ensure assessment results exist in the project's cache (`.metawoRld_cache/datafindr/assessment/`).
#'   \item If triggering API calls, ensure the relevant API key (e.g., `OPENAI_API_KEY`) is set in your environment.
#'   \item For LLM extraction, you currently need to provide a path to a **plain text file** containing the full paper content. PDF processing within the app is not yet implemented.
#'   \item The `rclipboard` package is needed for the 'Copy Prompt' button.
#' }
#'
#' @return Does not return a value; runs the Shiny application.
#' @export
#'
#' @import shiny
#' @importFrom glue glue
#' @importFrom rlang inform warn abort is_list `%||%` check_installed is_scalar_character is_null
#' @importFrom yaml read_yaml
#' @importFrom fs path_norm dir_exists file_exists path path_ext file_size
#' @importFrom utils packageVersion browseURL askYesNo
#' @importFrom jsonlite fromJSON toJSON validate
#' @importFrom tools file_path_sans_ext
#' @importFrom purrr compact
#' @importFrom metawoRld .desanitize_id .sanitize_id
df_shiny_extract <- function(launch.browser = TRUE, ...) {

  # --- Check suggested packages ---
  rlang::check_installed("shiny", reason = "to run the interactive extraction app.")
  copy_available <- rlang::is_installed("rclipboard")

  # --- UI Definition ---
  ui <- fluidPage(
    title = "DataFindR Study Extraction",
    if (copy_available) rclipboard::rclipboardSetup(),

    fluidRow( # Use fluidRow for better control
      column(4, # Sidebar Column
             wellPanel( # Use wellPanel for visual grouping
               h4("1. Project & Study Selection"),
               textInput("metawoRldPathExtract", "metawoRld Project Path:", value = ".", placeholder = "Path to project root"),
               actionButton("refreshPending", "Refresh Pending List", icon = icon("sync")),
               selectInput("pendingStudySelect", "Study Pending Extraction:", choices = c(""), selected = ""),
               hr(),
               h4("2. Paper Full Text Input"),
               textInput("paperPath", "Path to Full Text (.txt file):", placeholder = "/path/to/paper.txt"),
               p(em("Provide path to a plain text file. PDF processing not yet supported here.")),
               hr(),
               h4("3. LLM Extraction (Optional)"),
               actionButton("runExtraction", "Run LLM Extraction", icon = icon("robot"), class = "btn-primary"),
               p(em("Uses paper text & generated prompt. Requires API key.")),
               selectInput("llmServiceExtract", "LLM Service", choices = c("openai", "google"), selected = "google"),
               textInput("llmModelExtract", "LLM Model", value = "gemini-2.5-flash-preview-04-17") # Use a capable model
             )
      ), # End Sidebar Column

      column(8, # Main Panel Column
             tabsetPanel(
               id = "mainTabs",
               tabPanel("Prompt & Extraction",
                        h4("Identifier:"),
                        verbatimTextOutput("displayIdentifier", placeholder = TRUE),
                        h4("Generated Extraction Prompt:"),
                        uiOutput("extractionPromptSection"), # For copy button etc.
                        hr(),
                        h4("Extraction JSON Result / Manual Input:"),
                        textAreaInput("extractionJsonInput", label = NULL, rows = 20, width="100%",
                                      placeholder = 'LLM result appears here, or paste/edit manually...'),
                        actionButton("saveExtractionJson", "Save JSON to Extraction Cache", icon = icon("save"), class = "btn-success")

               ),
               tabPanel("Metadata Preview",
                        h4("Cached Metadata Preview (if available)"),
                        verbatimTextOutput("metadataPreview", placeholder = TRUE)
               )
             ) # End tabsetPanel
      ) # End Main Panel Column
    ) # End fluidRow
  ) # End fluidPage

  # --- Server Logic ---
  server <- function(input, output, session) {

    # --- Reactive Values ---
    rv <- reactiveValues(
      pending_list = character(0),
      selected_id = NULL,
      project_valid = FALSE,
      fetched_metadata = NULL, # Metadata from metadata cache for selected study
      generated_prompt = NULL, # Generated extraction prompt string
      extraction_result_json = "" # Stores the JSON STRING (from LLM or manual edit)
    )

    # --- Helper: Update Pending List ---
    update_pending_list <- function(show_notification = FALSE) {
      req(input$metawoRldPathExtract)
      proj_path <- fs::path_norm(input$metawoRldPathExtract)
      if (!rv$project_valid) return(NULL) # Ensure project is valid

      current_selection <- input$pendingStudySelect

      pending <- tryCatch({
        # Decision threshold could be configurable later
        .find_pending_extraction_studies(proj_path, decision_threshold = c("Include"))
      }, error = function(e) {
        showNotification(paste("Error finding pending studies:", e$message), type="error")
        character(0)
      })

      rv$pending_list <- pending

      # Preserve selection if possible
      if(current_selection %in% pending){
        updateSelectInput(session, "pendingStudySelect", choices = pending, selected = current_selection)
      } else {
        updateSelectInput(session, "pendingStudySelect", choices = pending, selected = "")
      }

      if(show_notification){
        showNotification(paste(length(pending), "studies pending extraction found."), type="message", duration = 5)
      }
    }

    # --- Validate Project Path ---
    observe({
      req(input$metawoRldPathExtract)
      proj_path <- fs::path_norm(input$metawoRldPathExtract)
      config_path <- fs::path(proj_path, "_metawoRld.yml")

      if (fs::dir_exists(proj_path) && fs::file_exists(config_path)) {
        # Basic check, schema validity checked when generating prompt
        rv$project_valid <- TRUE
        removeNotification("proj_extract_warn")
        showNotification("metawoRld project found.", type="message", duration = 5, id="proj_extract_msg")
        update_pending_list() # Update list once project is valid
      } else {
        rv$project_valid <- FALSE
        rv$pending_list <- character(0)
        updateSelectInput(session, "pendingStudySelect", choices = "", selected = "")
        removeNotification("proj_extract_msg")
        showNotification("Valid metawoRld project path not found.", type="warning", duration = 10, id="proj_extract_warn")
      }
    })

    # --- Refresh Pending List Button ---
    observeEvent(input$refreshPending, {
      update_pending_list(show_notification = TRUE)
    })

    # --- Actions on Study Selection Change ---
    observe({
      selected_id <- input$pendingStudySelect
      rv$selected_id <- if(is.null(selected_id) || selected_id == "") NULL else selected_id

      # Clear outputs when selection changes
      rv$fetched_metadata <- NULL
      rv$generated_prompt <- NULL
      rv$extraction_result_json <- ""
      updateTextAreaInput(session, "extractionJsonInput", value = "")
      updateTextInput(session, "paperPath", value = "") # Clear paper path too


      if (!is.null(rv$selected_id)) {
        # 1. Try to load metadata from cache
        meta <- .check_cache(rv$selected_id, type = "metadata", metawoRld_path = input$metawoRldPathExtract)
        if(is.list(meta)) {
          rv$fetched_metadata <- meta
        } else {
          rv$fetched_metadata <- NULL
          showNotification("Metadata not found in cache for selected study. Prompt generation might be less focused.", type="warning", duration = 8)
        }

        # 2. Generate extraction prompt
        if(rv$project_valid) {
          prompt_text <- tryCatch({
            df_generate_extraction_prompt(
              metawoRld_path = input$metawoRldPathExtract,
              identifier = rv$selected_id,
              fetched_metadata = rv$fetched_metadata # Pass metadata if found
            )
          }, error = function(e) {
            showNotification(paste("Error generating prompt:", e$message), type="error")
            NULL
          })
          rv$generated_prompt <- prompt_text
        } else {
          rv$generated_prompt <- NULL
        }
      }
    })

    # --- Display Identifier ---
    output$displayIdentifier <- renderText({ rv$selected_id %||% "N/A" })

    # --- Display Metadata Preview ---
    output$metadataPreview <- renderText({
      meta <- rv$fetched_metadata
      if(is.null(meta)) return("No metadata loaded from cache.")
      # Simple preview
      paste(
        glue::glue("Title: {meta$title %||% 'N/A'}"),
        glue::glue("Authors: {paste(unlist(meta$authors %||% 'N/A'), collapse=', ')}"),
        glue::glue("Year: {meta$year %||% 'N/A'}"),
        glue::glue("Journal: {meta$journal %||% 'N/A'}"),
        # Add abstract preview if useful, might be long
        # glue::glue("Abstract: {substr(meta$abstract %||% 'N/A', 1, 200)}..."),
        sep="\n"
      )
    })

    # --- Display Prompt and Copy Button ---
    output$extractionPromptSection <- renderUI({
      prompt_text <- rv$generated_prompt
      req(prompt_text)
      elements <- list(
        tags$textarea(id = "extractionPromptDisplay", readonly = "readonly", rows = 15, style = "width:100%; font-family: monospace;", prompt_text)
      )
      if (copy_available) {
        elements <- append(elements, list(
          rclipboard::rclipButton(
            "copyPromptBtn", "Copy Prompt", clipText = prompt_text,
            icon = icon("copy"), class="btn-sm btn-outline-secondary" ) ))
      } else {
        elements <- append(elements, list(p(em("Install 'rclipboard' package to enable copy button."))))
      }
      return(tagList(elements))
    })

    # --- Run LLM Extraction ---
    observeEvent(input$runExtraction, {
      req(rv$selected_id, rv$generated_prompt, input$paperPath)
      req(rv$project_valid)

      paper_file_path <- fs::path_norm(input$paperPath)

      if(!fs::file_exists(paper_file_path)) {
        showNotification("Paper text file not found at specified path.", type="error")
        return()
      }
      if(tolower(fs::path_ext(paper_file_path)) != "txt") {
        showNotification("Please provide a path to a .txt file.", type="warning")
        # Later, integrate PDF handling here
        return()
      }

      # Read paper content
      paper_content <- tryCatch({
        paste(readLines(paper_file_path, warn = FALSE), collapse = "\n")
      }, error = function(e){
        showNotification(paste("Error reading paper file:", e$message), type="error")
        return(NULL)
      })
      if(is.null(paper_content)) return()

      if(!nzchar(trimws(paper_content))) {
        showNotification("Paper text file appears to be empty.", type="warning")
        return()
      }

      # Clear previous JSON result before running
      rv$extraction_result_json <- ""
      updateTextAreaInput(session, "extractionJsonInput", value = "")

      llm_args <- rlang::list2(
        prompt = rv$generated_prompt,
        model = input$llmModelExtract,
        response_format = "json_object", # CRUCIAL for extraction
        instructions = "You are a biomedical data extraction assistant. Respond ONLY with valid JSON.",
        # Potentially pass other args like temperature if needed
        temperature = 0.1, # Low temp for extraction
        max_tokens = 100000
      )

      withProgress(message = 'Running LLM Extraction...', value = 0, {
        tryCatch({
          incProgress(0.5)
          llm_response_body <- if (tolower(input$llmServiceExtract) == "openai") {
            .call_llm_openai(!!!llm_args) # Use internal function
          } else if (tolower(input$llmServiceExtract) == "google") {
            inject(.call_llm_google(!!!llm_args)) # Use internal function
          } else {
            stop(glue::glue("LLM service '{input$llmServiceExtract}' not supported."))
          }

          # Extract content string
          llm_json_string <- .get_llm_content_gemini(llm_response_body) %||% ""

          if (llm_json_string == "") {
            stop("LLM returned empty content.")
          }

          # Basic validation: is it valid JSON?
          if(!jsonlite::validate(llm_json_string)) {
            stop("LLM response was not valid JSON.")
          }

          # Update reactive value and UI text area
          rv$extraction_result_json <- llm_json_string
          updateTextAreaInput(session, "extractionJsonInput", value = llm_json_string)

          incProgress(1)
          showNotification("LLM Extraction complete. Review JSON below.", type = "message", duration = 8)

        }, error = function(e) {
          incProgress(1)
          showNotification(paste("LLM Extraction Failed:", e$message), type = "error", duration = NULL)
          rv$extraction_result_json <- "" # Clear on error
          updateTextAreaInput(session, "extractionJsonInput", value = "")
        }) # end tryCatch
      }) # end withProgress
    })


    # --- Save Final Extraction JSON ---
    observeEvent(input$saveExtractionJson, {
      req(rv$selected_id)
      req(input$metawoRldPathExtract)
      req(input$extractionJsonInput) # Require something in the text box

      identifier <- rv$selected_id
      proj_path <- input$metawoRldPathExtract
      json_string <- input$extractionJsonInput

      if (!nzchar(trimws(json_string))) {
        showNotification("Extraction JSON input is empty. Nothing to save.", type = "warning")
        return()
      }

      # Validate JSON string before parsing and saving
      if(!jsonlite::validate(json_string)) {
        showNotification("The text entered is not valid JSON. Cannot save.", type="error")
        return()
      }

      # Parse the JSON (final step before saving)
      parsed_data <- tryCatch({
        jsonlite::fromJSON(json_string, simplifyVector = FALSE)
      }, error = function(e) {
        showNotification(paste("Error parsing JSON:", e$message), type="error")
        return(NULL)
      })
      if(is.null(parsed_data)) return()

      # Basic structure check
      if (!is.list(parsed_data) || !"metadata" %in% names(parsed_data) || !"data_points" %in% names(parsed_data)) {
        showNotification("Parsed JSON is missing required top-level keys: 'metadata' and 'data_points'. Cannot save.", type="error")
        return()
      }

      # Add saving metadata (optional but good practice)
      parsed_data$extraction_timestamp <- Sys.time()
      # Potentially record model used if it came from LLM, or mark as manual/edited
      # if(rv$extraction_result_json == json_string) { # Check if it's unchanged from LLM
      #parsed_data$extraction_model <- input$llmModelExtract # Record model only if directly from LLM?
      # } else {
      parsed_data$extraction_source <- "manual_or_edited"
      # }

      # Confirm overwrite? Cache save function will overwrite by default.
      # Add confirmation if needed.

      # Save the PARSED data object to cache
      save_path <- .save_to_cache(
        identifier = identifier,
        data = parsed_data, # Save the parsed list object
        type = "extraction",
        metawoRld_path = proj_path
      )

      if (!is.null(save_path)) {
        showNotification("Extraction JSON saved to cache successfully.", type = "message")
        # Refresh pending list as this study should now be removed
        update_pending_list()
        # Optionally clear fields or move to next pending?
        # updateSelectInput(session, "pendingStudySelect", selected="") # Clear selection
      } else {
        showNotification("Failed to save extraction JSON to cache.", type = "error")
      }
    })

  } # end server

  # --- Run the App ---
  shinyApp(ui = ui, server = server, options = list(launch.browser = launch.browser, ...))
}
