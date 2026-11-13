#' Interactive extraction app
#'
#' Pick an included study, run the LLM on its full text, review and correct
#' the extracted JSON with live schema validation, save it, and import it into
#' the metawoRld project.
#'
#' @param metawoRld_path Project root.
#' @param chat Optional ellmer chat object. If `NULL`, a `"provider/model"`
#'   string can be entered in the app.
#' @param fulltext_dir Optional folder with full texts named after the
#'   identifiers (see [df_find_fulltexts()]).
#' @param launch.browser Passed to [shiny::runApp()].
#' @return Runs the app.
#' @export
df_shiny_extract <- function(metawoRld_path = ".", chat = NULL, fulltext_dir = NULL, launch.browser = TRUE) {
  rlang::check_installed("shiny", reason = "to run the extraction app.")
  proj <- .check_project(metawoRld_path)
  schema <- metawoRld::get_schema(proj, "extraction")
  fulltexts <- if (!is.null(fulltext_dir)) df_find_fulltexts(fulltext_dir) else character(0)

  ui <- shiny::fluidPage(
    shiny::titlePanel("DataFindR extraction"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 4,
        shiny::radioButtons("filter", "Show", c("Waiting for extraction" = "todo",
                                                "Extracted, not imported" = "review",
                                                "All included" = "all")),
        shiny::selectInput("id", "Study", choices = NULL),
        shiny::textInput("file", "Full text (pdf, txt, html, xml)"),
        if (is.null(chat)) shiny::textInput("model", "LLM (provider/model)", "anthropic/claude-sonnet-4-5"),
        shiny::actionButton("run", "Run LLM extraction", class = "btn-primary"),
        shiny::hr(),
        shiny::actionButton("save", "Save JSON", class = "btn-success"),
        shiny::actionButton("import", "Import into project", class = "btn-warning")
      ),
      shiny::mainPanel(
        width = 8,
        shiny::h4("Validation"),
        shiny::verbatimTextOutput("problems"),
        shiny::h4("Extraction (JSON, editable)"),
        shiny::textAreaInput("json", NULL, rows = 30, width = "100%")
      )
    )
  )

  server <- function(input, output, session) {
    refresh <- shiny::reactiveVal(0)
    ids <- shiny::reactive({
      refresh()
      st <- df_status(proj)
      inc <- st$decision %in% "Include" | st$extracted
      keep <- switch(input$filter,
                     todo = inc & !st$extracted,
                     review = st$extracted & !st$imported,
                     inc)
      st$identifier[keep]
    })
    shiny::observe({
      ch <- ids()
      sel <- shiny::isolate(input$id)
      shiny::updateSelectInput(session, "id", choices = ch, selected = if (!is.null(sel) && sel %in% ch) sel else ch[1])
    })
    shiny::observeEvent(input$id, {
      shiny::req(input$id)
      shiny::updateTextInput(session, "file", value = unname(fulltexts[input$id]) %||% "")
      e <- .cache_get_entry(input$id, "extraction", proj)
      shiny::updateTextAreaInput(session, "json", value = if (is.null(e)) "" else
        as.character(jsonlite::toJSON(e$result, auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null")))
    })

    parsed <- shiny::reactive({
      shiny::req(nzchar(input$json))
      tryCatch(jsonlite::fromJSON(input$json, simplifyVector = FALSE), error = function(e) e)
    })
    output$problems <- shiny::renderText({
      p <- parsed()
      if (inherits(p, "error")) return(paste("Invalid JSON:", conditionMessage(p)))
      errs <- .extraction_problems(p, schema)
      if (!length(errs)) "No problems found." else paste(errs, collapse = "\n")
    })

    shiny::observeEvent(input$run, {
      shiny::req(input$id, nzchar(input$file))
      the_chat <- chat %||% input$model
      shiny::withProgress(message = "Extracting... this can take a few minutes", {
        res <- tryCatch(df_extract(the_chat, input$id, input$file, proj, force = TRUE), error = function(e) e)
      })
      if (inherits(res, "error")) {
        shiny::showNotification(conditionMessage(res), type = "error", duration = NULL)
      } else {
        shiny::updateTextAreaInput(session, "json", value = as.character(
          jsonlite::toJSON(res, auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null")))
      }
    })
    shiny::observeEvent(input$save, {
      p <- parsed()
      if (inherits(p, "error")) return(shiny::showNotification("Fix the JSON first.", type = "error"))
      errs <- suppressWarnings(df_save_extraction(input$id, p, proj))
      shiny::showNotification(if (length(errs)) "Saved (with validation problems)." else "Saved.",
                              type = if (length(errs)) "warning" else "message")
      refresh(refresh() + 1)
    })
    shiny::observeEvent(input$import, {
      r <- tryCatch(df_import_extraction(input$id, proj, overwrite = TRUE), error = function(e) e)
      if (inherits(r, "error")) {
        msg <- if (!is.null(r$validation_errors)) paste(utils::head(r$validation_errors, 5), collapse = "\n") else conditionMessage(r)
        shiny::showNotification(paste("Import failed:", msg), type = "error", duration = NULL)
      } else {
        shiny::showNotification("Imported.")
      }
      refresh(refresh() + 1)
    })
  }

  shiny::runApp(shiny::shinyApp(ui, server), launch.browser = launch.browser)
}
