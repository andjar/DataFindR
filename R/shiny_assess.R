#' Interactive screening app
#'
#' Browse studies, edit titles/abstracts, preview the exact prompt, run the
#' LLM for one study, and record or override decisions by hand. Manual
#' decisions are kept by later [df_assess_batch()] runs.
#'
#' @param metawoRld_path Project root.
#' @param chat Optional ellmer chat object. If `NULL`, a `"provider/model"`
#'   string can be entered in the app.
#' @param launch.browser Passed to [shiny::runApp()].
#' @return Runs the app.
#' @export
df_shiny_assess <- function(metawoRld_path = ".", chat = NULL, launch.browser = TRUE) {
  rlang::check_installed("shiny", reason = "to run the screening app.")
  proj <- .check_project(metawoRld_path)
  schema <- metawoRld::get_schema(proj, "screening")
  decisions <- unlist(schema$decision$`_values`)

  ui <- shiny::fluidPage(
    shiny::titlePanel("DataFindR screening"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 4,
        shiny::radioButtons("filter", "Show", c("Not screened" = "todo", "Needs manual review" = "manual",
                                                "All" = "all"), inline = TRUE),
        shiny::selectInput("id", "Study", choices = NULL),
        shiny::fluidRow(
          shiny::column(6, shiny::actionButton("prev", "< Previous", width = "100%")),
          shiny::column(6, shiny::actionButton("nxt", "Next >", width = "100%"))
        ),
        shiny::hr(),
        shiny::textInput("new_id", "Add DOI/PMID"),
        shiny::actionButton("fetch", "Fetch metadata"),
        shiny::hr(),
        if (is.null(chat)) shiny::textInput("model", "LLM (provider/model)", "openai/gpt-4.1-mini"),
        shiny::actionButton("run", "Run LLM assessment", class = "btn-primary"),
        shiny::hr(),
        shiny::h4("Decision"),
        shiny::radioButtons("decision", NULL, decisions),
        shiny::textAreaInput("rationale", "Rationale", rows = 3),
        shiny::textInput("reason", "Exclusion reason"),
        shiny::textInput("reviewer", "Reviewer id (people.yml)", getOption("metawoRld.person", "")),
        shiny::actionButton("save", "Save manual decision", class = "btn-success")
      ),
      shiny::mainPanel(
        width = 8,
        shiny::textAreaInput("title", "Title", rows = 2, width = "100%"),
        shiny::textAreaInput("abstract", "Abstract", rows = 12, width = "100%"),
        shiny::actionButton("save_meta", "Save title/abstract edits"),
        shiny::hr(),
        shiny::h4("Current assessment"),
        shiny::verbatimTextOutput("assessment"),
        shiny::h4("Prompt sent to the LLM"),
        shiny::verbatimTextOutput("prompt")
      )
    )
  )

  server <- function(input, output, session) {
    refresh <- shiny::reactiveVal(0)

    ids <- shiny::reactive({
      refresh()
      st <- df_status(proj)
      st <- st[st$has_metadata, ]
      keep <- switch(input$filter,
                     todo = is.na(st$decision),
                     manual = st$decision %in% "Needs manual review",
                     rep(TRUE, nrow(st)))
      st <- st[keep, ]
      rlang::set_names(st$identifier, paste0(st$identifier, " - ", substr(st$title, 1, 60)))
    })
    shiny::observe({
      choices <- ids()
      sel <- shiny::isolate(input$id)
      shiny::updateSelectInput(session, "id", choices = choices,
                               selected = if (!is.null(sel) && sel %in% choices) sel else choices[1])
    })
    step <- function(d) {
      ch <- unname(ids())
      i <- match(input$id, ch)
      if (!length(ch) || is.na(i)) return()
      shiny::updateSelectInput(session, "id", selected = ch[max(1, min(length(ch), i + d))])
    }
    shiny::observeEvent(input$prev, step(-1))
    shiny::observeEvent(input$nxt, step(1))

    current <- shiny::reactive({
      shiny::req(input$id)
      refresh()
      list(meta = .cache_read(input$id, "metadata", proj),
           assessment = .cache_get_entry(input$id, "assessment", proj))
    })
    shiny::observeEvent(current(), {
      m <- current()$meta
      shiny::updateTextAreaInput(session, "title", value = .or_na(m$title, ""))
      shiny::updateTextAreaInput(session, "abstract", value = .or_na(m$abstract, ""))
      a <- current()$assessment$result
      if (!is.null(a$decision)) shiny::updateRadioButtons(session, "decision", selected = a$decision)
      shiny::updateTextAreaInput(session, "rationale", value = a$rationale %||% "")
      shiny::updateTextInput(session, "reason", value = a$exclusion_reason %||% "")
    })

    edited_meta <- shiny::reactive({
      m <- current()$meta %||% list()
      m$title <- input$title
      m$abstract <- input$abstract
      m
    })
    output$prompt <- shiny::renderText(df_assessment_prompt(edited_meta(), proj))
    output$assessment <- shiny::renderText({
      a <- current()$assessment
      if (is.null(a)) return("Not assessed yet.")
      jsonlite::toJSON(a, auto_unbox = TRUE, pretty = TRUE, null = "null")
    })

    save_meta_if_edited <- function() {
      m <- current()$meta
      if (!identical(m$title, input$title) || !identical(m$abstract, input$abstract)) {
        e <- edited_meta()
        e$source <- "manual edit"
        df_add_metadata(input$id, e, proj)
      }
    }
    shiny::observeEvent(input$save_meta, {
      save_meta_if_edited()
      refresh(refresh() + 1)
      shiny::showNotification("Metadata saved.")
    })

    shiny::observeEvent(input$fetch, {
      shiny::req(nzchar(input$new_id))
      id <- metawoRld::normalize_identifier(input$new_id)
      m <- tryCatch(.get_metadata(id, proj, force = TRUE), error = function(e) e)
      if (inherits(m, "error") || is.null(m[[id]])) {
        shiny::showNotification("No metadata found. You can paste title and abstract and save them.", type = "warning")
        df_add_metadata(id, list(title = "", abstract = ""), proj)
      }
      shiny::updateRadioButtons(session, "filter", selected = "all")
      refresh(refresh() + 1)
      shiny::updateSelectInput(session, "id", selected = id)
    })

    shiny::observeEvent(input$run, {
      shiny::req(input$id)
      save_meta_if_edited()
      the_chat <- chat %||% input$model
      shiny::withProgress(message = "Asking the LLM...", {
        res <- tryCatch(df_assess_batch(the_chat, input$id, proj, mode = "sequential", force = TRUE),
                        error = function(e) e)
      })
      if (inherits(res, "error")) {
        shiny::showNotification(conditionMessage(res), type = "error", duration = NULL)
      } else if (res$status[1] == "failed") {
        shiny::showNotification(res$error[1], type = "error", duration = NULL)
      }
      refresh(refresh() + 1)
    })

    shiny::observeEvent(input$save, {
      shiny::req(input$id)
      df_save_assessment(input$id, input$decision, input$rationale,
                         exclusion_reason = if (nzchar(input$reason)) input$reason else NULL,
                         metawoRld_path = proj,
                         reviewer = if (nzchar(input$reviewer)) input$reviewer else NULL)
      shiny::showNotification("Decision saved.")
      refresh(refresh() + 1)
    })
  }

  shiny::runApp(shiny::shinyApp(ui, server), launch.browser = launch.browser)
}
