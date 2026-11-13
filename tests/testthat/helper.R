local_project <- function(env = parent.frame()) {
  path <- file.path(tempfile("df"), "proj")
  suppressMessages(metawoRld::create_metawoRld(
    path, "Test review", "Biomarker X in pregnancy.",
    inclusion_criteria = c("Human pregnancy", "Biomarker X measured"),
    exclusion_criteria = c("Animal study"),
    git_init = FALSE, website = FALSE
  ))
  metawoRld::add_person(path, "ahj", "Anders", roles = c("admin", "screener", "extractor", "verifier", "arbiter"))
  withr::defer(unlink(dirname(path), recursive = TRUE), envir = env)
  path
}

# Fake HTTP: PubMed efetch/esearch and Crossref answered from fixtures.
fake_http <- function(url, query = list(), realm = NULL, rate = 3) {
  if (grepl("efetch", url)) {
    xml <- paste(readLines(test_path("fixtures", "pubmed.xml")), collapse = "\n")
    return(xml)
  }
  if (grepl("esearch", url)) {
    ids <- if (grepl("10.1000/abc.1", query$term, fixed = TRUE)) '["11111111"]'
           else if (grepl("DOI", query$term)) "[]" else '["11111111","22222222"]'
    return(sprintf('{"esearchresult":{"count":"%d","idlist":%s}}', length(jsonlite::fromJSON(ids)), ids))
  }
  if (grepl("crossref", url)) return(paste(readLines(test_path("fixtures", "crossref.json")), collapse = ""))
  stop("unexpected url ", url)
}

# Fake ellmer chat. `responder(contents)` returns the structured result.
mock_chat <- function(responder) {
  self <- new.env()
  self$n_calls <- 0
  self$prompts <- list()
  self$clone <- function() self
  self$set_turns <- function(x) invisible(self)
  self$chat_structured <- function(..., type, convert = TRUE) {
    self$n_calls <- self$n_calls + 1
    self$prompts <- c(self$prompts, list(list(...)))
    responder(list(...))
  }
  self$get_tokens <- function() data.frame(input = 120, output = 30)
  self$get_cost <- function(include = "all") 0.002
  self$get_model <- function() "mock-model"
  self
}

include_all <- function(contents) {
  txt <- paste(unlist(Filter(is.character, contents)), collapse = " ")
  if (grepl("rats", txt)) list(decision = "exclude", score = 0.05, rationale = "Animal study.", exclusion_reason = "Animal study")
  else list(decision = "Include", score = 0.9, rationale = "Meets criteria.", exclusion_reason = NA)
}

good_extraction <- function(contents) {
  list(
    metadata = list(
      study_id = "whatever", title = "LLM title", authors = list("Smith J"), year = 2021L,
      country = list("Norway"), study_design = "Cohort", population = "Pregnant women",
      methods = list(list(method_id = "m1", name = "Serum X by ELISA", unit = "pg/mL")),
      groups = list(list(group_id = "g1", name = "Cases", sample_size = 20L),
                    list(group_id = "g2", name = "Controls", sample_size = 25L))
    ),
    data_points = list(
      list(measurement_id = "dp1", method_id = "m1", group_id = "g1", variable_name = "X",
           sample_size = 20L, statistic_type = "mean_sd", value1 = 5.1, value2 = 1.2, value3 = NULL, source = "Table 2"),
      list(measurement_id = "dp2", method_id = "m1", group_id = "g2", variable_name = "X",
           sample_size = 25L, statistic_type = "mean_sd", value1 = 4.0, value2 = NA, source = "Table 2")
    )
  )
}
