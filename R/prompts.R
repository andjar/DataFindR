#' Build the screening prompt for one study
#'
#' Fills the project's `_assessment_prompt.txt` with the project description,
#' criteria and the study's title and abstract. Useful to inspect exactly what
#' is sent to the LLM.
#'
#' @param metadata Study metadata (list with `title` and `abstract`), e.g.
#'   from [df_fetch_metadata()].
#' @param metawoRld_path Project root.
#' @return A single string.
#' @export
df_assessment_prompt <- function(metadata, metawoRld_path = ".") {
  proj <- .check_project(metawoRld_path)
  cfg <- metawoRld::read_config(proj)
  .fill(.read_prompt(proj, "assessment"), list(
    project_name = cfg$project_name %||% "",
    project_description = cfg$project_description %||% "",
    inclusion_criteria = .bullets(cfg$inclusion_criteria),
    exclusion_criteria = .bullets(cfg$exclusion_criteria),
    title = .or_na(metadata$title, "Not available"),
    abstract = .or_na(metadata$abstract, "Not available")
  ))
}

#' Build the extraction prompt for one study
#'
#' @param identifier Study identifier.
#' @param metawoRld_path Project root.
#' @param metadata Optional known bibliographic metadata; by default taken from
#'   the metadata cache.
#' @details The variable names and group categories defined in the project's
#'   `_rules.yml` are included (placeholders `{{known_variables}}` and
#'   `{{group_categories}}`), so the LLM uses the project's vocabulary. Adding
#'   synonyms does not make cached extractions outdated, because that would
#'   trigger costly re-extraction of every study; use `force = TRUE` in
#'   [df_extract_batch()] to redo studies deliberately.
#' @return A single string.
#' @export
df_extraction_prompt <- function(identifier, metawoRld_path = ".", metadata = NULL) {
  proj <- .check_project(metawoRld_path)
  cfg <- metawoRld::read_config(proj)
  id <- metawoRld::normalize_identifier(identifier)
  metadata <- metadata %||% .cache_read(id, "metadata", proj)
  known <- if (is.null(metadata)) "- (none)" else {
    f <- intersect(c("title", "authors", "year", "journal", "doi", "pmid"), names(metadata))
    vals <- vapply(f, function(k) paste(unlist(metadata[[k]]), collapse = ", "), character(1))
    keep <- !is.na(vals) & nzchar(vals) & vals != "NA"
    paste0("- ", f[keep], ": ", vals[keep], collapse = "\n")
  }
  vocab <- metawoRld::rules_for_prompt(proj)
  .fill(.read_prompt(proj, "extraction"), list(
    project_name = cfg$project_name %||% "",
    project_description = cfg$project_description %||% "",
    identifier = id,
    known_metadata = known,
    known_variables = vocab$known_variables,
    group_categories = vocab$group_categories
  ))
}

.read_prompt <- function(proj, type) {
  f <- fs::path(proj, paste0("_", type, "_prompt.txt"))
  if (!fs::file_exists(f)) {
    f <- system.file("templates", paste0("_", type, "_prompt.txt"), package = "metawoRld")
    rlang::warn(paste0("_", type, "_prompt.txt not found in project; using metawoRld's default. ",
                       "Run metawoRld::use_templates() to add it."), .frequency = "once",
                .frequency_id = paste0("prompt-", type))
  }
  paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

.fill <- function(template, values) {
  env <- list2env(lapply(values, function(v) paste(as.character(v), collapse = "\n")), parent = emptyenv())
  tryCatch(
    as.character(glue::glue(template, .envir = env, .open = "{{", .close = "}}", .trim = FALSE)),
    error = function(e) rlang::abort(c("Could not fill prompt template.", "i" = conditionMessage(e),
                                       "i" = paste("Available placeholders:", paste0("{{", names(values), "}}", collapse = ", "))))
  )
}

.bullets <- function(x) if (length(x)) paste0("- ", x, collapse = "\n") else "- (none specified)"

.or_na <- function(x, alt) if (.has_text(x)) x else alt

.assessment_hash <- function(proj, model) {
  cfg <- metawoRld::read_config(proj)
  .hash("assessment", .read_prompt(proj, "assessment"), metawoRld::get_schema(proj, "assessment"),
        cfg$project_name, cfg$project_description, cfg$inclusion_criteria, cfg$exclusion_criteria, model)
}

.extraction_hash <- function(proj, model) {
  cfg <- metawoRld::read_config(proj)
  .hash("extraction", .read_prompt(proj, "extraction"), metawoRld::get_schema(proj, "extraction"),
        cfg$project_name, cfg$project_description, model)
}
