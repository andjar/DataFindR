#' Submit a cached extraction to the metawoRld project
#'
#' Takes the cached extraction, optionally replaces bibliographic fields with
#' the fetched metadata (which is usually more reliable than the LLM's
#' reading), and submits it with [metawoRld::submit_study()]. The submission
#' records DataFindR as the tool, the LLM model and the hash of the full text
#' the values were read from. It is validated on submission and waits in
#' `submissions/` until a person accepts it ([metawoRld::accept_submission()])
#' or reconciles it with another extraction ([metawoRld::reconcile()]).
#'
#' @param identifier Study identifier (DOI or PMID of the report).
#' @param metawoRld_path Project root.
#' @param study_id Study id to submit under. Defaults to the study the report
#'   already belongs to, else [metawoRld::suggest_study_id()] from the first
#'   author and year.
#' @param merge_metadata Use fetched title, authors, year, journal, DOI, PMID
#'   and abstract when available? Defaults to `TRUE`.
#' @param canonical_names Replace variable names that are synonyms in the
#'   project's `_rules.yml` with the canonical name (e.g. "IL6" -> "IL-6")?
#'   The name as reported is kept in the data point's `notes`. Defaults to
#'   `TRUE`.
#' @param resubmit Submit again even if this cached extraction was submitted
#'   before? Defaults to `FALSE`.
#' @return A list with `study_id` and `extraction_id`, invisibly.
#' @export
df_import_extraction <- function(identifier, metawoRld_path = ".", study_id = NULL, merge_metadata = TRUE,
                                 canonical_names = TRUE, resubmit = FALSE) {
  proj <- .check_project(metawoRld_path)
  id <- metawoRld::normalize_identifier(identifier)
  entry <- .cache_get_entry(id, "extraction", proj)
  if (is.null(entry)) rlang::abort(paste0("No cached extraction for '", id, "'. Run df_extract_batch() first."))
  if (!is.null(entry$submission) && !resubmit) {
    rlang::abort(paste0("This extraction of '", id, "' was already submitted as ", entry$submission$study_id, "/",
                        entry$submission$extraction_id, ". Use `resubmit = TRUE` to submit it again."))
  }
  meta <- entry$result$metadata %||% list()
  data <- entry$result$data_points %||% list()

  if (merge_metadata) {
    fetched <- .cache_read(id, "metadata", proj)
    for (f in intersect(c("title", "authors", "year", "journal", "doi", "pmid", "abstract"), names(fetched))) {
      v <- fetched[[f]]
      if (length(v) && !all(is.na(unlist(v))) && any(nzchar(unlist(v)))) meta[[f]] <- if (f == "authors") as.list(unlist(v)) else v
    }
  }
  if (!is.null(meta$pmid)) meta$pmid <- as.character(meta$pmid)
  if (metawoRld::identifier_type(id) == "doi" && is.null(meta$doi)) meta$doi <- id
  if (metawoRld::identifier_type(id) == "pmid" && is.null(meta$pmid)) meta$pmid <- id
  if (canonical_names && length(data)) data <- .canonicalize_variables(data, metawoRld::read_rules(proj))

  reports <- list(Filter(function(v) length(v) == 1 && !is.na(v) && nzchar(v),
                         list(doi = as.character(meta$doi %||% ""), pmid = as.character(meta$pmid %||% ""), role = "primary")))
  study_id <- study_id %||% .study_for_report(.report_index(proj), meta$doi, meta$pmid)
  if (is.na(study_id)) study_id <- metawoRld::suggest_study_id(meta$authors, meta$year, proj)

  .ensure_tool(proj)
  manual <- identical(entry$provenance$source, "manual")
  person <- if (manual) .person(entry$provenance$reviewer) %||% .tool_id() else .tool_id()
  model <- entry$provenance$model
  if (manual || is.null(model) || is.na(model)) model <- NULL
  ext_id <- metawoRld::submit_study(
    proj, study_id, meta, data, person = person, channel = "datafindr", tool = .tool_string(),
    model = model, source_sha256 = entry$provenance$source_sha256, reports = reports
  )
  entry$submission <- list(study_id = study_id, extraction_id = ext_id, submitted = .now())
  .cache_write(id, entry, "extraction", proj)
  invisible(list(study_id = study_id, extraction_id = ext_id))
}

.canonicalize_variables <- function(data, rules) {
  if (!length(rules$variables)) return(data)
  lapply(data, function(dp) {
    reported <- dp$variable_name
    if (is.null(reported) || is.na(reported)) return(dp)
    canon <- metawoRld::canonical_variable(reported, rules)
    if (!is.na(canon) && !identical(canon, reported)) {
      dp$variable_name <- canon
      note <- paste0("Reported as '", reported, "'.")
      dp$notes <- if (is.null(dp$notes) || is.na(dp$notes) || !nzchar(dp$notes)) note else paste(dp$notes, note)
    }
    dp
  })
}

#' Submit several cached extractions
#'
#' @param identifiers Identifiers to submit. Defaults to every cached
#'   extraction that has not been submitted yet.
#' @param metawoRld_path Project root.
#' @param merge_metadata,canonical_names,resubmit See [df_import_extraction()].
#' @return A tibble with `identifier`, `status` (`"submitted"` or
#'   `"failed"`), `study_id`, `extraction_id` and `error`. Validation
#'   problems are listed in `error`.
#' @export
df_import_batch <- function(identifiers = NULL, metawoRld_path = ".", merge_metadata = TRUE,
                            canonical_names = TRUE, resubmit = FALSE) {
  proj <- .check_project(metawoRld_path)
  if (is.null(identifiers)) {
    identifiers <- Filter(function(id) is.null(.cache_get_entry(id, "extraction", proj)$submission),
                          .cached_ids(proj, "extraction"))
  }
  ids <- unique(metawoRld::normalize_identifier(unlist(identifiers)))
  out <- dplyr::tibble(identifier = ids, status = NA_character_, study_id = NA_character_,
                       extraction_id = NA_character_, error = NA_character_)
  for (i in seq_along(ids)) {
    r <- tryCatch(suppressMessages(df_import_extraction(ids[i], proj, merge_metadata = merge_metadata,
                                                        canonical_names = canonical_names, resubmit = resubmit)),
                  error = function(e) e)
    if (inherits(r, "error")) {
      out$status[i] <- "failed"
      out$error[i] <- if (!is.null(r$validation_errors)) paste(r$validation_errors, collapse = "; ") else conditionMessage(r)
    } else {
      out$status[i] <- "submitted"
      out$study_id[i] <- r$study_id
      out$extraction_id[i] <- r$extraction_id
    }
  }
  rlang::inform(sprintf("Submitted %d of %d stud%s; accept them with metawoRld::accept_submission().",
                        sum(out$status == "submitted"), nrow(out), if (nrow(out) == 1) "y" else "ies"))
  out
}

.now <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
