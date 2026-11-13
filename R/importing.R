#' Import a cached extraction into the metawoRld project
#'
#' Takes the cached extraction, optionally replaces bibliographic fields with
#' the fetched metadata (which is usually more reliable than the LLM's
#' reading), and stores the study with [metawoRld::add_study_data()], which
#' validates it against the schema. The study is stored under its DOI/PMID,
#' so cache and project stay linked.
#'
#' @param identifier Study identifier.
#' @param metawoRld_path Project root.
#' @param overwrite Replace the study if it was imported before?
#' @param merge_metadata Use fetched title, authors, year, journal, DOI, PMID
#'   and abstract when available? Defaults to `TRUE`.
#' @param canonical_names Replace variable names that are synonyms in the
#'   project's `_rules.yml` with the canonical name (e.g. "IL6" -> "IL-6")?
#'   The name as reported is kept in the data point's `notes`. Defaults to
#'   `TRUE`.
#' @return The study folder, invisibly.
#' @export
df_import_extraction <- function(identifier, metawoRld_path = ".", overwrite = FALSE, merge_metadata = TRUE,
                                 canonical_names = TRUE) {
  proj <- .check_project(metawoRld_path)
  id <- metawoRld::normalize_identifier(identifier)
  entry <- .cache_get_entry(id, "extraction", proj)
  if (is.null(entry)) rlang::abort(paste0("No cached extraction for '", id, "'. Run df_extract_batch() first."))
  meta <- entry$result$metadata %||% list()
  data <- entry$result$data_points %||% list()

  if (merge_metadata) {
    fetched <- .cache_read(id, "metadata", proj)
    for (f in intersect(c("title", "authors", "year", "journal", "doi", "pmid", "abstract"), names(fetched))) {
      v <- fetched[[f]]
      if (length(v) && !all(is.na(unlist(v))) && any(nzchar(unlist(v)))) meta[[f]] <- if (f == "authors") as.list(unlist(v)) else v
    }
    if (!is.null(meta$pmid)) meta$pmid <- as.character(meta$pmid)
  }

  if (canonical_names && length(data)) data <- .canonicalize_variables(data, metawoRld::read_rules(proj))

  metawoRld::add_study_data(proj, id, meta, data, overwrite = overwrite)
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

#' Import several cached extractions
#'
#' @param identifiers Identifiers to import. Defaults to every cached
#'   extraction that is not yet in the project.
#' @param metawoRld_path Project root.
#' @param overwrite,merge_metadata,canonical_names See [df_import_extraction()].
#' @return A tibble with `identifier`, `status` (`"imported"` or `"failed"`),
#'   `path` and `error`. Validation problems are listed in `error`.
#' @export
df_import_batch <- function(identifiers = NULL, metawoRld_path = ".", overwrite = FALSE, merge_metadata = TRUE,
                            canonical_names = TRUE) {
  proj <- .check_project(metawoRld_path)
  if (is.null(identifiers)) {
    identifiers <- setdiff(.cached_ids(proj, "extraction"), metawoRld::list_studies(proj))
  }
  ids <- unique(metawoRld::normalize_identifier(identifiers))
  out <- dplyr::tibble(identifier = ids, status = NA_character_, path = NA_character_, error = NA_character_)
  for (i in seq_along(ids)) {
    r <- tryCatch(df_import_extraction(ids[i], proj, overwrite = overwrite, merge_metadata = merge_metadata,
                                       canonical_names = canonical_names),
                  error = function(e) e)
    if (inherits(r, "error")) {
      out$status[i] <- "failed"
      out$error[i] <- if (!is.null(r$validation_errors)) paste(r$validation_errors, collapse = "; ") else conditionMessage(r)
    } else {
      out$status[i] <- "imported"
      out$path[i] <- as.character(r)
    }
  }
  rlang::inform(sprintf("Imported %d of %d stud%s.", sum(out$status == "imported"), nrow(out), if (nrow(out) == 1) "y" else "ies"))
  out
}
