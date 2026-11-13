# The cache lives inside the metawoRld project so it can be version-controlled
# and shared: <project>/.metawoRld_cache/datafindr/<type>/<id>_<type>.json
#
# Assessment and extraction entries are stored as
#   { "result": {...}, "provenance": { "source", "model", "hash", "timestamp",
#                                      "input_tokens", "output_tokens", "cost",
#                                      "validation_errors" } }
# `hash` fingerprints the prompt template, schema, criteria and model used, so
# results produced with an outdated setup can be detected (see df_status()).
# Metadata entries are stored as a flat list.

.cache_types <- c("metadata", "assessment", "extraction")

.cache_dir <- function(proj, type) fs::path(proj, ".metawoRld_cache", "datafindr", type)

.cache_file <- function(id, type, proj) {
  id <- metawoRld::normalize_identifier(id)
  fs::path(.cache_dir(proj, type), paste0(metawoRld::sanitize_id(id), "_", type, ".json"))
}

.cache_read <- function(id, type, proj) {
  f <- .cache_file(id, type, proj)
  if (!fs::file_exists(f)) return(NULL)
  tryCatch(jsonlite::read_json(f, simplifyVector = FALSE), error = function(e) {
    rlang::warn(paste0("Ignoring unreadable cache file ", f, ": ", conditionMessage(e)))
    NULL
  })
}

.cache_write <- function(id, data, type, proj) {
  f <- .cache_file(id, type, proj)
  fs::dir_create(fs::path_dir(f))
  json <- jsonlite::toJSON(data, auto_unbox = TRUE, pretty = TRUE, null = "null",
                           na = "null", digits = NA, POSIXt = "ISO8601", force = TRUE)
  tmp <- paste0(f, ".tmp")
  writeLines(json, tmp, useBytes = TRUE)
  fs::file_move(tmp, f)
  invisible(f)
}

# Wrap a result with provenance.
.cache_entry <- function(result, source, model = NA, hash = NA, usage = list(), errors = character(0)) {
  list(
    result = result,
    provenance = c(
      list(source = source, model = model, hash = hash,
           timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
           validation_errors = as.list(errors)),
      usage
    )
  )
}

# Read an assessment/extraction entry; older (flat) cache files are wrapped.
.cache_get_entry <- function(id, type, proj) {
  x <- .cache_read(id, type, proj)
  if (is.null(x)) return(NULL)
  if (is.null(x$result) || is.null(x$provenance)) {
    x <- list(result = x, provenance = list(source = "legacy", hash = NA))
  }
  x
}

# Is a cached entry usable for the current setup?
.entry_current <- function(entry, hash) {
  src <- entry$provenance$source %||% "legacy"
  if (src %in% c("manual", "legacy")) return(TRUE)
  identical(entry$provenance$hash, hash)
}

.hash <- function(...) digest::digest(list(...), algo = "sha1")

.check_project <- function(path) {
  if (!rlang::is_scalar_character(path) || !nzchar(path)) rlang::abort("`metawoRld_path` must be a single string.")
  proj <- fs::path_norm(path)
  if (!fs::file_exists(fs::path(proj, "_metawoRld.yml"))) {
    rlang::abort(paste0("Not a metawoRld project (no _metawoRld.yml): ", proj))
  }
  proj
}

.cached_ids <- function(proj, type) {
  dir <- .cache_dir(proj, type)
  if (!fs::dir_exists(dir)) return(character(0))
  files <- fs::path_file(fs::dir_ls(dir, glob = paste0("*_", type, ".json")))
  metawoRld::desanitize_id(sub(paste0("_", type, "\\.json$"), "", files))
}

#' Read cached results
#'
#' @param type `"metadata"`, `"assessment"` or `"extraction"`.
#' @param metawoRld_path Project root.
#' @param identifiers Optional identifiers to read; defaults to all cached.
#' @return A named list (names are identifiers). For assessments and
#'   extractions each element has `result` and `provenance`.
#' @export
df_read_cache <- function(type = c("assessment", "extraction", "metadata"),
                          metawoRld_path = ".", identifiers = NULL) {
  type <- match.arg(type)
  proj <- .check_project(metawoRld_path)
  ids <- if (is.null(identifiers)) .cached_ids(proj, type) else metawoRld::normalize_identifier(identifiers)
  reader <- if (type == "metadata") .cache_read else .cache_get_entry
  out <- lapply(ids, reader, type = type, proj = proj)
  names(out) <- ids
  Filter(Negate(is.null), out)
}

#' Clear cached results
#'
#' @param identifiers Identifiers to clear. `NULL` clears everything of the
#'   given types (asks for confirmation in interactive sessions).
#' @param type One or more of `"metadata"`, `"assessment"`, `"extraction"`.
#' @param metawoRld_path Project root.
#' @param ask Ask before clearing everything? Defaults to `interactive()`.
#' @return Number of files deleted, invisibly.
#' @export
df_clear_cache <- function(identifiers = NULL, type = c("metadata", "assessment", "extraction"),
                           metawoRld_path = ".", ask = interactive()) {
  type <- match.arg(type, .cache_types, several.ok = TRUE)
  proj <- .check_project(metawoRld_path)
  if (is.null(identifiers)) {
    if (ask && !isTRUE(utils::askYesNo(paste0("Delete all cached ", paste(type, collapse = "/"), " results?"), default = FALSE))) {
      return(invisible(0L))
    }
    files <- unlist(lapply(type, function(t) {
      d <- .cache_dir(proj, t)
      if (fs::dir_exists(d)) fs::dir_ls(d, glob = "*.json") else character(0)
    }))
  } else {
    files <- unlist(lapply(type, function(t) vapply(identifiers, .cache_file, character(1), type = t, proj = proj)))
    files <- files[fs::file_exists(files)]
  }
  if (length(files)) fs::file_delete(files)
  rlang::inform(paste0("Deleted ", length(files), " cache file(s)."))
  invisible(length(files))
}

#' Progress of each record
#'
#' Combines the metadata, assessment and extraction caches with the
#' project's submissions and accepted studies.
#'
#' @param metawoRld_path Project root.
#' @param chat Optional ellmer chat used for assessment/extraction. If given,
#'   `assessment_stale`/`extraction_stale` show whether the cached result was
#'   produced with a different prompt, form, criteria or model.
#' @return A tibble with one row per identifier: `has_metadata`,
#'   `has_abstract`, `decision`, `score`, `assessment_source`, `extracted`,
#'   `extraction_errors` (number of form problems), `submitted` (the cached
#'   extraction was submitted), `study_id` and `accepted` (the study has
#'   accepted data in `data/`), and the staleness columns.
#' @export
df_status <- function(metawoRld_path = ".", chat = NULL) {
  proj <- .check_project(metawoRld_path)
  index <- .report_index(proj)
  ids <- unique(c(.cached_ids(proj, "metadata"), .cached_ids(proj, "assessment"), .cached_ids(proj, "extraction")))
  a_hash <- if (!is.null(chat)) .assessment_hash(proj, .model_name(chat)) else NA
  e_hash <- if (!is.null(chat)) .extraction_hash(proj, .model_name(chat)) else NA
  rows <- lapply(ids, function(id) {
    m <- .cache_read(id, "metadata", proj)
    a <- .cache_get_entry(id, "assessment", proj)
    e <- .cache_get_entry(id, "extraction", proj)
    doi <- if (metawoRld::identifier_type(id) == "doi") id else as.character(m$doi %||% "")
    pmid <- if (metawoRld::identifier_type(id) == "pmid") id else as.character(m$pmid %||% "")
    acc <- .study_for_report(index, doi, pmid, "data")
    sid <- if (!is.na(acc)) acc else as.character(e$submission$study_id %||% .study_for_report(index, doi, pmid))
    list(
      identifier = id,
      title = as.character(m$title %||% NA_character_),
      has_metadata = !is.null(m),
      has_abstract = !is.null(m) && .has_text(m$abstract),
      decision = as.character(a$result$decision %||% NA_character_),
      score = as.numeric(a$result$score %||% NA_real_),
      assessment_source = as.character(a$provenance$source %||% NA_character_),
      assessment_stale = if (is.null(a) || is.null(chat)) NA else !.entry_current(a, a_hash),
      extracted = !is.null(e),
      extraction_errors = if (is.null(e)) NA_integer_ else length(e$provenance$validation_errors),
      extraction_stale = if (is.null(e) || is.null(chat)) NA else !.entry_current(e, e_hash),
      submitted = !is.null(e$submission),
      study_id = sid,
      accepted = !is.na(acc)
    )
  })
  empty <- dplyr::tibble(identifier = character(0), title = character(0), has_metadata = logical(0),
                         has_abstract = logical(0), decision = character(0), score = numeric(0),
                         assessment_source = character(0), assessment_stale = logical(0),
                         extracted = logical(0), extraction_errors = integer(0),
                         extraction_stale = logical(0), submitted = logical(0), study_id = character(0),
                         accepted = logical(0))
  dplyr::bind_rows(c(list(empty), rows))
}

#' Counts for a PRISMA flow diagram
#'
#' Deprecated: PRISMA counts come from the project's record registry and
#' screening log, which DataFindR fills. Use [metawoRld::prisma_counts()].
#'
#' @param metawoRld_path Project root.
#' @return See [metawoRld::prisma_counts()].
#' @export
df_prisma_counts <- function(metawoRld_path = ".") {
  .Deprecated("metawoRld::prisma_counts")
  metawoRld::prisma_counts(.check_project(metawoRld_path))
}

#' Studies waiting for data extraction
#'
#' @param metawoRld_path Project root.
#' @param decisions Assessment decisions that qualify. Defaults to `"Include"`.
#' @return Character vector of identifiers that qualify but have no cached
#'   extraction yet.
#' @export
df_pending_extraction <- function(metawoRld_path = ".", decisions = "Include") {
  st <- df_status(metawoRld_path)
  st$identifier[st$decision %in% decisions & !st$extracted]
}

.has_text <- function(x) !is.null(x) && length(x) == 1 && !is.na(x) && nzchar(trimws(x))
