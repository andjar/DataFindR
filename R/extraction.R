#' Extract data from full texts with an LLM
#'
#' Sends each full text with the project's extraction prompt and asks for a
#' structured result following `_extraction_schema.yml`. The result is
#' validated against the schema (including the `_ref` links between data
#' points and metadata) and cached with its provenance and any validation
#' problems. Use [df_import_batch()] to move the results into the project.
#'
#' Full texts can be PDF (sent as a document to models that support it),
#' plain text/Markdown, or HTML/XML (e.g. JATS from PubMed Central), whose
#' text is extracted first.
#'
#' @param chat An ellmer chat object or `"provider/model"` string. Use a model
#'   that accepts PDFs if you pass PDFs.
#' @param files Named character vector of file paths, names being the study
#'   identifiers; or a single folder, in which case files named after the
#'   identifiers are used (see [df_find_fulltexts()]).
#' @param metawoRld_path Project root.
#' @param mode `"sequential"` (default), `"parallel"` or `"batch"` (see
#'   [df_assess_batch()]). Full texts are large, so mind your provider's rate
#'   limits when using `"parallel"`.
#' @param force Re-extract even if a current cached result exists.
#' @param max_active,batch_file,wait See [df_assess_batch()].
#' @return A tibble with `identifier`, `status` (`"extracted"`, `"cached"`,
#'   `"failed"` or `"pending"`), `n_data_points`, `n_problems` (schema
#'   validation problems), `error`, `input_tokens`, `output_tokens`, `cost`.
#' @export
#' @examples
#' \dontrun{
#' chat <- ellmer::chat_anthropic()
#' todo <- df_pending_extraction("my_review")
#' # PDFs saved as fulltext/<identifier>.pdf (DOIs with "/" replaced, see df_find_fulltexts())
#' res <- df_extract_batch(chat, "my_review/fulltext", "my_review")
#' df_import_batch(metawoRld_path = "my_review")
#' }
df_extract_batch <- function(chat,
                             files,
                             metawoRld_path = ".",
                             mode = c("sequential", "parallel", "batch"),
                             force = FALSE,
                             max_active = 3,
                             batch_file = NULL,
                             wait = TRUE) {
  mode <- match.arg(mode)
  proj <- .check_project(metawoRld_path)
  chat <- .as_chat(chat)
  if (length(files) == 1 && is.null(names(files)) && fs::dir_exists(files)) files <- df_find_fulltexts(files)
  if (!length(files) || is.null(names(files)) || any(!nzchar(names(files)))) {
    rlang::abort("`files` must be a named vector (names = identifiers) or a folder of full texts.")
  }
  ids <- metawoRld::normalize_identifier(names(files))
  files <- rlang::set_names(as.character(files), ids)

  schema <- metawoRld::get_schema(proj, "extraction")
  type <- df_schema_to_ellmer(schema)
  model <- .model_name(chat)
  hash <- .extraction_hash(proj, model)

  res <- dplyr::tibble(identifier = ids, status = NA_character_, n_data_points = NA_integer_,
                       n_problems = NA_integer_, error = NA_character_,
                       input_tokens = NA_real_, output_tokens = NA_real_, cost = NA_real_)
  set_row <- function(res, id, ...) {
    v <- list(...)
    for (k in names(v)) res[[k]][match(id, res$identifier)] <- v[[k]]
    res
  }

  todo <- character(0)
  contents <- list()
  for (id in ids) {
    if (!force) {
      e <- .cache_get_entry(id, "extraction", proj)
      if (!is.null(e) && .entry_current(e, hash)) {
        res <- set_row(res, id, status = "cached", n_data_points = length(e$result$data_points),
                       n_problems = length(e$provenance$validation_errors))
        next
      }
    }
    content <- tryCatch(.fulltext_content(files[[id]]), error = function(e) e)
    if (inherits(content, "error")) {
      res <- set_row(res, id, status = "failed", error = conditionMessage(content))
      next
    }
    todo <- c(todo, id)
    contents[[id]] <- list(content, df_extraction_prompt(id, proj))
  }
  if (!length(todo)) return(.finish_extraction(res))

  rlang::inform(sprintf("Extracting %d stud%s with %s (%s mode)...", length(todo), if (length(todo) == 1) "y" else "ies", model, mode))
  out <- if (mode == "sequential") {
    .run_sequential(chat, unname(contents[todo]), type, flat = FALSE)
  } else {
    .llm_many(chat, unname(contents[todo]), type, mode = mode, flat = FALSE, max_active = max_active,
              path = batch_file %||% fs::path(.cache_dir(proj, "extraction"), "_batch.json"), wait = wait)
  }
  if (is.null(out)) {
    rlang::inform("Batch submitted but not finished yet. Run the same call again later to collect the results.")
    for (id in todo) res <- set_row(res, id, status = "pending")
    return(.finish_extraction(res))
  }

  for (i in seq_along(todo)) {
    id <- todo[[i]]
    r <- out$results[[i]]
    if (is.null(r)) {
      res <- set_row(res, id, status = "failed", error = out$errors[[i]] %||% "Request failed")
      next
    }
    r <- .compact_na(r)
    errs <- .extraction_problems(r, schema)
    .cache_write(id, .cache_entry(r, "llm", model, hash, out$usage[[i]], errs), "extraction", proj)
    u <- out$usage[[i]]
    res <- set_row(res, id, status = "extracted", n_data_points = length(r$data_points),
                   n_problems = length(errs),
                   input_tokens = as.numeric(u$input_tokens %||% NA), output_tokens = as.numeric(u$output_tokens %||% NA),
                   cost = as.numeric(u$cost %||% NA))
  }
  .finish_extraction(res)
}

#' @rdname df_extract_batch
#' @param identifier A single study identifier.
#' @param file Path to its full text.
#' @param ... Further arguments passed to `df_extract_batch()`.
#' @return `df_extract()`: the extracted data as a list (`metadata`,
#'   `data_points`), or an error.
#' @export
df_extract <- function(chat, identifier, file, metawoRld_path = ".", ...) {
  res <- df_extract_batch(chat, rlang::set_names(file, identifier), metawoRld_path, mode = "sequential", ...)
  if (res$status[1] == "failed") rlang::abort(paste0("Extraction failed for ", res$identifier[1], ": ", res$error[1]))
  .cache_get_entry(res$identifier[1], "extraction", .check_project(metawoRld_path))$result
}

#' Save a manually written or corrected extraction
#'
#' @param identifier Study identifier.
#' @param extraction List with `metadata` and `data_points` (as returned by
#'   [df_extract()]), or a JSON string.
#' @param metawoRld_path Project root.
#' @return The validation problems (character vector), invisibly. The result
#'   is saved even if there are problems, so it can be fixed later.
#' @export
df_save_extraction <- function(identifier, extraction, metawoRld_path = ".") {
  proj <- .check_project(metawoRld_path)
  if (is.character(extraction)) extraction <- jsonlite::fromJSON(extraction, simplifyVector = FALSE)
  if (!is.list(extraction) || is.null(extraction$metadata)) rlang::abort("`extraction` must contain `metadata` and `data_points`.")
  errs <- .extraction_problems(extraction, metawoRld::get_schema(proj, "extraction"))
  .cache_write(identifier, .cache_entry(extraction, "manual", "manual", NA, list(), errs), "extraction", proj)
  if (length(errs)) rlang::warn(c("Saved, but the extraction has problems:", rlang::set_names(utils::head(errs, 10), rep("x", min(10, length(errs))))))
  invisible(errs)
}

#' Find full-text files named after study identifiers
#'
#' File names are the identifier encoded with [metawoRld::sanitize_id()]
#' (e.g. `10.1000_fslash_abc.pdf` for DOI `10.1000/abc`, or `12345678.pdf` for
#' a PMID).
#'
#' @param dir Folder to search (not recursive).
#' @return Named character vector of file paths; names are identifiers.
#' @export
df_find_fulltexts <- function(dir) {
  f <- fs::dir_ls(dir, regexp = "\\.(pdf|txt|md|html?|xml|nxml)$", ignore.case = TRUE)
  ids <- metawoRld::normalize_identifier(metawoRld::desanitize_id(fs::path_ext_remove(fs::path_file(f))))
  rlang::set_names(as.character(f), ids)
}

#' Suggested file name for a study's full text
#'
#' @param identifier Study identifier(s).
#' @param ext File extension.
#' @return File name(s), e.g. `"10.1000_fslash_abc.pdf"`.
#' @export
df_fulltext_filename <- function(identifier, ext = "pdf") {
  paste0(metawoRld::sanitize_id(metawoRld::normalize_identifier(identifier)), ".", ext)
}

# ---- helpers -------------------------------------------------------------------

.fulltext_content <- function(file) {
  if (is.null(file) || is.na(file) || !fs::file_exists(file)) rlang::abort(paste0("Full text not found: ", file))
  ext <- tolower(fs::path_ext(file))
  switch(
    ext,
    pdf = ellmer::content_pdf_file(file),
    txt = , md = paste0("Full text:\n\n", paste(readLines(file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")),
    html = , htm = paste0("Full text:\n\n", xml2::xml_text(xml2::read_html(file))),
    xml = , nxml = paste0("Full text:\n\n", xml2::xml_text(xml2::read_xml(file))),
    rlang::abort(paste0("Unsupported full-text format: .", ext, " (use pdf, txt, md, html or xml)"))
  )
}

.extraction_problems <- function(r, schema) {
  c(
    metawoRld::validate_against_schema(r$metadata, schema$metadata, path = "metadata"),
    metawoRld::validate_against_schema(r$data_points %||% list(), schema$data_points, path = "data_points"),
    metawoRld::validate_links(r$metadata, r$data_points %||% list(), schema)
  )
}

.finish_extraction <- function(res) {
  tab <- table(factor(res$status, c("extracted", "cached", "failed", "pending")))
  rlang::inform(paste0(names(tab), ": ", tab, collapse = ", "))
  if (any(res$n_problems > 0, na.rm = TRUE)) {
    rlang::warn(sprintf("%d extraction(s) have schema problems; see `n_problems` and df_status(). They can be fixed with df_shiny_extract() or df_save_extraction().",
                        sum(res$n_problems > 0, na.rm = TRUE)))
  }
  if (any(!is.na(res$cost))) rlang::inform(sprintf("Estimated cost: $%.4f", sum(res$cost, na.rm = TRUE)))
  res
}
