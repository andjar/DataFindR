#' Screen studies by title and abstract with an LLM
#'
#' For each identifier: gets metadata (from the cache, or fetched and cached),
#' fills the project's assessment prompt, asks the LLM for a structured
#' decision following `forms/screening.yml`, validates it, and caches the
#' result together with its provenance (model, token use, cost and a
#' fingerprint of the prompt, schema and criteria).
#'
#' Cached results are reused unless they were produced with a different
#' prompt, schema, criteria or model (then they are redone), or
#' `force = TRUE`. Manual decisions (e.g. saved from [df_shiny_assess()]) are
#' never overwritten unless `force = TRUE`.
#'
#' @param chat An ellmer chat object, e.g. `ellmer::chat_openai(model =
#'   "gpt-4.1-mini")`, or a `"provider/model"` string for [ellmer::chat()].
#' @param identifiers Character vector of DOIs/PMIDs.
#' @param metawoRld_path Project root.
#' @param mode `"parallel"` (default; many requests at once),
#'   `"sequential"` (one by one), or `"batch"` (the provider's batch API:
#'   about half the price, but results can take up to 24 hours; supported by
#'   OpenAI, Anthropic and Google Gemini).
#' @param force Re-assess even if a current cached result exists.
#' @param force_fetch Re-fetch metadata even if cached.
#' @param max_active Maximum simultaneous requests in parallel mode.
#' @param batch_file File used to track a batch job (mode `"batch"`).
#'   Re-run the same call to resume. Defaults to a file in the cache folder.
#' @param wait In batch mode, wait for the results? If `FALSE` and the batch
#'   is not finished, the function returns early; call it again later.
#' @param email,api_key Passed to [df_fetch_metadata()].
#'
#' @return A tibble with one row per identifier: `identifier`, `status`
#'   (`"assessed"`, `"cached"`, `"failed"` or `"pending"`), `decision`,
#'   `score`, `rationale`, `exclusion_reason`, `error`, `input_tokens`,
#'   `output_tokens` and `cost`.
#' @export
#' @examples
#' \dontrun{
#' chat <- ellmer::chat_openai(model = "gpt-4.1-mini")
#' hits <- df_search_pubmed("biomarker X AND pregnancy", "my_review")
#' res <- df_assess_batch(chat, hits$identifier, "my_review")
#' table(res$decision)
#' }
df_assess_batch <- function(chat,
                            identifiers,
                            metawoRld_path = ".",
                            mode = c("parallel", "sequential", "batch"),
                            force = FALSE,
                            force_fetch = FALSE,
                            max_active = 10,
                            batch_file = NULL,
                            wait = TRUE,
                            email = Sys.getenv("DATAFINDR_EMAIL"),
                            api_key = Sys.getenv("NCBI_API_KEY")) {
  mode <- match.arg(mode)
  proj <- .check_project(metawoRld_path)
  chat <- .as_chat(chat)
  ids <- unique(metawoRld::normalize_identifier(identifiers))
  ids <- ids[nzchar(ids)]
  if (!length(ids)) rlang::abort("`identifiers` contains no valid identifiers.")

  cfg <- metawoRld::read_config(proj)
  if (!length(cfg$inclusion_criteria) && !length(cfg$exclusion_criteria)) {
    rlang::abort("No inclusion or exclusion criteria in _metawoRld.yml.")
  }
  schema <- metawoRld::get_schema(proj, "screening")
  type <- df_schema_to_ellmer(schema)
  model <- .model_name(chat)
  hash <- .assessment_hash(proj, model)

  res <- .empty_assessment_rows(ids)

  # 1. Cached results
  todo <- ids
  if (!force) {
    for (id in ids) {
      e <- .cache_get_entry(id, "assessment", proj)
      if (!is.null(e) && .entry_current(e, hash)) {
        res <- .fill_assessment_row(res, id, "cached", e$result)
        todo <- setdiff(todo, id)
      }
    }
  }
  if (!length(todo)) return(.finish_assessment(res))

  # 2. Metadata
  meta <- .get_metadata(todo, proj, force = force_fetch, email = email, api_key = api_key)
  no_meta <- names(meta)[vapply(meta, is.null, logical(1))]
  for (id in no_meta) res <- .fill_assessment_row(res, id, "failed", error = "No metadata (title/abstract) available. Add it with df_add_metadata().")
  todo <- setdiff(todo, no_meta)
  if (!length(todo)) return(.finish_assessment(res))

  # 3. Ask the LLM
  prompts <- lapply(todo, function(id) df_assessment_prompt(meta[[id]], proj))
  rlang::inform(sprintf("Assessing %d stud%s with %s (%s mode)...", length(todo), if (length(todo) == 1) "y" else "ies", model, mode))
  out <- if (mode == "sequential") {
    .run_sequential(chat, lapply(prompts, list), type, flat = TRUE)
  } else {
    .llm_many(chat, prompts, type, mode = mode, flat = TRUE, max_active = max_active,
              path = batch_file %||% fs::path(.cache_dir(proj, "assessment"), "_batch.json"),
              wait = wait)
  }
  if (is.null(out)) {
    rlang::inform("Batch submitted but not finished yet. Run the same call again later to collect the results.")
    for (id in todo) res <- .fill_assessment_row(res, id, "pending")
    return(.finish_assessment(res))
  }

  # 4. Validate, cache and log in the project's screening.csv
  .ensure_tool(proj)
  values <- unlist(schema$decision$`_values`)
  for (i in seq_along(todo)) {
    id <- todo[[i]]
    r <- out$results[[i]]
    if (is.null(r)) {
      res <- .fill_assessment_row(res, id, "failed", error = out$errors[[i]] %||% "Request failed")
      next
    }
    r <- .compact_na(r)
    # tolerate case differences in the decision label
    if (!is.null(r$decision) && !r$decision %in% values) {
      hit <- values[tolower(values) == tolower(r$decision)]
      if (length(hit)) r$decision <- hit[[1]]
    }
    errs <- metawoRld::validate_against_schema(r, schema, path = "assessment")
    .cache_write(id, .cache_entry(r, "llm", model, hash, out$usage[[i]], errs), "assessment", proj)
    if (!length(errs)) .log_screening(proj, id, r$decision, r$exclusion_reason, .tool_id())
    res <- .fill_assessment_row(res, id, if (length(errs)) "failed" else "assessed", r,
                                error = if (length(errs)) paste(errs, collapse = "; ") else NA_character_,
                                usage = out$usage[[i]])
  }
  .finish_assessment(res)
}

#' @rdname df_assess_batch
#' @param identifier A single DOI/PMID.
#' @param ... Further arguments passed to `df_assess_batch()`.
#' @return `df_assess_relevance()`: the assessment as a list (`decision`,
#'   `score`, `rationale`, ...), or an error.
#' @export
df_assess_relevance <- function(chat, identifier, metawoRld_path = ".", ...) {
  res <- df_assess_batch(chat, identifier, metawoRld_path, mode = "sequential", ...)
  if (res$status[1] == "failed") rlang::abort(paste0("Assessment failed for ", res$identifier[1], ": ", res$error[1]))
  e <- .cache_get_entry(res$identifier[1], "assessment", .check_project(metawoRld_path))
  e$result
}

#' Save a manual screening decision
#'
#' Manual decisions are never overwritten by [df_assess_batch()] unless
#' `force = TRUE` is used there. The decision is also logged in the project's
#' `screening.csv` under the reviewer's id ([metawoRld::add_screening()]).
#'
#' @param identifier Study identifier.
#' @param decision One of the decisions allowed by the assessment schema
#'   (by default `"Include"`, `"Exclude"`, `"Needs manual review"`).
#' @param rationale Short justification.
#' @param score Optional inclusion probability (0-1).
#' @param exclusion_reason Optional reason for exclusion.
#' @param metawoRld_path Project root.
#' @param reviewer Reviewer id from `people.yml`. Defaults to the
#'   `metawoRld.person` option. Without a reviewer the decision is only
#'   cached, not logged.
#' @param stage Screening stage: `"title_abstract"` (default) or
#'   `"full_text"`.
#' @return The cache file, invisibly.
#' @export
df_save_assessment <- function(identifier, decision, rationale = "", score = NULL,
                               exclusion_reason = NULL, metawoRld_path = ".", reviewer = NULL,
                               stage = c("title_abstract", "full_text")) {
  stage <- match.arg(stage)
  proj <- .check_project(metawoRld_path)
  schema <- metawoRld::get_schema(proj, "screening")
  values <- unlist(schema$decision$`_values`)
  if (!decision %in% values) rlang::abort(paste0("`decision` must be one of: ", paste(values, collapse = ", ")))
  score <- score %||% switch(decision, Include = 1, Exclude = 0, 0.5)
  r <- .compact_na(list(decision = decision, score = score, rationale = rationale, exclusion_reason = exclusion_reason))
  reviewer <- .person(reviewer)
  usage <- if (is.null(reviewer)) list() else list(reviewer = reviewer)
  f <- .cache_write(identifier, .cache_entry(r, "manual", "manual", NA, usage), "assessment", proj)
  if (is.null(reviewer)) {
    rlang::warn("No reviewer given (or `metawoRld.person` option set): the decision is cached but not logged in screening.csv.")
  } else {
    .log_screening(proj, metawoRld::normalize_identifier(identifier), decision, exclusion_reason %||% "", reviewer, stage)
  }
  invisible(f)
}

# ---- helpers -------------------------------------------------------------------

.run_sequential <- function(chat, contents_list, type, flat) {
  n <- length(contents_list)
  results <- vector("list", n)
  usage <- rep(list(list()), n)
  errors <- rep(NA_character_, n)
  cli::cli_progress_bar("Requests", total = n)
  for (i in seq_len(n)) {
    r <- tryCatch(.llm_structured(chat, contents_list[[i]], type, convert = flat), error = function(e) e)
    if (inherits(r, "error")) {
      errors[i] <- conditionMessage(r)
    } else {
      results[i] <- list(r$result)
      usage[[i]] <- r$usage
    }
    cli::cli_progress_update()
  }
  cli::cli_progress_done()
  list(results = results, usage = usage, errors = errors)
}

.empty_assessment_rows <- function(ids) {
  n <- length(ids)
  dplyr::tibble(
    identifier = ids, status = NA_character_, decision = NA_character_, score = NA_real_,
    rationale = NA_character_, exclusion_reason = NA_character_, error = NA_character_,
    input_tokens = NA_real_, output_tokens = NA_real_, cost = NA_real_
  )
}

.fill_assessment_row <- function(res, id, status, r = list(), error = NA_character_, usage = list()) {
  i <- match(id, res$identifier)
  res$status[i] <- status
  res$decision[i] <- as.character(r$decision %||% NA_character_)
  res$score[i] <- suppressWarnings(as.numeric(r$score %||% NA_real_))
  res$rationale[i] <- as.character(r$rationale %||% NA_character_)
  res$exclusion_reason[i] <- as.character(r$exclusion_reason %||% NA_character_)
  res$error[i] <- error
  for (u in c("input_tokens", "output_tokens", "cost")) res[[u]][i] <- as.numeric(usage[[u]] %||% NA_real_)
  res
}

.finish_assessment <- function(res) {
  tab <- table(factor(res$status, c("assessed", "cached", "failed", "pending")))
  rlang::inform(paste0(names(tab), ": ", tab, collapse = ", "))
  if (any(!is.na(res$cost))) rlang::inform(sprintf("Estimated cost: $%.4f", sum(res$cost, na.rm = TRUE)))
  res
}

# Drop NULL/NA scalars from a (possibly nested) list.
.compact_na <- function(x) {
  if (!is.list(x)) return(x)
  x <- lapply(x, function(v) if (is.list(v)) .compact_na(v) else v)
  keep <- !vapply(x, function(v) is.null(v) || (length(v) == 1 && !is.list(v) && is.na(v)), logical(1))
  if (is.null(names(x))) x else x[keep]
}
