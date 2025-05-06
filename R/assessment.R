#' @title Assess Study Relevance using LLM
#'
#' @description
#' Fetches metadata for an identifier, generates a prompt based on project
#' criteria, calls an LLM API to assess relevance based on Title/Abstract,
#' parses the response, and caches results.
#'
#' @param chat An `ellmer` chat object.
#' @param identifier Character string. The DOI or PMID of the study.
#' @param metawoRld_path Character string. Path to the root of the metawoRld project.
#' @param force_fetch Logical. If TRUE, bypass the metadata cache and re-fetch
#'   from online sources. Defaults to FALSE.
#' @param force_assess Logical. If TRUE, bypass the assessment cache and
#'   re-run the LLM assessment. Defaults to FALSE.
#' @param service Character string. The LLM service to use (currently only "openai").
#' @param model Character string. The specific LLM model name.
#' @param email Character string (optional). Email for NCBI Entrez.
#' @param ncbi_api_key Character string (optional). NCBI API key.
#' @param ... Additional arguments passed to the underlying LLM API call function
#'   (e.g., `temperature`, `max_tokens` passed to `.call_llm_openai`).
#'
#' @return A list containing the structured assessment result (decision, score,
#'   rationale) or aborts on critical failure.
#' @export
#' @importFrom fs path file_exists path_norm
#' @importFrom yaml read_yaml
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom rlang is_list list2 warn inform abort %||% check_installed set_names
#' @importFrom glue glue
#' @importFrom metawoRld .sanitize_id
#'
#' @examples
#' \dontrun{
#' # --- Prerequisites ---
#' # 1. Set API key: usethis::edit_r_environ("project") -> add OPENAI_API_KEY=sk-... -> Restart R
#' # 2. Create a dummy metawoRld project
#' proj_path <- file.path(tempdir(), "assess_test_proj")
#' metawoRld::create_metawoRld(
#'    proj_path,
#'    project_name = "Test Assessment",
#'    project_description = "Testing DataFindR assessment",
#'    inclusion_criteria = c("Human study", "Pregnancy", "Serum or Plasma", "Cytokine measurement"),
#'    exclusion_criteria = c("Animal study", "Review article", "Non-English")
#' )
#'
#' # --- Run Assessment ---
#' pmid <- "31772108" # Example PMID relevant to cytokines/pregnancy
#' tryCatch({
#'   assessment_res <- df_assess_relevance(
#'      identifier = pmid,
#'      metawoRld_path = proj_path,
#'      email = "your.email@example.com", # Replace with your email
#'      service = "openai",
#'      model = "gpt-3.5-turbo" # Use a cheaper model for testing initially
#'   )
#'   print(assessment_res)
#'
#'   # --- Run again (should use cache) ---
#'   assessment_res_cached <- df_assess_relevance(pmid, proj_path, email = "your.email@example.com")
#'   print(assessment_res_cached)
#'
#'   # --- Force re-assessment ---
#'   assessment_res_forced <- df_assess_relevance(pmid, proj_path, email = "your.email@example.com", force_assess = TRUE)
#'   print(assessment_res_forced)
#'
#' }, error = function(e) {
#'   message("Assessment failed: ", e$message)
#' })
#'
#' # --- Clean up ---
#' unlink(proj_path, recursive = TRUE)
#' }
df_assess_relevance <- function(chat,
                                identifier,
                                metawoRld_path,
                                force_fetch = FALSE,
                                force_assess = FALSE,
                                email = NULL,
                                ncbi_api_key = NULL,
                                ...) {

  # --- Input & Path Checks ---
  if (!rlang::is_scalar_character(identifier) || identifier == "") {
    rlang::abort("`identifier` must be a non-empty character string.")
  }
  if (!rlang::is_scalar_character(metawoRld_path)) {
    rlang::abort("`metawoRld_path` must be a character string.")
  }
  proj_path <- fs::path_norm(metawoRld_path)
  if (!fs::dir_exists(proj_path)) {
    rlang::abort(glue::glue("metawoRld project path not found: {proj_path}"))
  }
  config_path <- fs::path(proj_path, "_metawoRld.yml")
  if (!fs::file_exists(config_path)) {
    rlang::abort(glue::glue("Project configuration file '_metawoRld.yml' not found in: {proj_path}"))
  }

  chat <- chat$clone()
  assessment_prompt_path <- fs::path(metawoRld_path, "_assessment_prompt.txt")
  assessment_schema_path <- fs::path(metawoRld_path, "_assessment_schema.yml")


  # --- 1. Check Assessment Cache ---
  # Use internal caching functions directly
  if (!force_assess) {
    cached_assessment <- .check_cache(identifier, type = "assessment", metawoRld_path = proj_path)
    if (!is.null(cached_assessment) && is.list(cached_assessment)) {
      # Optional: Basic validation of cached structure
      if (all(c("decision", "score", "rationale") %in% names(cached_assessment))) {
        rlang::inform(glue::glue("Using cached assessment result for '{identifier}'."))
        return(cached_assessment)
      } else {
        rlang::warn(glue::glue("Cached assessment for '{identifier}' has unexpected structure. Re-assessing."))
        # Optionally clear the bad cache entry here?
        # df_clear_cache(identifier, type="assessment", metawoRld_path = proj_path)
      }
    }
  }

  # --- 2. Fetch or Load Metadata ---
  rlang::inform(glue::glue("Checking metadata cache for '{identifier}'..."))
  metadata <- NULL
  if (!force_fetch) {
    metadata <- .check_cache(identifier, type = "metadata", metawoRld_path = proj_path)
    if(!is.null(metadata)) {
      rlang::inform("Using cached metadata.")
    }
  }

  if (is.null(metadata)) {
    rlang::inform("Metadata not cached or `force_fetch=TRUE`. Fetching...")
    metadata <- df_fetch_metadata(identifier = identifier, email = email, ncbi_api_key = ncbi_api_key)
    if (is.null(metadata)) {
      rlang::abort(glue::glue("Failed to fetch metadata for '{identifier}'. Cannot proceed with assessment."))
    }
    # Save fetched metadata to cache
    .save_to_cache(identifier = identifier, data = metadata, type = "metadata", metawoRld_path = proj_path)
  }

  # Basic check: ensure metadata is a list with expected components
  if (!is.list(metadata) || is.null(metadata$title) || is.null(metadata$abstract)) {
    rlang::abort(glue::glue("Fetched/cached metadata for '{identifier}' is invalid or missing Title/Abstract."))
  }


  # --- 3. Get Project Criteria ---
  project_config <- tryCatch({
    yaml::read_yaml(config_path)
  }, error = function(e){
    rlang::abort(c("Failed to read or parse _metawoRld.yml", "i" = e$message), parent = e)
  })

  inclusion_criteria <- project_config$inclusion_criteria %||% character(0)
  exclusion_criteria <- project_config$exclusion_criteria %||% character(0)

  if (length(inclusion_criteria) == 0 && length(exclusion_criteria) == 0) {
    rlang::abort("No inclusion or exclusion criteria found in _metawoRld.yml. Cannot perform assessment.")
  }

  # --- 4. Generate Assessment Prompt ---
  assessment_prompt <- .generate_assessment_prompt(
    title = metadata$title,
    abstract = metadata$abstract,
    inclusion_criteria = inclusion_criteria,
    exclusion_criteria = exclusion_criteria
  )

  type_assessment <- parse_yaml_to_ellmer_schema(assessment_schema_path)

  parsed_assessment <- tryCatch({
    chat$extract_data(
      assessment_prompt,
      type = type_assessment,
    )
  }, error = function(e) {
    rlang::abort(c(glue::glue("Failed to get response from LLM for assessment of '{identifier}'."),
                   "i" = e$message), parent = e)
  })

  # Validate structure
  required_fields <- c("decision", "score", "rationale")
  if (!is.list(parsed_assessment) || !all(required_fields %in% names(parsed_assessment))) {
    rlang::abort(glue::glue("LLM assessment JSON for '{identifier}' is missing required fields ({paste(required_fields, collapse=', ')}).\nLLM Raw Content: {llm_content_string}"))
  }
  # Optional: Validate decision value
  valid_decisions <- c("Include", "Exclude", "Needs Manual Review")
  if(!parsed_assessment$decision %in% valid_decisions){
    rlang::warn(glue::glue("LLM assessment decision ('{parsed_assessment$decision}') for '{identifier}' is not one of the expected values: {paste(valid_decisions, collapse=', ')}. Using the value as is."))
  }
  # Optional: Validate score type/range
  if(!is.numeric(parsed_assessment$score) || parsed_assessment$score < 0 || parsed_assessment$score > 1){
    rlang::warn(glue::glue("LLM assessment score ('{parsed_assessment$score}') for '{identifier}' is not a number between 0 and 1. Using the value as is."))
  }

  # Add timestamp and model info?
  tokens_info <- chat$tokens()
  parsed_assessment$assessment_timestamp <- Sys.time()
  parsed_assessment$assessment_model <- chat$get_model()
  parsed_assessment$assessment_prompt_tokens <- chat$last_turn()@tokens[1]
  parsed_assessment$assessment_candidate_tokens <- chat$last_turn()@tokens[2]

  # --- 7. Save Validated Assessment to Cache ---
  .save_to_cache(identifier = identifier, data = parsed_assessment, type = "assessment", metawoRld_path = proj_path)

  rlang::inform(glue::glue("Assessment completed for '{identifier}': {parsed_assessment$decision} (Score: {round(parsed_assessment$score, 2)})"))

  # --- 8. Return Result ---
  return(parsed_assessment)
}

#' @title Assess Relevance for a Batch of Study Identifiers
#'
#' @description
#' Runs the relevance assessment workflow (`df_assess_relevance`) for multiple
#' DOIs/PMIDs, leveraging caching and providing a summary of results.
#'
#' @param identifiers Character vector. A vector of DOIs and/or PMIDs.
#' @param metawoRld_path Character string. Path to the root of the metawoRld project.
#' @param force_fetch Logical. If TRUE, bypass the metadata cache for all identifiers.
#' @param force_assess Logical. If TRUE, bypass the assessment cache for all identifiers.
#' @param service Character string. The LLM service to use (e.g., "openai").
#' @param model Character string. The specific LLM model name.
#' @param email Character string (optional). Email for NCBI Entrez.
#' @param ncbi_api_key Character string (optional). NCBI API key.
#' @param stop_on_error Logical. If TRUE, the batch process stops if any single
#'   assessment fails. If FALSE (default), it attempts to process all identifiers
#'   and reports errors in the summary.
#' @param ... Additional arguments passed down to `df_assess_relevance` and
#'   subsequently to the LLM API call function (e.g., `temperature`).
#'
#' @return A data frame (tibble) summarizing the assessment results for each
#'   identifier, with columns:
#'   \describe{
#'     \item{`identifier`}{The DOI or PMID.}
#'     \item{`status`}{"Success" or "Failure".}
#'     \item{`decision`}{Assessment decision ("Include", "Exclude", etc.) if status is "Success".}
#'     \item{`score`}{Confidence score if status is "Success".}
#'     \item{`rationale`}{LLM rationale if status is "Success".}
#'     \item{`error_message`}{The error message if status is "Failure".}
#'   }
#' Also prints progress and summary information to the console. Assessment
#' results are saved to the cache within the `metawoRld` project.
#'
#' @export
#' @importFrom purrr map safely list_transpose keep discard set_names map_chr map_dbl map_lgl compact
#' @importFrom dplyr bind_rows tibble mutate select relocate if_else everything
#' @importFrom rlang inform warn is_character abort is_logical `%||%` list2 !!!
#' @importFrom glue glue
#' @import cli
#'
#' @examples
#' \dontrun{
#' # --- Prerequisites ---
#' # 1. Set API key: usethis::edit_r_environ("project") -> add OPENAI_API_KEY=sk-... -> Restart R
#' # 2. Create a dummy metawoRld project
#' proj_path <- file.path(tempdir(), "assess_batch_proj")
#' metawoRld::create_metawoRld(
#'    proj_path,
#'    project_name = "Test Batch Assessment",
#'    project_description = "Testing DataFindR batch assessment",
#'    inclusion_criteria = c("Human study", "Pregnancy", "Serum or Plasma", "Cytokine measurement"),
#'    exclusion_criteria = c("Animal study", "Review article", "Non-English")
#' )
#'
#' # --- Identifiers from a hypothetical search ---
#' ids_to_assess <- c(
#'   "31772108", # Should likely be Include
#'   "25376210", # Should likely be Include
#'   "invalid_pmid", # Should fail fetch
#'   "10.1038/nature14539" # Example DOI (Nature review, likely Exclude)
#' )
#'
#' # --- Run Batch Assessment ---
#' batch_results <- df_assess_batch(
#'   identifiers = ids_to_assess,
#'   metawoRld_path = proj_path,
#'   email = "your.email@example.com", # Replace with your email
#'   service = "openai",
#'   model = "gpt-3.5-turbo",
#'   stop_on_error = FALSE # Continue processing even if one fails
#' )
#'
#' # --- View Results ---
#' print(batch_results)
#'
#' # --- Clean up ---
#' unlink(proj_path, recursive = TRUE)
#' }
df_assess_batch <- function(chat,
                            identifiers,
                            metawoRld_path,
                            force_fetch = FALSE,
                            force_assess = FALSE,
                            email = NULL,
                            ncbi_api_key = NULL,
                            stop_on_error = FALSE,
                            ...) {

  # --- Input Validation ---
  if (!rlang::is_character(identifiers) || length(identifiers) == 0) {
    rlang::abort("`identifiers` must be a non-empty character vector.")
  }
  # Other args validated within df_assess_relevance

  n_total <- length(identifiers)
  rlang::inform(glue::glue("Starting batch assessment for {n_total} identifier(s)..."))

  # --- Use purrr::safely for error handling ---
  # Pass ... args correctly to df_assess_relevance
  safe_assess <- purrr::safely(
    .f = df_assess_relevance,
    otherwise = NULL, # Return NULL on error within the wrapper
    quiet = FALSE # Show errors from df_assess_relevance as they happen
  )

  # Prepare arguments list for map - ensure ... are passed correctly
  # Need to construct a list where each element contains args for one call
  # This is complex if ... needs careful handling. Simpler: call inside map.

  results_list <- list()
  cli::cli_progress_bar("Assessing identifiers", total = n_total)

  for (i in seq_along(identifiers)) {
    id <- identifiers[[i]]
    cli::cli_progress_update()
    # Capture result/error for this ID
    res <- safe_assess(
      chat = chat,
      identifier = id,
      metawoRld_path = metawoRld_path,
      force_fetch = force_fetch,
      force_assess = force_assess,
      email = email,
      ncbi_api_key = ncbi_api_key,
      ... # Pass extra arguments
    )
    results_list[[id]] <- res # Store result named by identifier

    # Stop processing if an error occurred and stop_on_error is TRUE
    if (!is.null(res$error) && stop_on_error) {
      cli::cli_progress_done(result = "failed")
      extracted_error_msg <- res$error$message %||% "Unknown error during assessment."
      rlang::abort(glue::glue("Batch assessment stopped due to error on identifier '{id}': {extracted_error_msg}"), parent = res$error)
    }
  }
  cli::cli_progress_done()

  # --- Process Results List ---
  # Transpose to easily access all results or all errors
  results_transposed <- purrr::list_transpose(results_list, simplify = FALSE)

  # Create summary tibble
  summary_df <- dplyr::tibble(
    identifier = names(results_list),
    status = dplyr::if_else(purrr::map_lgl(results_transposed$error, rlang::is_null), "Success", "Failure"),
    # Extract fields safely using map_* with default NA
    decision = purrr::map_chr(results_transposed$result, ~ .x$decision %||% NA_character_),
    score = purrr::map_dbl(results_transposed$result, ~ .x$score %||% NA_real_),
    rationale = purrr::map_chr(results_transposed$result, ~ .x$rationale %||% NA_character_),
    error_message = purrr::map_chr(results_transposed$error, ~ .x$message %||% NA_character_)
  )

  # --- Report Summary ---
  n_success <- sum(summary_df$status == "Success")
  n_failed <- n_total - n_success

  rlang::inform("--- Batch Assessment Summary ---")
  rlang::inform(glue::glue("Total identifiers processed: {n_total}"))
  rlang::inform(glue::glue("Successful assessments: {n_success}"))
  rlang::inform(glue::glue("Failed assessments: {n_failed}"))
  if (n_failed > 0) {
    rlang::warn("Failures occurred for the following identifiers (see output table for details):")
    failed_ids <- summary_df$identifier[summary_df$status == "Failure"]
    # Print only first few failed IDs if list is long
    max_print <- 10
    print_ids <- if(length(failed_ids) > max_print) c(failed_ids[1:max_print], "...") else failed_ids
    rlang::warn(paste(print_ids, collapse = ", "))
  }

  return(summary_df)
}

#' @export
df_add_meta_manual <- function(identifier, meta, metawoRldPath) {
  .save_to_cache(identifier = identifier, data = meta, type = "metadata", metawoRld_path = metawoRldPath)
}
