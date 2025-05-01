#' @title Extract Data for a Batch of Studies using LLM
#'
#' @description
#' Runs the LLM data extraction workflow for multiple studies, taking paths to
#' full-text files as input. Saves the raw JSON output from the LLM to the
#' extraction cache within the `metawoRld` project. Does *not* import into metawoRld.
#' Use `df_import_batch` subsequently to import cached results.
#'
#' @param identifiers Character vector. A vector of DOIs and/or PMIDs for studies
#'   identified as relevant (e.g., having an "Include" assessment decision).
#' @param paper_paths Character vector. **Named** vector or list where names are
#'   the identifiers (matching `identifiers` argument) and values are the file
#'   paths to the corresponding full-text **plain text (.txt) files**.
#' @param metawoRld_path Character string. Path to the root of the metawoRld project.
#' @param force_extract Logical. If TRUE, bypass the extraction cache and re-run
#'   LLM extraction even if cached JSON exists. Defaults to FALSE.
#' @param service Character string. The LLM service to use (e.g., "openai").
#' @param model Character string. The specific LLM model name (e.g., "gpt-4-turbo").
#' @param stop_on_error Logical. If TRUE, stop the batch if any single extraction fails.
#'   Defaults to FALSE.
#' @param ... Additional arguments passed down to the LLM API call function
#'   (e.g., `temperature`, `max_tokens`).
#'
#' @return A data frame (tibble) summarizing the extraction attempt for each
#'   identifier, with columns:
#'   \describe{
#'     \item{`identifier`}{The DOI or PMID.}
#'     \item{`status`}{"Success" (LLM ran, JSON saved), "Cached" (used existing cache), "Skipped" (e.g., missing paper path), or "Failure".}
#'     \item{`cache_file`}{Path to the saved/checked cache file if status is "Success" or "Cached".}
#'     \item{`error_message`}{The error message if status is "Failure".}
#'   }
#' Also prints progress and summary information.
#'
#' @export
#' @importFrom purrr map safely list_transpose keep discard set_names map_chr map_lgl compact walk pmap # Added pmap walk
#' @importFrom dplyr bind_rows tibble mutate select relocate if_else everything rename # Added rename
#' @importFrom rlang inform warn is_character abort is_logical is_named `%||%` list2 !!! is_null # Added is_named is_null
#' @importFrom glue glue
#' @importFrom fs file_exists path_ext path_norm
#' @import cli
#' @importFrom jsonlite validate # Added validate
#' @importFrom metawoRld .sanitize_id # Need export or copy
df_extract_batch <- function(chat,
                             identifiers,
                             paper_paths,
                             metawoRld_path,
                             extraction_prompt_path = system.file(fs::path("prompts", "_extraction_prompt.txt"), package = "DataFindR"),
                             extraction_schema_path = system.file(fs::path("prompts", "_extraction_schema.yml"), package = "DataFindR"),
                             force_extract = FALSE,
                             stop_on_error = FALSE,
                             ellmer_timeout_s = 300,
                             ...) {

  # --- Input Validation ---
  if (!rlang::is_character(identifiers) || length(identifiers) == 0) {
    rlang::abort("`identifiers` must be a non-empty character vector.")
  }
  if (!rlang::is_named(paper_paths) || !all(identifiers %in% names(paper_paths))) {
    missing_names <- setdiff(identifiers, names(paper_paths))
    rlang::abort(glue::glue("`paper_paths` must be a named vector/list with names matching all provided `identifiers`. Missing paths for: {paste(missing_names, collapse=', ')}"))
  }
  # Other args validated downstream or basic types checked

  n_total <- length(identifiers)
  rlang::inform(glue::glue("Starting batch extraction for {n_total} identifier(s)..."))

  # --- Internal Extraction Worker Function (with error handling) ---
  .extract_single_safe <- function(id, paper_path, meta_path, force, svc, mdl, ...) {
    rlang::inform(glue::glue("Processing extraction for: {id}")) # Inner message

    # 1. Check paper path validity
    if(is.null(paper_path) || !rlang::is_scalar_character(paper_path) || paper_path == ""){
      stop("Missing or invalid paper path provided.") # Stop this item
    }
    paper_file <- fs::path_norm(paper_path)
    if(!fs::file_exists(paper_file)) {
      stop(glue::glue("Paper file not found: {paper_file}"))
    }
    # Basic check for TXT - enhance later for PDF etc.
    if(tolower(fs::path_ext(paper_file)) != "pdf") {
      stop("Only pdf (.pdf) files currently supported for batch extraction.")
    }

    # 2. Check extraction cache
    cached_data <- NULL
    cache_file_path <- NA_character_ # Placeholder for return value
    if (!force) {
      cached_data <- .check_cache(identifier = id, type = "extraction", metawoRld_path = meta_path)
      if (!is.null(cached_data)) {
        sanitized_id_for_path <- metawoRld::.sanitize_id(id)
        cache_dir <- .get_datafindr_cache_path(meta_path)
        filename <- paste0(sanitized_id_for_path, "_extraction.json")
        cache_file_path <- fs::path(cache_dir, "extraction", filename)
        return(list(status = "Cached", cache_file = cache_file_path, error_message = NA_character_))
      }
    }

    # 3. Read paper content
    # paper_content <- tryCatch({
    #   paste(readLines(paper_file, warn = FALSE), collapse = "\n")
    # }, error = function(e) stop(glue::glue("Error reading paper file '{paper_file}': {e$message}")))
    # if (!nzchar(trimws(paper_content))) {
    #   stop(glue::glue("Paper file '{paper_file}' is empty."))
    # }

    # 4. Generate Prompt
    # Fetch metadata for context? Optional, but helps prompt focus.
    fetched_metadata <- .check_cache(identifier = id, type = "metadata", metawoRld_path = meta_path)
    extraction_prompt <- tryCatch({
      .generate_extraction_prompt()
    }, error = function(e) stop(glue::glue("Error generating extraction prompt: {e$message}")))

    uploaded_file_info <- tryCatch({
      ellmer::content_pdf_file(paper_file)
    }, error = function(e) {
      message("Error during file upload: ", e$message)
      NULL
    })

    type_extraction <- parse_yaml_to_ellmer_schema(extraction_schema_path)

    parsed_data <- tryCatch({
      withr::local_options(ellmer_timeout_s = ellmer_timeout_s)
      chat$extract_data(
        uploaded_file_info,
        extraction_prompt,
        type = type_extraction,
      )
    }, error = function(e) {
      rlang::abort(c(glue::glue("Failed to get response from LLM for extraction of '{id}'."),
                     "i" = e$message), parent = e)
    })

    # Add metadata about extraction run
    parsed_data$extraction_timestamp <- Sys.time()
    parsed_data$extraction_model <- chat$get_model()
    parsed_data$extration_total_tokens <- sum(chat$last_turn()@tokens)

    # 8. Save Parsed Data to Cache
    save_path <- .save_to_cache(
      identifier = id,
      data = parsed_data, # Save the parsed list
      type = "extraction",
      metawoRld_path = meta_path
    )

    if(is.null(save_path)) {
      stop("Failed to save extraction result to cache.")
    }

    # Return success status
    return(list(status = "Success", cache_file = save_path, error_message = NA_character_))
  }

  # Wrap the worker in safely
  safe_extract_single <- purrr::safely(.extract_single_safe, otherwise = NULL, quiet = FALSE)

  # --- Iterate using pmap (for multiple arguments) ---
  # Create input list for pmap
  pmap_inputs <- list(
    id = identifiers,
    paper_path = paper_paths[identifiers], # Ensure correct paths are matched
    meta_path = rep(metawoRld_path, n_total),
    force = rep(force_extract, n_total),
    # How to pass ... to pmap? Need to capture them and replicate.
    dots_args = rep(list(rlang::list2(...)), n_total) # Capture ... and replicate
  )

  results_list <- list()
  cli::cli_progress_bar("Extracting identifiers", total = n_total)

  for (i in seq_len(n_total)) {
    id <- pmap_inputs$id[[i]]
    cli::cli_progress_update()
    # Call safely, splicing the dots_args
    res <- safe_extract_single(
      id = id,
      paper_path = pmap_inputs$paper_path[[i]],
      meta_path = pmap_inputs$meta_path[[i]],
      force = pmap_inputs$force[[i]],
      !!!pmap_inputs$dots_args[[i]] # Splice the extra arguments
    )
    results_list[[id]] <- res # Store result named by identifier

    # Stop processing if an error occurred and stop_on_error is TRUE
    if (!is.null(res$error) && stop_on_error) {
      cli::cli_progress_done(result = "failed")
      extracted_error_msg <- res$error$message %||% "Unknown error during extraction."
      rlang::abort(glue::glue("Batch extraction stopped due to error on identifier '{id}': {extracted_error_msg}"), parent = res$error)
    }
  }
  cli::cli_progress_done()

  # --- Process Results List ---
  results_transposed <- purrr::list_transpose(results_list, simplify = FALSE)

  # Create summary tibble
  summary_df <- dplyr::tibble(
    identifier = names(results_list),
    # Determine status based on error and result content
    status = purrr::map_chr(seq_along(results_list), ~{
      if (!is.null(results_transposed$error[[.x]])) {
        "Failure"
      } else if (is.list(results_transposed$result[[.x]])) {
        results_transposed$result[[.x]]$status %||% "Unknown" # Should be Success or Cached
      } else {
        "Unknown" # Should not happen with safely unless otherwise=NULL failed
      }
    }),
    cache_file = purrr::map_chr(results_transposed$result, ~ .x$cache_file %||% NA_character_),
    error_message = purrr::map_chr(results_transposed$error, ~ .x$message %||% NA_character_)
  )

  # --- Report Summary ---
  n_success <- sum(summary_df$status == "Success")
  n_cached <- sum(summary_df$status == "Cached")
  n_failed <- sum(summary_df$status == "Failure")
  n_skipped <- sum(!summary_df$status %in% c("Success", "Cached", "Failure")) # Should be 0 normally

  rlang::inform("--- Batch Extraction Summary ---")
  rlang::inform(glue::glue("Total identifiers processed: {n_total}"))
  rlang::inform(glue::glue("Successful extractions (new): {n_success}"))
  rlang::inform(glue::glue("Used existing cache: {n_cached}"))
  rlang::inform(glue::glue("Failures/Skipped: {n_failed + n_skipped}"))
  if (n_failed > 0) {
    rlang::warn("Failures occurred for the following identifiers (see output table for details):")
    failed_ids <- summary_df$identifier[summary_df$status == "Failure"]
    max_print <- 10
    print_ids <- if(length(failed_ids) > max_print) c(failed_ids[1:max_print], "...") else failed_ids
    rlang::warn(paste(print_ids, collapse = ", "))
  }

  return(summary_df)

}
