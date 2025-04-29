#' Import Extracted Study Data into a metawoRld Project
#'
#' Reads cached extraction JSON data for a study, validates it against the
#' project schema, optionally merges with cached metadata, transforms it,
#' and imports it into the metawoRld project structure using
#' `metawoRld::add_study_data`.
#'
#' @param identifier Character string. The DOI or PMID of the study to import.
#' @param metawoRld_path Character string. Path to the root of the target
#'   metawoRld project.
#' @param overwrite Logical. Passed to `metawoRld::add_study_data`. If a directory
#'   for the study already exists in `metawoRld/data/`, should its contents be
#'   overwritten? Defaults to `FALSE`.
#' @param validate_json Logical. Should the extracted JSON content be validated
#'   against the project schema and internal links before importing? Defaults to `TRUE`.
#' @param merge_metadata Logical. Should cached metadata (Title, Authors, Year,
#'   Journal, DOI, Abstract fetched via `df_fetch_metadata`) be merged into the
#'   metadata section from the extraction, prioritizing the fetched values for
#'   these specific fields? Defaults to `TRUE`.
#'
#' @return Invisibly returns the path to the study directory within the
#'   `metawoRld` project if successful, otherwise aborts with an error.
#' @export
#' @importFrom fs path path_norm dir_exists file_exists
#' @importFrom rlang is_list list2 inform warn abort `%||%` is_scalar_character is_scalar_logical
#' @importFrom glue glue
#' @importFrom dplyr bind_rows # For converting list of lists to data frame
#' @importFrom purrr map_dfr # Alternative for converting list to data frame
#' @importFrom data.table rbindlist # For converting list of lists to data frame
#' @import metawoRld # Need get_schema, add_study_data, .sanitize_id
df_import_extraction <- function(identifier,
                                 metawoRld_path,
                                 overwrite = FALSE,
                                 validate_json = TRUE,
                                 merge_metadata = TRUE) {

  # --- Input Validation ---
  if (!rlang::is_scalar_character(identifier) || identifier == "") {
    rlang::abort("`identifier` must be a non-empty character string.")
  }
  if (!rlang::is_scalar_character(metawoRld_path)) {
    rlang::abort("`metawoRld_path` must be a character string.")
  }
  if (!rlang::is_scalar_logical(overwrite)) {
    rlang::abort("`overwrite` must be TRUE or FALSE.")
  }
  if (!rlang::is_scalar_logical(validate_json)) {
    rlang::abort("`validate_json` must be TRUE or FALSE.")
  }
  if (!rlang::is_scalar_logical(merge_metadata)) {
    rlang::abort("`merge_metadata` must be TRUE or FALSE.")
  }

  proj_path <- fs::path_norm(metawoRld_path)
  if (!fs::dir_exists(proj_path)) {
    rlang::abort(glue::glue("metawoRld project path not found: {proj_path}"))
  }
  config_path <- fs::path(proj_path, "_metawoRld.yml")
  if (!fs::file_exists(config_path)) {
    rlang::abort(glue::glue("Project configuration file '_metawoRld.yml' not found in: {proj_path}"))
  }

  # --- 1. Load Extraction Cache ---
  rlang::inform(glue::glue("Loading extraction cache for '{identifier}'..."))
  extracted_data <- .check_cache(identifier, type = "extraction", metawoRld_path = proj_path)

  if (is.null(extracted_data)) {
    rlang::abort(glue::glue("Extraction cache file not found or could not be read for identifier '{identifier}'. Please run extraction first."))
  }
  if (!is.list(extracted_data)) { # Should be caught by check_cache, but double check
    rlang::abort(glue::glue("Cached extraction data for '{identifier}' is not a valid list object."))
  }

  # --- 2. Validate JSON (Optional) ---
  if (validate_json) {
    schema <- tryCatch({
      metawoRld::get_schema(path = proj_path)
    }, error = function(e) {
      rlang::abort(c(glue::glue("Failed to get schema from metawoRld project at: {proj_path}"), "i" = e$message), parent = e)
    })
    if(is.null(schema)){ # get_schema returns NULL on certain errors too
      rlang::abort(glue::glue("Failed to get schema from metawoRld project at: {proj_path}"))
    }

    # Use the validation helper - it will abort on failure
    .validate_extracted_data(extracted_data, schema, identifier)
  } else {
    rlang::warn(glue::glue("Skipping JSON validation for '{identifier}' as requested."))
  }

  # --- 3. Load and Merge Metadata (Optional) ---
  final_metadata_list <- extracted_data$metadata # Start with LLM metadata

  if (merge_metadata) {
    rlang::inform(glue::glue("Attempting to merge with cached bibliographic metadata for '{identifier}'..."))
    fetched_metadata <- .check_cache(identifier, type = "metadata", metawoRld_path = proj_path)

    if (!is.null(fetched_metadata) && is.list(fetched_metadata)) {
      # Define fields to potentially overwrite from fetched source
      fields_to_merge <- c("title", "authors", "year", "journal", "doi", "abstract") # 'doi' might be fetched even if identifier is PMID
      merged_count <- 0
      for (field in fields_to_merge) {
        if (!is.null(fetched_metadata[[field]])) {
          # Overwrite if fetched metadata has a non-null value
          final_metadata_list[[field]] <- fetched_metadata[[field]]
          merged_count <- merged_count + 1
        }
      }
      rlang::inform(glue::glue("Merged/prioritized {merged_count} field(s) from cached metadata."))
    } else {
      rlang::inform("No cached bibliographic metadata found or loaded to merge.")
    }
  } else {
    rlang::inform("Skipping merge with cached bibliographic metadata.")
  }

  # --- 4. Data Transformation (List to Data Frame) ---
  data_points_list <- extracted_data$data_points

  if(length(data_points_list) > 0) {
    # Use dplyr::bind_rows for robustness to missing optional columns
    data_df <- tryCatch({
      dplyr::tibble(data.table::rbindlist(data_points_list, fill=TRUE))
      # Consider type conversions here if needed, e.g., using readr::type_convert
      # Or rely on metawoRld::add_study_data / load_metawoRld for later handling
    }, error = function(e) {
      rlang::abort(c(glue::glue("Failed to convert 'data_points' list to data frame for '{identifier}'."),
                     "i" = e$message), parent = e)
    })
    rlang::inform(glue::glue("Converted {length(data_points_list)} data points to data frame."))
  } else {
    # Create an empty data frame matching schema if possible? Or let add_study_data handle?
    # For now, create an empty tibble. add_study_data might need adapting if schema expects cols.
    rlang::inform(glue::glue("No data points extracted for '{identifier}'. Creating empty data frame."))
    data_df <- dplyr::tibble() # An empty tibble
    # If add_study_data requires specific columns even when empty, need schema:
    # schema <- metawoRld::get_schema(proj_path) # Fetch schema if not already done
    # req_data_cols <- schema$data_fields$required %||% character(0)
    # opt_data_cols <- schema$data_fields$optional %||% character(0)
    # all_cols <- unique(c(req_data_cols, opt_data_cols))
    # data_df <- stats::setNames(data.frame(matrix(ncol = length(all_cols), nrow = 0)), all_cols)
  }


  # --- 5. Get Study ID for Saving ---
  study_id_to_save <- final_metadata_list$study_id
  if(is.null(study_id_to_save) || study_id_to_save == "") {
    rlang::abort(glue::glue("Cannot determine study_id from final metadata for identifier '{identifier}'. It is missing or empty after processing."))
  }


  # --- 6. Call metawoRld::add_study_data ---
  rlang::inform(glue::glue("Attempting to import data into metawoRld project for study_id '{study_id_to_save}'..."))

  tryCatch({
    study_dir_path <- metawoRld::add_study_data(
      path = proj_path,
      study_id = study_id_to_save, # Use the ID from the metadata
      metadata_list = final_metadata_list,
      data_df = data_df,
      overwrite = overwrite
    )
    rlang::inform(glue::glue("Successfully imported data for study '{study_id_to_save}' into: {study_dir_path}"))
    invisible(study_dir_path) # Return the path on success
  }, error = function(e) {
    rlang::abort(c(glue::glue("Failed to import data into metawoRld project for study_id '{study_id_to_save}' using identifier '{identifier}'."),
                   "i" = e$message), parent = e)
  })
}

#' @title Import Cached Extraction Data for a Batch of Studies
#'
#' @description
#' Reads cached extraction data (JSON format saved by `df_extract_batch` or
#' manually) for multiple studies and imports them into the specified `metawoRld`
#' project using `df_import_extraction`.
#'
#' @param identifiers Character vector. A vector of DOIs and/or PMIDs for studies
#'   that have cached extraction data ready for import.
#' @param metawoRld_path Character string. Path to the root of the target
#'   metawoRld project.
#' @param overwrite Logical. Passed to `metawoRld::add_study_data` via
#'   `df_import_extraction`. If TRUE, overwrite existing study data in the
#'   `metawoRld` project. Defaults to `FALSE`.
#' @param validate_json Logical. Passed to `df_import_extraction`. If TRUE,
#'   validate the cached JSON against the schema before importing. Defaults to `TRUE`.
#' @param merge_metadata Logical. Passed to `df_import_extraction`. If TRUE,
#'   merge cached bibliographic metadata with extracted metadata. Defaults to `TRUE`.
#' @param stop_on_error Logical. If TRUE, stop the batch if any single import fails.
#'   Defaults to `FALSE`.
#'
#' @return A data frame (tibble) summarizing the import attempt for each
#'   identifier, with columns:
#'   \describe{
#'     \item{`identifier`}{The DOI or PMID.}
#'     \item{`status`}{"Success", "Skipped" (e.g., cache file missing), or "Failure".}
#'     \item{`metawoRld_study_path`}{Path to the study directory if import was successful.}
#'     \item{`error_message`}{The error message if status is "Failure" or "Skipped".}
#'   }
#' Also prints progress and summary information.
#'
#' @export
#' @importFrom purrr map safely list_transpose keep discard set_names map_chr map_lgl compact walk
#' @importFrom dplyr bind_rows tibble mutate select relocate if_else everything
#' @importFrom rlang inform warn is_character abort is_logical `%||%` list2 !!! is_null
#' @importFrom glue glue
#' @import cli
df_import_batch <- function(identifiers,
                            metawoRld_path,
                            overwrite = FALSE,
                            validate_json = TRUE,
                            merge_metadata = TRUE,
                            stop_on_error = FALSE) {

  # --- Input Validation ---
  if (!rlang::is_character(identifiers) || length(identifiers) == 0) {
    rlang::abort("`identifiers` must be a non-empty character vector.")
  }
  # Other args validated downstream or basic types checked

  n_total <- length(identifiers)
  rlang::inform(glue::glue("Starting batch import for {n_total} identifier(s)..."))

  # --- Use purrr::safely for error handling ---
  safe_import <- purrr::safely(
    .f = df_import_extraction,
    otherwise = NULL,
    quiet = FALSE # Show errors from df_import_extraction
  )

  results_list <- list()
  cli::cli_progress_bar("Importing identifiers", total = n_total)

  for (i in seq_along(identifiers)) {
    id <- identifiers[[i]]
    cli::cli_progress_update()

    # Check if extraction cache file exists *before* calling import
    # Avoids unnecessary work/errors in df_import_extraction
    extraction_cache_exists <- !is.null(
      .check_cache(identifier = id, type = "extraction", metawoRld_path = metawoRld_path)
    )

    if (!extraction_cache_exists) {
      rlang::warn(glue::glue("Skipping import for '{id}': Extraction cache file not found."))
      # Store a specific "Skipped" result
      results_list[[id]] <- list(result = list(status = "Skipped", metawoRld_study_path = NA_character_), # Mimic success structure slightly
                                 error = list(message="Extraction cache file not found.")) # Mimic error structure
      next # Move to the next identifier
    }

    # Call safe wrapper for import
    res <- safe_import(
      identifier = id,
      metawoRld_path = metawoRld_path,
      overwrite = overwrite,
      validate_json = validate_json,
      merge_metadata = merge_metadata
    )
    results_list[[id]] <- res # Store result named by identifier

    # Stop processing if an error occurred and stop_on_error is TRUE
    if (!is.null(res$error) && stop_on_error) {
      cli::cli_progress_done(result = "failed")
      extracted_error_msg <- res$error$message %||% "Unknown error during import."
      rlang::abort(glue::glue("Batch import stopped due to error on identifier '{id}': {extracted_error_msg}"), parent = res$error)
    }
  }
  cli::cli_progress_done()

  # --- Process Results List ---
  results_transposed <- purrr::list_transpose(results_list, simplify = FALSE)

  # Create summary tibble
  summary_df <- dplyr::tibble(
    identifier = names(results_list),
    # Check error first, then check internal status for Skipped
    status = purrr::map_chr(seq_along(results_list), ~{
      if (!is.null(results_transposed$error[[.x]]) && results_transposed$result[[.x]]$status %in% "Skipped") {
        "Skipped" # Prioritize Skipped status if indicated internally
      } else if (!is.null(results_transposed$error[[.x]])) {
        "Failure"
      } else {
        "Success" # df_import_extraction returns path on success
      }
    }),
    metawoRld_study_path = purrr::map_chr(results_transposed$result, ~ if(is.character(.x)) .x else NA_character_), # Extract path if successful
    error_message = purrr::map_chr(results_transposed$error, ~ .x$message %||% NA_character_)
  )

  # --- Report Summary ---
  n_success <- sum(summary_df$status == "Success")
  n_skipped <- sum(summary_df$status == "Skipped")
  n_failed <- sum(summary_df$status == "Failure")

  rlang::inform("--- Batch Import Summary ---")
  rlang::inform(glue::glue("Total identifiers processed: {n_total}"))
  rlang::inform(glue::glue("Successful imports: {n_success}"))
  rlang::inform(glue::glue("Skipped (e.g., cache missing): {n_skipped}"))
  rlang::inform(glue::glue("Failed imports: {n_failed}"))
  if (n_failed > 0 || n_skipped > 0) {
    rlang::warn("Issues occurred for the following identifiers (see output table for details):")
    issue_ids <- summary_df$identifier[summary_df$status != "Success"]
    max_print <- 10
    print_ids <- if(length(issue_ids) > max_print) c(issue_ids[1:max_print], "...") else issue_ids
    rlang::warn(paste(print_ids, collapse = ", "))
  }

  return(summary_df)
}
