#' Caching Utility Functions for DataFindR
#'
#' This file contains functions for managing cached data within a metawoRld project.
#' It includes functions for saving, loading, and clearing cached items related to
#' metadata, assessment results, and extracted data.
#' Get the Path to the DataFindR Cache Directory within a metawoRld Project
#'
#' @param metawoRld_path Path to the root of the metawoRld project.
#' @param create Logical. If TRUE, create the directory if it doesn't exist.
#' @return Path to the DataFindR cache directory (`<metawoRld_path>/.metawoRld_cache/datafindr`).
#' @noRd
#' @keywords internal
#' @importFrom fs path dir_exists dir_create
.get_datafindr_cache_path <- function(metawoRld_path, create = FALSE) {
  # Standardize the project path first
  proj_path <- fs::path_norm(metawoRld_path)
  if (!fs::dir_exists(proj_path)) {
    rlang::abort(glue::glue("metawoRld project path not found: {proj_path}"))
  }
  cache_base <- fs::path(proj_path, ".metawoRld_cache")
  df_cache_path <- fs::path(cache_base, "datafindr")

  if (create && !fs::dir_exists(df_cache_path)) {
    # Create parent first if needed
    if (!fs::dir_exists(cache_base)) {
      fs::dir_create(cache_base)
      # Optional: Add a .gitignore inside .metawoRld_cache if needed?
      # Or assume the main .gitignore handles it if cache isn't tracked.
    }
    fs::dir_create(df_cache_path)
    # Create subdirs too
    fs::dir_create(fs::path(df_cache_path, "assessment"))
    fs::dir_create(fs::path(df_cache_path, "extraction"))
    fs::dir_create(fs::path(df_cache_path, "metadata")) # For fetched abstracts etc.
  }
  return(df_cache_path)
}


#' Check Cache for a Specific Identifier and Type
#'
#' @param identifier The original identifier (e.g., DOI). It will be sanitized.
#' @param type Cache type: "assessment", "extraction", or "metadata".
#' @param metawoRld_path Path to the metawoRld project containing the cache.
#' @return The content of the cache file if found (parsed JSON or text),
#'   otherwise `NULL`.
#' @noRd
#' @keywords internal
#' @importFrom fs path file_exists
#' @importFrom jsonlite fromJSON
#' @importFrom rlang warn inform
#' @importFrom metawoRld .sanitize_id
.check_cache <- function(identifier, type = c("assessment", "extraction", "metadata"), metawoRld_path) {
  type <- rlang::arg_match(type)
  cache_dir <- .get_datafindr_cache_path(metawoRld_path, create = FALSE) # Don't create on check


  sanitized_id <- metawoRld::.sanitize_id(identifier)
  if (sanitized_id == "") {
    rlang::warn("Cannot check cache for empty identifier.")
    return(NULL)
  }

  # Define filename based on type
  filename <- switch(type,
                     assessment = paste0(sanitized_id, "_assessment.json"),
                     extraction = paste0(sanitized_id, "_extraction.json"),
                     metadata = paste0(sanitized_id, "_metadata.json") # Store fetched metadata as JSON too
  )

  cache_file_path <- fs::path(cache_dir, type, filename)

  if (fs::file_exists(cache_file_path)) {
    rlang::inform(glue::glue("Found cached {type} result for '{identifier}'."))
    # Read and parse JSON, handle potential errors
    tryCatch({
      # Read as text first, then parse, more robust to empty files?
      # content_raw <- readChar(cache_file_path, file.info(cache_file_path)$size)
      # if(nchar(trimws(content_raw)) == 0) return(NULL) # Handle empty file
      # jsonlite::fromJSON(content_raw, simplifyVector = FALSE) # Prefer lists

      # Simpler: direct fromJSON
      jsonlite::fromJSON(cache_file_path, simplifyVector = FALSE)

    }, error = function(e) {
      rlang::warn(glue::glue("Failed to read or parse cached file: {cache_file_path}. Error: {e$message}"))
      return(NULL)
    })
  } else {
    return(NULL)
  }
}


#' Save Data to the DataFindR Cache within a metawoRld Project
#'
#' @param identifier The original identifier (e.g., DOI). It will be sanitized.
#' @param data The data to save (typically a list/object to be saved as JSON).
#' @param type Cache type: "assessment", "extraction", or "metadata".
#' @param metawoRld_path Path to the metawoRld project containing the cache.
#' @return Invisibly returns the path to the saved cache file, or NULL on failure.
#' @noRd
#' @keywords internal
#' @importFrom fs path file_exists
#' @importFrom jsonlite toJSON
#' @importFrom rlang warn inform
#' @importFrom metawoRld .sanitize_id
.save_to_cache <- function(identifier, data, type = c("assessment", "extraction", "metadata"), metawoRld_path) {
  type <- rlang::arg_match(type)
  # Ensure cache directory exists
  cache_dir <- .get_datafindr_cache_path(metawoRld_path, create = TRUE)

  sanitized_id <- metawoRld::.sanitize_id(identifier)
  if (sanitized_id == "") {
    rlang::warn("Cannot save cache for empty identifier.")
    return(invisible(NULL))
  }

  # Define filename based on type
  filename <- switch(type,
                     assessment = paste0(sanitized_id, "_assessment.json"),
                     extraction = paste0(sanitized_id, "_extraction.json"),
                     metadata = paste0(sanitized_id, "_metadata.json")
  )

  cache_file_path <- fs::path(cache_dir, type, filename)

  tryCatch({
    # Convert R object to JSON string
    json_string <- jsonlite::toJSON(data, auto_unbox = TRUE, pretty = TRUE, force = TRUE)
    # Write to file
    writeLines(json_string, con = cache_file_path)
    rlang::inform(glue::glue("Saved {type} result for '{identifier}' to cache: {fs::path_rel(cache_file_path, start = metawoRld_path)}"))
    return(invisible(cache_file_path))
  }, error = function(e) {
    rlang::warn(glue::glue("Failed to save data to cache file: {cache_file_path}. Error: {e$message}"))
    return(invisible(NULL))
  })
}

#' Clear DataFindR Cache within a metawoRld Project
#'
#' Removes cached files for a specific identifier or the entire cache.
#' Note: If using the Shiny applications, they might hold some data in memory; restarting the Shiny app might be necessary to fully reflect cache clearing.
#'
#' @param identifier The original identifier (e.g., DOI) to clear cache for.
#'   If NULL (default), clears the *entire* DataFindR cache for the project.
#'   Use with caution!
#' @param type Cache type(s) to clear: "assessment", "extraction", "metadata",
#'   or a vector like `c("assessment", "extraction")`. If NULL (default), clears
#'   all types for the given identifier(s).
#' @param metawoRld_path Path to the metawoRld project containing the cache.
#' @return Invisibly returns TRUE if deletion operations were attempted
#'  (doesn't guarantee success for every file if permissions fail).
#' @export
#' @importFrom fs dir_exists dir_delete file_delete file_exists path
#' @importFrom rlang check_installed %||% warn inform abort is_null is_character
#' @importFrom metawoRld .sanitize_id
df_clear_cache <- function(identifier = NULL, type = NULL, metawoRld_path) {

  cache_dir <- .get_datafindr_cache_path(metawoRld_path, create = FALSE)

  if (!fs::dir_exists(cache_dir)) {
    rlang::inform("Cache directory does not exist. Nothing to clear.")
    return(invisible(TRUE))
  }

  valid_types <- c("assessment", "extraction", "metadata")
  types_to_clear <- type %||% valid_types
  if (!all(types_to_clear %in% valid_types)) {
    rlang::abort(glue::glue("Invalid cache type specified. Must be one or more of: {paste(valid_types, collapse=', ')}"))
  }

  if (rlang::is_null(identifier)) {
    # Clear entire DataFindR cache
    rlang::check_installed("utils", reason = "to ask for user confirmation before deleting the entire cache.")
    user_consent <- utils::askYesNo(
      glue::glue("Are you sure you want to delete the ENTIRE DataFindR cache in '{cache_dir}'?"),
      default = FALSE
    )
    if (isTRUE(user_consent)) {
      rlang::inform(glue::glue("Deleting directory: {cache_dir}"))
      fs::dir_delete(cache_dir)
    } else {
      rlang::inform("Cache clearing aborted by user.")
    }
  } else if (rlang::is_character(identifier)) {
    # Clear cache for specific identifier(s)
    sanitized_ids <- vapply(identifier, metawoRld::.sanitize_id, character(1))
    files_deleted_count <- 0
    for (s_id in sanitized_ids) {
      if (s_id == "") next # Skip empty
      for (t in types_to_clear) {
        filename <- switch(t,
                           assessment = paste0(s_id, "_assessment.json"),
                           extraction = paste0(s_id, "_extraction.json"),
                           metadata = paste0(s_id, "_metadata.json")
        )
        file_path <- fs::path(cache_dir, t, filename)
        if (fs::file_exists(file_path)) {
          fs::file_delete(file_path)
          files_deleted_count <- files_deleted_count + 1
          # rlang::inform(glue::glue("Deleted: {file_path}")) # Can be noisy
        }
      }
    }
    rlang::inform(glue::glue("Deleted {files_deleted_count} cache file(s) for the specified identifier(s)."))

  } else {
    rlang::abort("`identifier` must be NULL or a character vector.")
  }

  invisible(TRUE)
}

#' Find Studies Pending Extraction
#'
#' Scans the assessment cache and compares it against the extraction cache
#' within a metawoRld project to find studies marked for inclusion that
#' haven't been extracted yet.
#'
#' @param metawoRld_path Path to the root of the metawoRld project.
#' @param decision_threshold Character vector. Assessment decisions that trigger
#'   inclusion for extraction (e.g., c("Include", "Needs Manual Review")).
#'   Case-sensitive.
#'
#' @return A character vector of original study identifiers (DOIs/PMIDs)
#'   pending extraction, or an empty vector if none are found or on error.
#' @noRd
#' @keywords internal
#'
#' @importFrom fs dir_ls file_exists path path_file
#' @importFrom jsonlite fromJSON
#' @importFrom rlang warn inform is_list `%||%`
#' @importFrom purrr map map_lgl keep discard set_names
#' @importFrom tools file_path_sans_ext
#' @importFrom metawoRld .desanitize_id .sanitize_id
.find_pending_extraction_studies <- function(metawoRld_path,
                                             decision_threshold = c("Include")) {

  # --- START temporary copy of sanitizers ---
  .sanitize_id <- function(id) {
    if (is.null(id) || id == "") return("")
    id <- gsub("/", "_fslash_", id, fixed = TRUE)
    id <- gsub(":", "_colon_", id, fixed = TRUE)
    id <- gsub("\\?", "_qmark_", id, fixed = TRUE)
    id <- gsub("\\*", "_star_", id, fixed = TRUE)
    id <- gsub("<", "_lt_", id, fixed = TRUE)
    id <- gsub(">", "_gt_", id, fixed = TRUE)
    id <- gsub("\\|", "_pipe_", id, fixed = TRUE)
    id <- gsub("\"", "_quote_", id, fixed = TRUE)
    return(id)
  }
  .desanitize_id <- function(sanitized_id) {
    if (is.null(sanitized_id) || sanitized_id == "") return("")
    id <- sanitized_id
    id <- gsub("_quote_", "\"", id, fixed = TRUE)
    id <- gsub("_pipe_", "|", id, fixed = TRUE)
    id <- gsub("_gt_", ">", id, fixed = TRUE)
    id <- gsub("_lt_", "<", id, fixed = TRUE)
    id <- gsub("_star_", "*", id, fixed = TRUE)
    id <- gsub("_qmark_", "?", id, fixed = TRUE)
    id <- gsub("_colon_", ":", id, fixed = TRUE)
    id <- gsub("_fslash_", "/", id, fixed = TRUE)
    return(id)
  }
  # --- END temporary copy ---

  cache_dir <- .get_datafindr_cache_path(metawoRld_path, create = FALSE)
  assessment_cache_dir <- fs::path(cache_dir, "assessment")
  extraction_cache_dir <- fs::path(cache_dir, "extraction")

  if (!fs::dir_exists(assessment_cache_dir)) {
    rlang::inform("Assessment cache directory not found. Cannot determine pending studies.")
    return(character(0))
  }
  # Extraction dir might not exist yet, that's okay

  assessment_files <- fs::dir_ls(assessment_cache_dir, glob = "*_assessment.json")
  if (length(assessment_files) == 0) {
    return(character(0))
  }

  # Get sanitized IDs from assessment files
  sanitized_ids_assessed <- gsub("_assessment\\.json$", "", fs::path_file(assessment_files))

  # Read assessment files and filter based on decision
  included_sanitized_ids <- purrr::map(assessment_files, ~{
    res <- tryCatch(jsonlite::fromJSON(., simplifyVector = FALSE), error = function(e) NULL)
    if (rlang::is_list(res) && !is.null(res$decision) && res$decision %in% decision_threshold) {
      # Return the sanitized ID from the filename
      gsub("_assessment\\.json$", "", fs::path_file(.))
    } else {
      NULL
    }
  }) |> purrr::compact() |> unlist()


  if (length(included_sanitized_ids) == 0) {
    return(character(0)) # No studies met the decision threshold
  }

  # Check which of these already have an extraction file
  pending_sanitized_ids <- purrr::keep(included_sanitized_ids, ~{
    extraction_filename <- paste0(., "_extraction.json")
    extraction_filepath <- fs::path(extraction_cache_dir, extraction_filename)
    !fs::file_exists(extraction_filepath) # Keep if extraction file DOES NOT exist
  })

  # Convert back to original identifiers
  pending_original_ids <- vapply(pending_sanitized_ids, .desanitize_id, character(1))

  return(pending_original_ids)
}

#' Read all datafindr cache files from a metawoRld project
#'
#' Scans the datafindr cache directory within a metawoRld project, reads all
#' valid cached JSON files (assessment, extraction, metadata), parses them,
#' and returns them organized in a nested list.
#'
#' The function identifies files matching the pattern
#' `<sanitized_id>_<type>.json` within the `assessment`, `extraction`, and
#' `metadata` subdirectories of the datafindr cache. It attempts to parse each
#' valid file as JSON. Files that cannot be parsed or do not match the naming
#' convention are skipped with a warning.
#'
#' @param metawoRld_path The path to the metawoRld project directory, which
#'   contains the datafindr cache subdirectory.
#'
#' @return A named list. The top-level names are the sanitized identifiers found
#'   in the cache. Each element is itself a list containing the parsed data,
#'   named by the type ("assessment", "extraction", "metadata"). Returns an
#'   empty list (`list()`) if the cache directory doesn't exist or no valid
#'   cache files are found.
#'
#' @export
#' @importFrom rlang warn inform
#' @importFrom fs dir_exists dir_ls path path_file path_ext_remove path_rel
#' @importFrom glue glue
#' @importFrom jsonlite fromJSON
#' @importFrom stringr str_match
#' @importFrom utils readLines
read_all_from_cache <- function(type = c("assessment", "extraction", "metadata"), metawoRld_path) {

  # Get the base cache directory path, don't create it
  cache_dir <- .get_datafindr_cache_path(metawoRld_path, create = FALSE)
  type <- rlang::arg_match(type)

  # Check if the base cache directory exists
  if (!fs::dir_exists(cache_dir)) {
    rlang::inform(glue::glue("Cache directory does not exist: {fs::path_rel(cache_dir, start = metawoRld_path)}"))
    return(list()) # Return empty list if cache dir not found
  }

  # List all files recursively within the cache directory
  # Only consider files within the expected type subdirectories
  # Using tryCatch around dir_ls in case of permission issues, though less likely
  all_files <- tryCatch({
    fs::dir_ls(
      path = fs::path(cache_dir, type),
      type = "file", # Only list files
      recurse = TRUE, # Search subdirectories if needed (though structure is flat within type dirs)
      fail = FALSE # Don't error if one of the type dirs doesn't exist
    )
  }, error = function(e) {
    rlang::warn(glue::glue("Error listing files in cache directory '{cache_dir}': {e$message}"))
    return(character(0)) # Return empty character vector on error
  })


  if (length(all_files) == 0) {
    rlang::inform(glue::glue("No files found in cache subdirectories: {fs::path_rel(cache_dir, start = metawoRld_path)}"))
    return(list())
  }

  # Initialize the results list
  cached_data <- list()
  files_processed <- 0
  files_failed <- 0

  # Regex to extract sanitized ID and type from filename
  # Matches: (anything)_ (assessment|extraction|metadata) .json
  filename_pattern <- paste0("^(.*?)_", type, "\\.json$")

  # Iterate through potential cache files
  for (cache_file_path in all_files) {
    filename <- fs::path_file(cache_file_path)

    # Attempt to match the filename pattern
    match_result <- stringr::str_match(filename, filename_pattern)

    # Check if the filename matches the expected pattern
    if (is.na(match_result[1, 1])) {
      # Optional: Warn about files that don't match the pattern
      # rlang::warn(glue::glue("Skipping file with unexpected name format: {filename}"))
      next # Skip to the next file
    }

    sanitized_id <- match_result[1, 2]

    # Try to read and parse the file
    tryCatch({
      # Read the JSON string(s) from the file
      json_string_lines <- readLines(con = cache_file_path, warn = FALSE)

      if (length(json_string_lines) == 0) {
        rlang::warn(glue::glue("Cache file is empty, skipping: {fs::path_rel(cache_file_path, start = metawoRld_path)}"))
        files_failed <- files_failed + 1
        next # Skip empty file
      }

      # Collapse lines into a single string
      json_string <- paste(json_string_lines, collapse = "\n")

      # Deserialize JSON to R object
      data <- jsonlite::fromJSON(json_string, simplifyVector = TRUE)

      # Store the data in the nested list structure
      # Initialize the identifier level if it doesn't exist
      if (is.null(cached_data[[sanitized_id]])) {
        cached_data[[sanitized_id]] <- list()
      }
      # Assign the data to the correct type
      cached_data[[sanitized_id]][[type]] <- data
      files_processed <- files_processed + 1

    }, error = function(e) {
      # Handle errors during reading or parsing for this specific file
      files_failed <- files_failed + 1
      rlang::warn(glue::glue(
        "Failed to read or parse cache file: {fs::path_rel(cache_file_path, start = metawoRld_path)}. Error: {e$message}"
      ))
      # Do not add this file's data to the results
    }) # End tryCatch for file processing
  } # End for loop

  rlang::inform(glue::glue("Read {files_processed} file(s) successfully from cache. Skipped/failed {files_failed} file(s)."))

  return(cached_data)
}
