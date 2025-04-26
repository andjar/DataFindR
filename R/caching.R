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
