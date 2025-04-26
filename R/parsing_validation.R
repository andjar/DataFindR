#' Validate Parsed LLM Extraction JSON Against Schema
#'
#' Checks if the parsed list object from the LLM's extraction JSON conforms
#' structurally to the project's schema and internal linkage rules.
#'
#' @param extracted_data List. The parsed R list object from the extraction JSON.
#'   Expected to have `metadata` and `data_points` top-level keys.
#' @param schema List. The project schema obtained via `metawoRld::get_schema()`.
#' @param identifier Character string. The study identifier (for error messages).
#'
#' @return Invisibly returns `TRUE` if validation passes. Aborts with an
#'   informative error message if validation fails.
#' @noRd
#' @keywords internal
#'
#' @importFrom rlang is_list `%||%` abort inform glue
#' @importFrom purrr map map_lgl every
.validate_extracted_data <- function(extracted_data, schema, identifier) {

  rlang::inform(glue::glue("Validating extracted data structure for '{identifier}'..."))

  # --- Basic Structure Checks ---
  if (!rlang::is_list(extracted_data)) {
    rlang::abort(glue::glue("Validation Error ({identifier}): Extracted data is not a list."))
  }
  if (!all(c("metadata", "data_points") %in% names(extracted_data))) {
    rlang::abort(glue::glue("Validation Error ({identifier}): Extracted data missing 'metadata' or 'data_points' top-level keys."))
  }
  if (!rlang::is_list(extracted_data$metadata)) {
    rlang::abort(glue::glue("Validation Error ({identifier}): 'metadata' section is not a list."))
  }
  if (!rlang::is_list(extracted_data$data_points)) {
    # Allow empty list if no data points were found, but check individual items later
    if (length(extracted_data$data_points) > 0 && !all(purrr::map_lgl(extracted_data$data_points, rlang::is_list))) {
      rlang::abort(glue::glue("Validation Error ({identifier}): 'data_points' section is not a list of lists."))
    }
  }

  # --- Metadata Field Checks ---
  meta_fields_def <- schema$metadata_fields %||% list()
  req_meta <- meta_fields_def$required %||% character()
  provided_meta <- names(extracted_data$metadata)
  missing_req_meta <- setdiff(req_meta, provided_meta)
  if (length(missing_req_meta) > 0) {
    rlang::abort(glue::glue("Validation Error ({identifier}): Metadata section missing required fields: {paste(missing_req_meta, collapse=', ')}"))
  }
  # Check if study_id is present and non-empty (needed later)
  if(!"study_id" %in% req_meta || is.null(extracted_data$metadata$study_id) || extracted_data$metadata$study_id == ""){
    rlang::abort(glue::glue("Validation Error ({identifier}): Required field 'study_id' is missing or empty in the extracted metadata."))
  }

  # --- Data Point Field Checks ---
  data_fields_def <- schema$data_fields %||% list()
  req_data <- data_fields_def$required %||% character(0)

  if (length(extracted_data$data_points) > 0) {
    # Check fields for each data point item
    all_items_valid <- purrr::every(seq_along(extracted_data$data_points), ~{
      item <- extracted_data$data_points[[.x]]
      if (!rlang::is_list(item)) return(FALSE) # Already checked above, but be safe
      provided_data <- names(item)
      missing_req_data <- setdiff(req_data, provided_data)
      if (length(missing_req_data) > 0) {
        rlang::abort(glue::glue("Validation Error ({identifier}): Data point #{.x} missing required fields: {paste(missing_req_data, collapse=', ')}"))
        # return(FALSE) # Not reachable due to abort
      }
      return(TRUE)
    })
    # if (!all_items_valid) { } # Not reachable due to abort
  } else {
    rlang::inform(glue::glue("Validation Info ({identifier}): 'data_points' array is empty. No quantitative data found or extracted."))
  }

  # --- Linkage Checks ---
  rlang::inform(glue::glue("Validating linkages for '{identifier}'..."))
  # 1. Check if required structures for linkage exist
  if (!"measurement_methods" %in% names(extracted_data$metadata) || !rlang::is_list(extracted_data$metadata$measurement_methods)) {
    rlang::abort(glue::glue("Validation Error ({identifier}): Metadata is missing 'measurement_methods' list, needed for linkage."))
  }
  if (!"outcome_groups" %in% names(extracted_data$metadata) || !rlang::is_list(extracted_data$metadata$outcome_groups)) {
    rlang::abort(glue::glue("Validation Error ({identifier}): Metadata is missing 'outcome_groups' list, needed for linkage."))
  }

  # Only proceed if data points exist
  if (length(extracted_data$data_points) > 0) {
    valid_method_ids <- names(extracted_data$metadata$measurement_methods) %||% character(0)
    valid_group_labels <- names(extracted_data$metadata$outcome_groups) %||% character(0)

    if(length(valid_method_ids) == 0 && any(purrr::map_lgl(extracted_data$data_points, ~!is.null(.x$method_ref_id)))){
      rlang::warn(glue::glue("Validation Warning ({identifier}): No methods defined in metadata$measurement_methods, but data points have method_ref_id."))
      # Allow to proceed, but linkage is broken. Or abort? Let's abort for strictness.
      rlang::abort(glue::glue("Validation Error ({identifier}): No methods defined in metadata$measurement_methods keys, but 'method_ref_id' is present in data_points."))

    }
    if(length(valid_group_labels) == 0 && any(purrr::map_lgl(extracted_data$data_points, ~!is.null(.x$group_label)))){
      rlang::abort(glue::glue("Validation Error ({identifier}): No groups defined in metadata$outcome_groups keys, but 'group_label' is present in data_points."))
    }

    # Check linkages for each data point item
    linkage_check_passed <- purrr::every(seq_along(extracted_data$data_points), ~{
      item <- extracted_data$data_points[[.x]]
      item_valid <- TRUE

      # Check method_ref_id
      method_ref <- item$method_ref_id %||% NA_character_
      if (!is.na(method_ref) && !(method_ref %in% valid_method_ids)) {
        rlang::abort(glue::glue("Validation Error ({identifier}): Data point #{.x} has 'method_ref_id' ('{method_ref}') not found in metadata$measurement_methods keys: {paste(valid_method_ids, collapse=', ')}"))
        item_valid <- FALSE
      }

      # Check group_label
      group_ref <- item$group_label %||% NA_character_
      if (!is.na(group_ref) && !(group_ref %in% valid_group_labels)) {
        rlang::abort(glue::glue("Validation Error ({identifier}): Data point #{.x} has 'group_label' ('{group_ref}') not found in metadata$outcome_groups keys: {paste(valid_group_labels, collapse=', ')}"))
        item_valid <- FALSE
      }

      # Check comparison_group_label (if present)
      # comp_group_ref <- item$comparison_group_label %||% NA_character_
      # if (!is.na(comp_group_ref) && !(comp_group_ref %in% valid_group_labels)) {
      #   rlang::warn(glue::glue("Validation Warning ({identifier}): Data point #{.x} has 'comparison_group_label' ('{comp_group_ref}') not found in metadata$outcome_groups keys: {paste(valid_group_labels, collapse=', ')}"))
      #   # Don't abort for optional field link failure, just warn.
      #   # item_valid <- FALSE
      # }
      item_valid # Return status for this item (currently always TRUE unless abort occurs)
    })
    # if (!linkage_check_passed) { } # Not reachable due to aborts

  }

  rlang::inform(glue::glue("Validation checks passed for '{identifier}'."))
  invisible(TRUE)
}
