#' Parsing and Validation Utilities
#'
#' This file contains utility functions for parsing schema definitions and validating
#' data structures, particularly for data extracted by LLMs against these schemas.
#' Validate extracted data against a schema definition (list format).
#'
#' Checks if the structure, types, and required fields of the extracted data
#' conform to the rules defined in the schema list (parsed from YAML).
#'
#' @param extracted_data The list/data frame structure returned by `ellmer::extract_data`.
#' @param schema The schema definition as an R list, typically loaded from YAML
#'   using a function like `get_schema`.
#'
#' @return A list of validation error messages. An empty list indicates successful
#'   validation. Each error message includes the path to the problematic node.
#' @noRd
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Assume schema_list is loaded from YAML using get_schema()
#' # Assume llm_output is the result from chat$extract_data(..., type = ellmer_schema)
#'
#' schema_definition <- get_schema(".") # Or path to your schema YAML
#' # llm_output <- chat$extract_data(...) # Run your extraction
#'
#' validation_errors <- validate_extracted_data(llm_output, schema_definition)
#'
#' if (length(validation_errors) == 0) {
#'   print("Validation successful!")
#' } else {
#'   print("Validation failed with the following errors:")
#'   purrr::walk(validation_errors, print) # Requires purrr package
#'   # Or use base R:
#'   # invisible(lapply(validation_errors, print))
#' }
#' }
.validate_extracted_data <- function(extracted_data, schema) {

  if (!rlang::is_list(schema)) {
    stop("`schema` must be a list (typically parsed from YAML).", call. = FALSE)
  }
  # Extracted data can be a list or potentially a data frame at the top level
  # if the schema defines an array of objects. We'll check specific types inside.

  errors <- list()

  # --- Recursive Helper Function ---
  validate_node <- function(data_node, schema_node, path = "root") {

    node_errors <- list()

    # --- Check NULL data ---
    # If data is NULL, it's only valid if the schema node is NOT required.
    if (is.null(data_node) || rlang::is_empty(data_node)) {
      is_required <- !identical(schema_node$`_required`, FALSE) # Default required = TRUE
      if (is_required) {
        node_errors <- c(node_errors, paste0(path, ": Is required but data is NULL."))
      }
      # If optional and NULL, it's valid, so return empty errors for this node
      return(node_errors)
    }

    # --- Check Schema Type Exists ---
    if (is.null(schema_node$`_type`)) {
      # This indicates an issue with the schema itself, but we note it here.
      # Should ideally be caught when parsing YAML to ellmer type.
      node_errors <- c(node_errors, paste0(path, ": Schema definition is missing '_type' key."))
      return(node_errors) # Cannot proceed without type info
    }

    schema_type <- schema_node$`_type`

    # --- Type-Specific Validation ---
    switch(
      schema_type,
      "object" = {
        # Expecting a list (or potentially a single row data frame treated as list)
        if (!rlang::is_list(data_node) && !(is.data.frame(data_node) && nrow(data_node) == 1)) {
          node_errors <- c(node_errors, paste0(path, ": Expected an object (list/named list), but got ", class(data_node)[1], "."))
          return(node_errors) # Cannot proceed if not list-like
        }
        # Coerce single row data frame to list for easier field access
        if (is.data.frame(data_node) && nrow(data_node) == 1) {
          data_node <- as.list(data_node)
        }


        schema_fields <- setdiff(names(schema_node), grep("^_", names(schema_node), value = TRUE))
        data_fields <- names(data_node)

        # Check Required Fields
        for (field in schema_fields) {
          field_schema <- schema_node[[field]]
          is_required <- !identical(field_schema$`_required`, FALSE) # Defaults to TRUE
          if (is_required && !(field %in% data_fields)) {
            node_errors <- c(node_errors, paste0(path, ".", field, ": Required field is missing."))
          }
        }

        # Validate Present Fields
        for (field in data_fields) {
          if (field %in% schema_fields) {
            field_schema <- schema_node[[field]]
            field_data <- data_node[[field]]
            field_path <- paste0(path, ".", field)
            # Recursive call
            node_errors <- c(node_errors, validate_node(field_data, field_schema, field_path))
          } else {
            # Optional: Warn about fields present in data but not schema
            # node_errors <- c(node_errors, paste0(path, ".", field, ": Field present in data but not defined in schema."))
          }
        }
      }, # End object case

      "array" = {
        items_schema <- schema_node$`_items`
        if (is.null(items_schema)) {
          node_errors <- c(node_errors, paste0(path, ": Array schema definition missing '_items' key."))
          return(node_errors)
        }

        # Handle vectors, lists, and data frames (common outputs from ellmer)
        if (is.data.frame(data_node)) {
          # Iterate through rows
          if (nrow(data_node) > 0) {
            for (i in 1:nrow(data_node)) {
              # Convert row to list for consistent validation with object items
              # Use drop=FALSE defensively if validation expects list-like rows
              row_data <- tryCatch(as.list(data_node[i, , drop = FALSE]), error = function(e) data_node[i,]) # Fallback for simple vectors
              if(is.data.frame(row_data) && ncol(row_data) == 1) row_data <- as.list(row_data) # Ensure it becomes a list if single column df row

              item_path <- paste0(path, "[", i, "]")
              node_errors <- c(node_errors, validate_node(row_data, items_schema, item_path))
            }
          } # else: empty data frame is valid for an array
        } else if (rlang::is_list(data_node) || is.atomic(data_node)) {
          # Iterate through list elements or vector elements
          if (length(data_node) > 0) {
            for (i in 1:length(data_node)) {
              item_data <- data_node[[i]]
              item_path <- paste0(path, "[", i, "]")
              node_errors <- c(node_errors, validate_node(item_data, items_schema, item_path))
            }
          } # else: empty list/vector is valid
        } else {
          node_errors <- c(node_errors, paste0(path, ": Expected an array (vector, list, or data.frame), but got ", class(data_node)[1], "."))
        }
      }, # End array case

      "string" = {
        if (!rlang::is_scalar_character(data_node)) {
          node_errors <- c(node_errors, paste0(path, ": Expected a single string (character), but got ", class(data_node)[1], "."))
        }
      },

      "integer" = {
        # Allow numeric values that are whole numbers
        is_valid_integer <- rlang::is_scalar_integer(data_node) ||
          (rlang::is_scalar_double(data_node) && !is.na(data_node) && data_node == floor(data_node))
        if (!is_valid_integer) {
          node_errors <- c(node_errors, paste0(path, ": Expected a single integer, but got ", class(data_node)[1], " with value '", data_node, "'."))
        }
      },

      "number" = {
        # Allows integer or double
        if (!rlang::is_scalar_double(data_node) && !rlang::is_scalar_integer(data_node)) {
          node_errors <- c(node_errors, paste0(path, ": Expected a single number (numeric/double/integer), but got ", class(data_node)[1], "."))
        }
      },

      "boolean" = {
        if (!rlang::is_scalar_logical(data_node)) {
          node_errors <- c(node_errors, paste0(path, ": Expected a single boolean (logical), but got ", class(data_node)[1], "."))
        }
      },

      "enum" = {
        allowed_values <- schema_node$`_values`
        if (is.null(allowed_values)) {
          node_errors <- c(node_errors, paste0(path, ": Enum schema definition missing '_values' key."))
        } else if (!rlang::is_scalar_character(data_node)) {
          node_errors <- c(node_errors, paste0(path, ": Expected a single string for enum value, but got ", class(data_node)[1], "."))
        } else if (!(data_node %in% allowed_values)) {
          node_errors <- c(node_errors, paste0(path, ": Value '", data_node, "' is not in the allowed enum values: [", paste(allowed_values, collapse=", "), "]."))
        }
      },

      # Default case for unknown schema types
      {
        node_errors <- c(node_errors, paste0(path, ": Unknown schema type '", schema_type, "' encountered."))
      }
    ) # End switch

    return(node_errors)

  } # --- End validate_node Helper ---


  # --- Start Validation from Root ---
  all_errors <- validate_node(extracted_data, schema, path = "root")

  return(all_errors)
}
