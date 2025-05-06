#' Generate Assessment Prompt
#'
#' Constructs a formatted prompt string for assessing a study based on its
#' title, abstract, and inclusion/exclusion criteria, using a provided template.
#' Intended for use with Large Language Models or similar assessment systems.
#'
#' @param title Character string. The title of the study. Handles `NULL` or
#'   empty strings by substituting "Title not available".
#' @param abstract Character string. The abstract of the study. Handles `NULL` or
#'   empty strings by substituting "Abstract not available".
#' @param inclusion_criteria Character vector. A vector where each element is
#'   an inclusion criterion. These will be formatted as a bulleted list.
#' @param exclusion_criteria Character vector. A vector where each element is
#'   an exclusion criterion. These will be formatted as a bulleted list.
#' @param prompt_template Character string. The template for the prompt. It should
#'   contain placeholders like `{{title}}`, `{{abstract}}`, `{{incl_crit_str}}`,
#'   and `{{excl_crit_str}}` which will be replaced by the corresponding
#'   arguments. Defaults to reading the content of the
#'   `assessment_prompt.txt` file within the package's `inst/prompts` directory.
#'
#' @return A single character string representing the fully formatted prompt.
#'
#' @keywords internal
#' @noRd
#' @importFrom readr read_file
#' @importFrom fs path
#' @importFrom glue glue
#' @importFrom rlang `%||%`
.generate_assessment_prompt <- function(
    title,
    abstract,
    inclusion_criteria,
    exclusion_criteria,
    prompt_template = readr::read_file(
        system.file(fs::path("prompts", "_assessment_prompt.txt"), package = "DataFindR")
      )
    ) {

  # Format criteria for the prompt
  incl_crit_str <- paste0("- ", inclusion_criteria, collapse = "\n")
  excl_crit_str <- paste0("- ", exclusion_criteria, collapse = "\n")

  # Ensure title/abstract are handled if NA/empty
  title <- title %||% "Title not available"
  abstract <- abstract %||% "Abstract not available"

  prompt <- glue::glue(
    prompt_template,
    .open = "{{", .close = "}}"
  )

  return(prompt)
}

#' Generate LLM Prompt for Extracting Study Data
#'
#' Creates a detailed prompt for an LLM based on the schema defined in a
#' metawoRld project, guiding the extraction of data primarily from the
#' Methods, Results, and Tables/Figures of a full research paper.
#' It fetches the schema directly from the specified project path.
#'
#' @param metawoRld_path Character string. Path to the root of the metawoRld
#'   project containing the `_metawoRld.yml` file with the schema.
#' @param identifier Character string (optional). The study identifier (DOI/PMID)
#'   to potentially pre-fill or reference in the prompt. Can be used as the default `study_id`.
#' @param fetched_metadata List (optional). Pre-fetched metadata (e.g., from
#'   `df_fetch_metadata`) containing fields like title, authors, year etc.
#'   If provided, the prompt can instruct the LLM to focus less on these.
#'
#' @return Character string. The formatted LLM prompt for data extraction.
#' @export
#' @importFrom glue glue
#' @importFrom rlang `%||%` is_list abort is_scalar_character is_null
#' @importFrom jsonlite toJSON
#' @importFrom fs path_norm dir_exists file_exists path
#' @import metawoRld
#'
#' @examples
#' \dontrun{
#' # --- Setup: Create a dummy metawoRld project ---
#' proj_path <- file.path(tempdir(), "prompt_test_proj_full")
#' metawoRld::create_metawoRld(
#'    proj_path,
#'    project_name = "Prompt Generation Test Full",
#'    project_description = "Testing prompt generation",
#'    inclusion_criteria = c("Human", "Pregnancy"),
#'    exclusion_criteria = c("Animal")
#'    # Assuming default schema is created inside
#' )
#' doi <- "10.1234/test.doi.full"
#'
#' # Example with pre-fetched metadata
#' fetched_meta_example <- list(
#'     identifier = doi, type = "doi", title = "Pre-Fetched Title",
#'     authors = list("Smith J", "Doe A"), year = 2023, journal = "Fetched Journal",
#'     abstract = "Fetched abstract text..."
#' )
#'
#' # --- Generate the prompt (with fetched metadata hint) ---
#' extraction_prompt_with_meta <- df_generate_extraction_prompt(
#'     metawoRld_path = proj_path,
#'     identifier = doi,
#'     fetched_metadata = fetched_meta_example
#' )
#' cat("--- Prompt with Fetched Metadata Hint --- \n")
#' # cat(extraction_prompt_with_meta) # View the generated prompt
#'
#' # --- Generate the prompt (without fetched metadata hint) ---
#' extraction_prompt_no_meta <- df_generate_extraction_prompt(
#'     metawoRld_path = proj_path,
#'     identifier = doi,
#'     fetched_metadata = NULL # Explicitly NULL
#' )
#' cat("\n--- Prompt without Fetched Metadata Hint --- \n")
#' # cat(extraction_prompt_no_meta) # View the generated prompt
#'
#' # --- Clean up ---
#' unlink(proj_path, recursive = TRUE)
#' }
.generate_extraction_prompt <- function(
    identifier = NULL,
    fetched_metadata = NULL,
    prompt_template = readr::read_file(
      system.file(fs::path("prompts", "_extraction_prompt.txt"), package = "DataFindR")
    )
    ) {

  # --- Construct the Prompt ---
  prompt <- glue::glue(
    prompt_template
    , .open = "{{", .close = "}}") # Use different delimiters for glue

  return(prompt)
}

#' Parse a YAML file defining a schema into an ellmer type object structure.
#'
#' @param yaml_path Path to the YAML file containing the schema definition.
#'
#' @return An ellmer type object (e.g., type_object, type_array) representing
#'         the schema.
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming 'schema.yaml' exists and contains the schema definition
#' ellmer_schema <- parse_yaml_to_ellmer_schema("schema.yaml")
#'
#' # You can then use this schema with ellmer::extract_data
#' # chat <- ellmer::chat_openai()
#' # extracted_data <- chat$extract_data(paper_text, type = ellmer_schema)
#' }
parse_yaml_to_ellmer_schema <- function(yaml_path) {
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Package 'yaml' is required. Please install it.", call. = FALSE)
  }
  if (!requireNamespace("ellmer", quietly = TRUE)) {
    stop("Package 'ellmer' is required. Please install it.", call. = FALSE)
  }

  schema_list <- yaml::read_yaml(yaml_path)

  # Recursive helper function to convert list nodes to ellmer types
  convert_node <- function(node) {
    if (!is.list(node) || is.null(node$`_type`)) {
      stop("Invalid node structure in YAML. Each node must be a list with a '_type' key.", call. = FALSE)
    }

    type <- node$`_type`
    description <- node$`_description` # Can be NULL
    # ellmer defaults to required = TRUE. Only set required = FALSE if explicitly stated in YAML.
    required <- !identical(node$`_required`, FALSE) # TRUE if missing or anything other than FALSE

    switch(
      type,
      "object" = {
        # Get field names (all keys except the special ones starting with '_')
        field_names <- setdiff(names(node), grep("^_", names(node), value = TRUE))
        if (length(field_names) == 0 && !is.null(node$`_additional_properties`) && node$`_additional_properties`) {
          fields_list <- list()
        } else {
          # Recursively convert each field definition
          fields_list <- stats::setNames(
            lapply(field_names, function(name) convert_node(node[[name]])),
            field_names
          )
        }

        # Use do.call to handle NULL description cleanly
        args <- list(
          .description = description,
          .required = required
        )
        # Add fields list as named arguments to the list passed to do.call
        args <- c(args, fields_list)
        do.call(ellmer::type_object, args)
      },
      "array" = {
        if (is.null(node$`_items`)) {
          stop("Array type definition missing '_items' key.", call. = FALSE)
        }
        items_type <- convert_node(node$`_items`)
        ellmer::type_array(description = description, items = items_type, required = required)
      },
      "string" = {
        ellmer::type_string(description = description, required = required)
      },
      "integer" = {
        ellmer::type_integer(description = description, required = required)
      },
      "number" = {
        ellmer::type_number(description = description, required = required)
      },
      "boolean" = {
        ellmer::type_boolean(description = description, required = required)
      },
      "enum" = {
        if (is.null(node$`_values`)) {
          stop("Enum type definition missing '_values' key.", call. = FALSE)
        }
        ellmer::type_enum(description = description, values = node$`_values`, required = required)
      },
      # Default case for unknown types
      stop(paste("Unsupported type:", type), call. = FALSE)
    )
  }

  # Start conversion from the root node
  convert_node(schema_list)
}
