#' Generate LLM Prompt for Assessing Study Relevance
#'
#' @param title Character string. Study title.
#' @param abstract Character string. Study abstract.
#' @param inclusion_criteria Character vector. List of inclusion criteria.
#' @param exclusion_criteria Character vector. List of exclusion criteria.
#'
#' @return A character string containing the formatted prompt.
#' @noRd
#' @keywords internal
#' @importFrom glue glue
.generate_assessment_prompt <- function(title, abstract, inclusion_criteria, exclusion_criteria) {

  # Format criteria for the prompt
  incl_crit_str <- paste0("- ", inclusion_criteria, collapse = "\n")
  excl_crit_str <- paste0("- ", exclusion_criteria, collapse = "\n")

  # Ensure title/abstract are handled if NA/empty
  title <- title %||% "Title not available"
  abstract <- abstract %||% "Abstract not available"

  prompt <- glue::glue(
'**Role:** You are a scientific literature screening assistant for a systematic review project.

**Task:** Carefully evaluate the provided study Title and Abstract against the project\'s Inclusion and Exclusion criteria. Based ONLY on the Title and Abstract, determine if this study is likely relevant and should be included for full-text review and data extraction. Provide your assessment in a specific JSON format.

**Input Data:**
*   **Title:** {title}
*   **Abstract:** {abstract}

**Project Criteria:**
*   **Inclusion Criteria (Study MUST meet these):**
{incl_crit_str}

*   **Exclusion Criteria (Study MUST NOT meet these):**
{excl_crit_str}

**Output Format:**
Respond with ONLY a single, valid JSON object with the following keys:
*   `"decision"`: (string) Your assessment category. Choose ONE of:
    *   `"Include"`: High confidence the study meets inclusion criteria and avoids exclusion criteria based on Title/Abstract.
    *   `"Exclude"`: High confidence the study meets one or more exclusion criteria OR clearly fails critical inclusion criteria based on Title/Abstract.
    *   `"Needs Manual Review"`: Unclear from Title/Abstract whether criteria are met (e.g., mentions relevant concepts but lacks specific details, borderline case).
*   `"score"`: (number) Your confidence score (0.0 to 1.0) reflecting the likelihood of inclusion. Examples: ~0.9+ for "Include", ~0.1- for "Exclude", 0.3-0.7 for "Needs Manual Review".
*   `"rationale"`: (string) A BRIEF explanation (1-2 sentences) justifying your decision and score, referencing specific criteria or information (or lack thereof) in the Title/Abstract.

**Instructions:**
*   Base your decision solely on the Title and Abstract provided. Do not assume information not present.
*   If the abstract is missing or very short, lean towards "Needs Manual Review" unless the title is definitively exclusionary.
*   Be critical - if key inclusion criteria details (like sample type or outcome) are absent, do not automatically assume "Include".
*   Provide ONLY the JSON object in your response, without any introductory text or markdown formatting like json.

```json
{{
  "decision": "Include | Exclude | Needs Manual Review",
  "score": 0.0_to_1.0,
  "rationale": "Brief justification..."
}}
```'
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
#' @importFrom jsonlite toJSON # Used implicitly by glue here, but good practice
#' @importFrom fs path_norm dir_exists file_exists path
#' @import metawoRld # Import the package to use get_schema
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
df_generate_extraction_prompt <- function(metawoRld_path, identifier = NULL, fetched_metadata = NULL) {

  # --- Validate Path and Get Schema ---
  if (!rlang::is_scalar_character(metawoRld_path)) {
    rlang::abort("`metawoRld_path` must be a character string.")
  }
  proj_path <- fs::path_norm(metawoRld_path)
  if (!fs::dir_exists(proj_path)) {
    rlang::abort(glue::glue("metawoRld project path not found: {proj_path}"))
  }

  # Get schema using metawoRld function
  schema <- tryCatch({
    # Ensure metawoRld functions are available if running during dev
    # Or ensure metawoRld is installed and listed in Imports
    metawoRld::get_schema(path = proj_path)
  }, error = function(e) {
    rlang::abort(c(glue::glue("Failed to get schema from metawoRld project at: {proj_path}"),
                   "i" = e$message), parent = e)
  })

  # Validate the retrieved schema
  if (rlang::is_null(schema) || !rlang::is_list(schema) ||
      !"metadata_fields" %in% names(schema) || !rlang::is_list(schema$metadata_fields) ||
      !"data_fields" %in% names(schema) || !rlang::is_list(schema$data_fields) ) {
    rlang::abort(glue::glue("Invalid or incomplete schema found in metawoRld project at: {proj_path}. Missing 'metadata_fields' or 'data_fields'."))
  }

  # --- Extract Field Names from Schema ---
  req_meta_all <- schema$metadata_fields$required %||% character(0)
  opt_meta_all <- schema$metadata_fields$optional %||% character(0)
  req_data_all <- schema$data_fields$required %||% character(0)
  opt_data_all <- schema$data_fields$optional %||% character(0)

  # Define common fields explicitly mentioned in the prompt template for examples
  common_req_meta <- c("study_id", "title", "authors", "year", "journal", "study_design", "country", "sample_type", "outcome_groups", "measurement_methods")
  common_opt_meta <- c("doi", "abstract", "keywords_paper", "funding_source", "ethics_approval", "inclusion_summary", "exclusion_summary", "datafindr_assessment")
  common_req_data <- c("measurement_id", "method_ref_id", "group_label", "n", "statistic_type", "value1")
  common_opt_data <- c("value2", "unit", "comparison_group_label", "comparison_p_value", "notes")

  # Find fields defined in schema but NOT explicitly listed in the template examples
  additional_req_meta <- setdiff(req_meta_all, common_req_meta)
  additional_opt_meta <- setdiff(opt_meta_all, common_opt_meta)
  additional_req_data <- setdiff(req_data_all, common_req_data)
  additional_opt_data <- setdiff(opt_data_all, common_opt_data)

  # --- Structure Hints ---
  complex_hints <- schema$complex_field_structures %||% list()
  outcome_hint <- complex_hints$outcome_groups %||% "JSON object: keys=short unique group IDs, values=objects with 'name' (string) & 'definition' (string)."
  methods_hint <- complex_hints$measurement_methods %||% "JSON object: keys=short unique method IDs, values=objects detailing method (type, target, unit, kit...)."
  assess_hint <- complex_hints$datafindr_assessment %||% "JSON object: fields like score, rationale..."

  # --- Construct Focus Instructions ---
  # Generate instruction based on whether fetched_metadata was provided
  focus_instruction <- if (!rlang::is_null(fetched_metadata)) {
    "Some basic metadata (title, authors, etc.) may have been provided or fetched separately. Focus your primary extraction effort on the **Methods, Results, Tables, and Figures/Legends** to find: `study_design`, `country`, `sample_type`, detailed `outcome_groups`, detailed `measurement_methods`, and all quantitative `data_points`."
  } else {
    "Extract all requested metadata and data points. Pay close attention to Methods, Results, Tables, and Figures/Legends for study details, group definitions, methods, and quantitative results."
  }

  # Helper function to create the JSON lines for additional fields
  format_additional_fields <- function(fields, type = "REQUIRED") {
    if(length(fields) == 0) {
      return(paste0("// (No additional ", tolower(type), " fields in schema)"))
    }
    lines <- paste0('"', fields, '": "...", // ', type, ': As defined in schema')
    # Add trailing comma to all but the last line if needed for JSON structure visualization
    # lines <- paste0(lines, ",") # Be careful if this is the absolute last item
    # Simpler: just list them as comments
    paste(lines, collapse = "\n        ") # Indent correctly
  }


  # --- Construct the Prompt ---
  prompt <- glue::glue(
    '**Role:** You are a highly accurate and meticulous biomedical data extraction assistant. Your task is to carefully read the provided scientific research paper (full text as input) and extract specific information.

    **Input:** The full text content of a single scientific research paper. Assume the text may have been extracted from a PDF and might contain minor formatting artifacts, but represents the core content. Pay close attention to Methods, Results, Tables, and Figure Legends.

    **Task:** Extract the requested information and structure it **precisely** according to the JSON schema defined below. Ensure all required fields are present and correctly populated. If information for a required field absolutely cannot be found after thorough reading, use the JSON value `null`. For optional fields, populate them if found, use `null`, or omit the field entirely. Do NOT hallucinate or invent data.

    **Focus:** <<focus_instruction>>

    **Output Format:** Respond with ONLY a single, valid JSON object. Do NOT include any explanatory text, greetings, apologies, or markdown formatting like ```json before or after the JSON object itself.

    **JSON Schema and Structure:**

    The output JSON object must have two top-level keys: `"metadata"` (object) and `"data_points"` (array of objects).

    ```json
    {{
      "metadata": {{
        // --- Common Required Metadata Fields ---
        "study_id": "string", // REQUIRED: Use PMID/DOI like "<<identifier %||% "USE_DOI_OR_PMID">>". If none, generate ID (e.g., FirstAuthorYear).
        "title": "string", // REQUIRED: Full study title. (May be provided externally)
        "authors": ["string", "..."], // REQUIRED: List of author names. (May be provided externally)
        "year": integer, // REQUIRED: Publication year. (May be provided externally)
        "journal": "string", // REQUIRED: Journal name. (May be provided externally)
        "study_design": "string", // REQUIRED: Find in Methods (e.g., "Case-Control", "Cohort"). FOCUS HERE.
        "country": "string", // REQUIRED: Find in Methods/Affiliations. FOCUS HERE.
        "sample_type": "string", // REQUIRED: Find in Methods (e.g., "Serum", "Plasma"). FOCUS HERE.
        "outcome_groups": {{ // REQUIRED: Define groups from Methods/Results. FOCUS HERE. <<outcome_hint>>
          // Example: "grp1": {{ "name": "Preeclampsia Cases", "definition": "Diagnosed via XYZ criteria..." }}
        }},
        "measurement_methods": {{ // REQUIRED: Define assays from Methods. FOCUS HERE. <<methods_hint>>
          // Example: "elisa_il6": {{ "analysis_type": "ELISA", "target_cytokine": "IL-6", "unit": "pg/mL", ... }}
        }},
        // --- Additional Required Metadata Fields (from project schema if any) ---
        <<format_additional_fields(additional_req_meta, "REQUIRED")>>

        // --- Common Optional Metadata Fields ---
        // (Use null or omit if not found)
        "doi": "string | null", // (May be provided externally)
        "abstract": "string | null", // (May be provided externally)
        "keywords_paper": ["string", ...] | null,
        "funding_source": "string | null",
        "ethics_approval": "string | boolean | null", // Look in Methods
        "inclusion_summary": "string | null", // Look in Methods
        "exclusion_summary": "string | null", // Look in Methods
        "datafindr_assessment": {{ // If available/applicable: <<assess_hint>>
          "relevance_score": number | null,
          "rationale": "string | null"
        }} | null,
        // --- Additional Optional Metadata Fields (from project schema if any) ---
        <<format_additional_fields(additional_opt_meta, "OPTIONAL")>>
      }},
      "data_points": [ // REQUIRED: Array of objects. Extract ALL relevant results from Results/Tables. FOCUS HERE.
        // Each object represents a specific measurement result:
        {{
          // --- Common Required Data Fields ---
          "measurement_id": "string", // REQUIRED: Create unique ID ("m1", "m2"...).
          "method_ref_id": "string", // REQUIRED: Key matching metadata.measurement_methods. CRITICAL LINK.
          "cytokine_name": "string", // REQUIRED: Specific analyte (e.g., "IL-6").
          "group_label": "string", // REQUIRED: Key matching metadata.outcome_groups. CRITICAL LINK.
          "n": integer, // REQUIRED: Sample size for this group/statistic. Check tables/legends carefully.
          "statistic_type": "string", // REQUIRED: Be precise (e.g., "mean_sd", "median_iqr", "mean_sem").
          "value1": number, // REQUIRED: Mean or Median value.
          // --- Additional Required Data Fields (from project schema if any) ---
          <<format_additional_fields(additional_req_data, "REQUIRED")>>

          // --- Common Optional Data Fields ---
          "value2": number | string | null, // SD, SEM, IQR/range/CI bounds (e.g., "10.5-15.2" or number). Null if N/A.
          "unit": "string | null", // Unit (e.g., "pg/mL"). Check table/legend/method.
          "comparison_group_label": "string | null", // Optional: Key of comparison group for p-value.
          "comparison_p_value": number | string | null, // Optional: p-value (e.g., 0.04, "<0.001").
          "notes": "string | null", // Optional: Specific notes (e.g., "Adjusted", "LLOD").
          // --- Additional Optional Data Fields (from project schema if any) ---
          <<format_additional_fields(additional_opt_data, "OPTIONAL")>>
        }}
        // ... potentially more data point objects
      ]
    }}
    ```

    **Detailed Extraction Instructions:**

    *   **Focus Areas:** Prioritize Methods, Results, Tables, Figures/Legends for `study_design`, `country`, `sample_type`, `outcome_groups` definitions, `measurement_methods` details, and the entire `data_points` array.
    *   **Bibliographic Metadata (`title`, `authors`, etc.):** Extract if readily available, but primary focus is elsewhere. These may be overwritten by externally fetched data later.
    *   **`metadata.study_id`**: Use the provided identifier (`<<identifier %||% "DOI/PMID if provided">>`) if possible, otherwise look for DOI/PMID or generate `FirstAuthorLastNameYear`.
    *   **`metadata.outcome_groups`**: Define unique keys (e.g., `grp1`, `ctrl`). Use these EXACT keys in `data_points.group_label`.
    *   **`metadata.measurement_methods`**: Define unique keys (e.g., `il6_elisa_rd`). Use these EXACT keys in `data_points.method_ref_id`. Capture units accurately.
    *   **`data_points` Array**: Create ONE object for EACH reported result statistic (Mean IL-6@T1 != Median IL-6@T1). Find `n` carefully for each specific result. Be precise with `statistic_type`. Link correctly using `method_ref_id` and `group_label`.

    **Final Check:** Before outputting, ensure:
    1.  Output is a SINGLE valid JSON object, nothing else.
    2.  All REQUIRED fields (marked `// REQUIRED`) are present (use `null` if data truly absent).
    3.  `method_ref_id` and `group_label` values in `data_points` EXACTLY match keys defined in `metadata`.
    '
    , .open = "<<", .close = ">>") # Use different delimiters for glue

  return(prompt)
}
