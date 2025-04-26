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
