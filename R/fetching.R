#' Metadata Fetching Functions
#'
#' This file contains functions to retrieve study metadata (e.g., title, abstract, authors)
#' from online sources like PubMed and Crossref using DOIs or PMIDs.
#' @title Fetch Metadata for a Study Identifier (DOI or PMID)
#'
#' @description
#' Retrieves bibliographic metadata (title, abstract, authors, year, journal)
#' for a given DOI or PMID using online services (Crossref, PubMed).
#'
#' @param identifier Character string. The DOI or PMID of the study.
#' @param email Character string (optional). Your email address, recommended
#'   for NCBI Entrez politeness when fetching from PubMed. If not provided,
#'   `rentrez` might issue a warning.
#' @param ncbi_api_key Character string (optional). Your NCBI API key for
#'   potentially higher rate limits. Set using `rentrez::set_entrez_key()`.
#'
#' @return A list containing the fetched metadata (identifier, type, title,
#'   abstract, authors, year, journal) or `NULL` if fetching fails or the
#'   identifier type is not recognized. Issues warnings on failure.
#' @export
#' @importFrom rentrez entrez_summary entrez_fetch set_entrez_key fetch_error uid_error
#' @importFrom rlang check_installed is_scalar_character warn inform abort `%||%`
#' @importFrom glue glue
#' @importFrom stats setNames
#'
#' @examples
#' \dontrun{
#' # Fetch metadata for a known PMID (requires internet connection)
#' pmid_meta <- df_fetch_metadata("25376210", email = "your.email@example.com")
#' print(pmid_meta)
#'
#' # Fetch metadata for a known DOI (requires internet connection and rcrossref)
#' doi_meta <- df_fetch_metadata("10.1038/nature14539", email = "your.email@example.com")
#' print(doi_meta)
#'
#' # Example of a potentially invalid identifier
#' invalid_meta <- df_fetch_metadata("invalid-id")
#' print(invalid_meta)
#' }
df_fetch_metadata <- function(identifier, email = NULL, ncbi_api_key = NULL) {

  if (!rlang::is_scalar_character(identifier) || identifier == "") {
    rlang::abort("`identifier` must be a non-empty character string.")
  }
  if (!is.null(email) && !rlang::is_scalar_character(email)) {
    rlang::warn("`email` should be a single string. Ignoring provided value.")
    email <- NULL
  }
  if (!is.null(ncbi_api_key)) {
    rentrez::set_entrez_key(ncbi_api_key)
    rlang::inform("NCBI API key set via rentrez.")
  }
  if (!is.null(email)) {
    Sys.setenv("ENTREZ_EMAIL" = email) # Set for rentrez session
    rlang::inform("NCBI email address set via environment variable ENTREZ_EMAIL.")
  } else {
    # Check if email is already set globally, otherwise rentrez warns
    if(Sys.getenv("ENTREZ_EMAIL") == "") {
      rlang::warn("No email address provided for NCBI Entrez. Rate limiting may be stricter.")
    }
  }

  # --- Determine Identifier Type ---
  id_type <- NULL
  if (grepl("^[0-9]+$", identifier)) {
    id_type <- "pmid"
  } else if (grepl("^10\\.", identifier)) {
    # Basic check for DOI starting with 10. More robust check possible
    id_type <- "doi"
  } else {
    rlang::warn(glue::glue("Could not determine identifier type (PMID or DOI) for: '{identifier}'"))
    return(NULL)
  }

  rlang::inform(glue::glue("Attempting to fetch metadata for {toupper(id_type)}: {identifier}"))

  # --- Fetch Data ---
  metadata <- NULL
  tryCatch({
    if (id_type == "pmid") {
      # Fetch Summary for core metadata
      summary_rec <- rentrez::entrez_summary(db = "pubmed", id = identifier, email = email)
      if (inherits(summary_rec, "uid_error") || length(summary_rec) == 0) {
        stop(glue::glue("PMID '{identifier}' not found or invalid on PubMed."))
      }
      # Extract fields safely
      title <- summary_rec$title %||% NA_character_
      authors <- summary_rec$authors$name %||% NA_character_
      journal <- summary_rec$source %||% NA_character_
      year_str <- summary_rec$pubdate %||% NA_character_
      year <- suppressWarnings(as.integer(substr(year_str, 1, 4))) # Extract year

      # Fetch Abstract separately
      abstract_rec <- rentrez::entrez_fetch(db = "pubmed", id = identifier, rettype = "abstract", retmode = "text", email = email)
      if (inherits(abstract_rec, "fetch_error") || length(abstract_rec) == 0) {
        rlang::warn(glue::glue("Could not fetch abstract for PMID '{identifier}'."))
        abstract <- NA_character_
      } else {
        # Clean up abstract formatting
        abstract <- gsub("\\n+", " ", abstract_rec) # Replace multiple newlines with space
        abstract <- trimws(abstract)
      }

      metadata <- list(
        identifier = identifier,
        type = "pmid",
        title = title,
        abstract = abstract,
        authors = as.list(authors), # Keep as list for consistency
        year = year,
        journal = journal
      )

    } else if (id_type == "doi") {
      rlang::check_installed("rcrossref", reason = "to fetch metadata using DOIs.")
      # Use cr_works, suppressing messages if desired
      work_info <- rcrossref::cr_works(dois = identifier, .progress = "none")

      if (is.null(work_info) || is.null(work_info$data) || nrow(work_info$data) == 0) {
        stop(glue::glue("DOI '{identifier}' not found or invalid via Crossref."))
      }
      data <- work_info$data

      # Extract fields safely - names can vary slightly, use %||%
      title <- data$title %||% NA_character_
      # Authors might be nested
      authors_df <- data$author[[1]] # Assumes one work returned
      authors <- if (is.data.frame(authors_df)) paste(authors_df$given, authors_df$family) else NA_character_
      journal <- data$container.title %||% NA_character_
      # Year extraction needs care
      year_parts <- strsplit(data$issued[[1]], "-", fixed = TRUE)[[1]]
      year <- if(length(year_parts) > 0) suppressWarnings(as.integer(year_parts[1])) else NA_integer_
      abstract_html <- data$abstract %||% NA_character_
      # Clean abstract HTML (basic cleaning)
      abstract <- gsub("<.*?>", " ", abstract_html) # Remove HTML tags
      abstract <- gsub("\\s+", " ", abstract) # Collapse whitespace
      abstract <- trimws(abstract)


      metadata <- list(
        identifier = identifier,
        type = "doi",
        title = title,
        abstract = abstract,
        authors = as.list(authors), # Keep as list
        year = year,
        journal = journal
      )
    }
  }, error = function(e) {
    rlang::warn(glue::glue("Failed to fetch metadata for {toupper(id_type)} '{identifier}'. Reason: {e$message}"))
    metadata <<- NULL # Ensure metadata is NULL on error
  })

  if (is.null(metadata)) {
    return(NULL)
  }

  # --- Final Check for critical missing info ---
  if (is.na(metadata$title) || metadata$title == "") {
    rlang::warn(glue::glue("Fetched metadata for '{identifier}', but title is missing."))
    # Decide if this is fatal - maybe allow proceeding without title?
  }
  if (is.na(metadata$abstract) || metadata$abstract == "") {
    rlang::warn(glue::glue("Fetched metadata for '{identifier}', but abstract is missing. Assessment quality may be reduced."))
    # Proceeding without abstract might be okay, but assessment will be poor.
  }

  rlang::inform(glue::glue("Successfully fetched metadata for {toupper(id_type)}: {identifier}"))
  return(metadata)
}
