#' DataFindR: LLM-assisted screening and data extraction for metawoRld projects
#'
#' Typical workflow:
#' 1. [df_search_pubmed()] (or [df_fetch_metadata()] / [df_add_metadata()])
#'    collects records and caches their titles and abstracts.
#' 2. [df_assess_batch()] screens them against the project's criteria;
#'    [df_shiny_assess()] lets you review and override decisions.
#' 3. [df_extract_batch()] extracts data from the full texts of included
#'    studies; [df_shiny_extract()] lets you check and correct the results.
#' 4. [df_import_batch()] validates and stores the studies in the metawoRld
#'    project. [df_status()] and [df_prisma_counts()] show progress.
#'
#' Prompts, schemas and criteria live in the metawoRld project and can be
#' edited there. All LLM results are cached in the project with their
#' provenance, so repeated runs only process new or changed work.
#'
#' @keywords internal
#' @importFrom rlang %||%
"_PACKAGE"
