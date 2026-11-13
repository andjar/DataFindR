#' DataFindR: LLM-assisted screening and data extraction for metawoRld projects
#'
#' DataFindR is one input channel for a metawoRld project. It writes to the
#' project only through metawoRld's functions: searches and records go to the
#' registry, screening decisions to `screening.csv` (as the tool
#' `datafindr`), and extracted data become submissions that a person accepts.
#'
#' Typical workflow:
#' 1. [df_search_pubmed()] (or [df_fetch_metadata()] / [df_add_metadata()])
#'    collects records, logs the search and registers the records.
#' 2. [df_assess_batch()] screens them against the project's criteria;
#'    [df_shiny_assess()] lets people review and add their own decisions.
#' 3. [df_extract_batch()] extracts data from the full texts of included
#'    studies; [df_shiny_extract()] lets you check and correct the results.
#' 4. [df_import_batch()] submits the extractions with
#'    [metawoRld::submit_study()]; accept them with
#'    [metawoRld::accept_submission()] or [metawoRld::reconcile()].
#'    [df_status()] and [metawoRld::prisma_counts()] show progress.
#'
#' Prompts, forms and criteria live in the metawoRld project and can be
#' edited there. All LLM results are cached with their provenance (model,
#' tokens, cost, source file hash), so repeated runs only process new or
#' changed work. Local models (e.g. `ellmer::chat_ollama()`) work like any
#' other provider.
#'
#' @keywords internal
#' @importFrom rlang %||%
"_PACKAGE"
