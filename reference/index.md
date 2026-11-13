# Package index

## Records

- [`df_search_pubmed()`](https://andjar.github.io/DataFindR/reference/df_search_pubmed.md)
  : Search PubMed and cache the results
- [`df_fetch_metadata()`](https://andjar.github.io/DataFindR/reference/df_fetch_metadata.md)
  : Fetch bibliographic metadata for DOIs and PMIDs
- [`df_add_metadata()`](https://andjar.github.io/DataFindR/reference/df_add_metadata.md)
  : Add metadata for a study by hand

## Screening

- [`df_assess_batch()`](https://andjar.github.io/DataFindR/reference/df_assess_batch.md)
  [`df_assess_relevance()`](https://andjar.github.io/DataFindR/reference/df_assess_batch.md)
  : Screen studies by title and abstract with an LLM
- [`df_save_assessment()`](https://andjar.github.io/DataFindR/reference/df_save_assessment.md)
  : Save a manual screening decision
- [`df_shiny_assess()`](https://andjar.github.io/DataFindR/reference/df_shiny_assess.md)
  : Interactive screening app
- [`df_assessment_prompt()`](https://andjar.github.io/DataFindR/reference/df_assessment_prompt.md)
  : Build the screening prompt for one study

## Extraction

- [`df_extract_batch()`](https://andjar.github.io/DataFindR/reference/df_extract_batch.md)
  [`df_extract()`](https://andjar.github.io/DataFindR/reference/df_extract_batch.md)
  : Extract data from full texts with an LLM
- [`df_save_extraction()`](https://andjar.github.io/DataFindR/reference/df_save_extraction.md)
  : Save a manually written or corrected extraction
- [`df_shiny_extract()`](https://andjar.github.io/DataFindR/reference/df_shiny_extract.md)
  : Interactive extraction app
- [`df_find_fulltexts()`](https://andjar.github.io/DataFindR/reference/df_find_fulltexts.md)
  : Find full-text files named after study identifiers
- [`df_fulltext_filename()`](https://andjar.github.io/DataFindR/reference/df_fulltext_filename.md)
  : Suggested file name for a study's full text
- [`df_extraction_prompt()`](https://andjar.github.io/DataFindR/reference/df_extraction_prompt.md)
  : Build the extraction prompt for one study
- [`df_schema_to_ellmer()`](https://andjar.github.io/DataFindR/reference/df_schema_to_ellmer.md)
  : Convert a metawoRld schema to an ellmer type specification

## Import and progress

- [`df_import_batch()`](https://andjar.github.io/DataFindR/reference/df_import_batch.md)
  : Import several cached extractions
- [`df_import_extraction()`](https://andjar.github.io/DataFindR/reference/df_import_extraction.md)
  : Import a cached extraction into the metawoRld project
- [`df_status()`](https://andjar.github.io/DataFindR/reference/df_status.md)
  : Status of every study in the pipeline
- [`df_prisma_counts()`](https://andjar.github.io/DataFindR/reference/df_prisma_counts.md)
  : Counts for a PRISMA flow diagram
- [`df_pending_extraction()`](https://andjar.github.io/DataFindR/reference/df_pending_extraction.md)
  : Studies waiting for data extraction
- [`df_read_cache()`](https://andjar.github.io/DataFindR/reference/df_read_cache.md)
  : Read cached results
- [`df_clear_cache()`](https://andjar.github.io/DataFindR/reference/df_clear_cache.md)
  : Clear cached results
