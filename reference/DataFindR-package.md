# DataFindR: LLM-assisted screening and data extraction for metawoRld projects

Typical workflow:

1.  [`df_search_pubmed()`](https://andjar.github.io/DataFindR/reference/df_search_pubmed.md)
    (or
    [`df_fetch_metadata()`](https://andjar.github.io/DataFindR/reference/df_fetch_metadata.md)
    /
    [`df_add_metadata()`](https://andjar.github.io/DataFindR/reference/df_add_metadata.md))
    collects records and caches their titles and abstracts.

2.  [`df_assess_batch()`](https://andjar.github.io/DataFindR/reference/df_assess_batch.md)
    screens them against the project's criteria;
    [`df_shiny_assess()`](https://andjar.github.io/DataFindR/reference/df_shiny_assess.md)
    lets you review and override decisions.

3.  [`df_extract_batch()`](https://andjar.github.io/DataFindR/reference/df_extract_batch.md)
    extracts data from the full texts of included studies;
    [`df_shiny_extract()`](https://andjar.github.io/DataFindR/reference/df_shiny_extract.md)
    lets you check and correct the results.

4.  [`df_import_batch()`](https://andjar.github.io/DataFindR/reference/df_import_batch.md)
    validates and stores the studies in the metawoRld project.
    [`df_status()`](https://andjar.github.io/DataFindR/reference/df_status.md)
    and
    [`df_prisma_counts()`](https://andjar.github.io/DataFindR/reference/df_prisma_counts.md)
    show progress.

## Details

Prompts, schemas and criteria live in the metawoRld project and can be
edited there. All LLM results are cached in the project with their
provenance, so repeated runs only process new or changed work.

## See also

Useful links:

- <https://github.com/andjar/DataFindR>

- Report bugs at <https://github.com/andjar/DataFindR/issues>

## Author

**Maintainer**: Anders Hagen Jarmund <anders.h.jarmund@ntnu.no>
([ORCID](https://orcid.org/0000-0002-3923-1637))
