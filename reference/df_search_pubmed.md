# Search PubMed and cache the results

Runs a PubMed query, fetches metadata for every hit and stores it in the
project's metadata cache, ready for
[`df_assess_batch()`](https://andjar.github.io/DataFindR/reference/df_assess_batch.md).
Records that are already cached are not fetched again, which makes
repeated searches cheap in a living review.

## Usage

``` r
df_search_pubmed(
  query,
  metawoRld_path = ".",
  max_results = 10000,
  mindate = NULL,
  maxdate = NULL,
  email = Sys.getenv("DATAFINDR_EMAIL"),
  api_key = Sys.getenv("NCBI_API_KEY")
)
```

## Arguments

- query:

  PubMed query string.

- metawoRld_path:

  Project root.

- max_results:

  Maximum number of records. Defaults to 10000.

- mindate, maxdate:

  Optional publication date limits (`"YYYY/MM/DD"`, `"YYYY/MM"` or
  `"YYYY"`).

- email:

  Contact e-mail sent to NCBI and Crossref (recommended; NCBI may block
  anonymous heavy use). Defaults to the `DATAFINDR_EMAIL` environment
  variable.

- api_key:

  NCBI API key (allows 10 instead of 3 requests per second). Defaults to
  the `NCBI_API_KEY` environment variable.

## Value

A tibble with `identifier` (PMID), `title`, `year` and `new` (`TRUE` if
the record was not in the cache before).
