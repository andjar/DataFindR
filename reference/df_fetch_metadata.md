# Fetch bibliographic metadata for DOIs and PMIDs

Looks up title, abstract, authors, year, journal, DOI and PMID. PubMed
is tried first (structured abstracts are kept, with section labels);
DOIs not indexed in PubMed fall back to Crossref. Requests are batched
and rate-limited according to NCBI's rules.

## Usage

``` r
df_fetch_metadata(
  identifiers,
  email = Sys.getenv("DATAFINDR_EMAIL"),
  api_key = Sys.getenv("NCBI_API_KEY")
)
```

## Arguments

- identifiers:

  Character vector of DOIs and/or PMIDs. Prefixes such as
  `https://doi.org/` or `PMID:` are removed.

- email:

  Contact e-mail sent to NCBI and Crossref (recommended; NCBI may block
  anonymous heavy use). Defaults to the `DATAFINDR_EMAIL` environment
  variable.

- api_key:

  NCBI API key (allows 10 instead of 3 requests per second). Defaults to
  the `NCBI_API_KEY` environment variable.

## Value

A named list (names are normalized identifiers). Each element is a list
with `identifier`, `doi`, `pmid`, `title`, `abstract`, `authors`,
`year`, `journal`, `source` and `fetched`, or `NULL` if nothing was
found.

## Examples

``` r
if (FALSE) { # \dontrun{
df_fetch_metadata(c("31772108", "10.1038/nature14539"), email = "me@example.org")
} # }
```
