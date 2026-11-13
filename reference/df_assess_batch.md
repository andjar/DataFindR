# Screen studies by title and abstract with an LLM

For each identifier: gets metadata (from the cache, or fetched and
cached), fills the project's assessment prompt, asks the LLM for a
structured decision following `_assessment_schema.yml`, validates it,
and caches the result together with its provenance (model, token use,
cost and a fingerprint of the prompt, schema and criteria).

## Usage

``` r
df_assess_batch(
  chat,
  identifiers,
  metawoRld_path = ".",
  mode = c("parallel", "sequential", "batch"),
  force = FALSE,
  force_fetch = FALSE,
  max_active = 10,
  batch_file = NULL,
  wait = TRUE,
  email = Sys.getenv("DATAFINDR_EMAIL"),
  api_key = Sys.getenv("NCBI_API_KEY")
)

df_assess_relevance(chat, identifier, metawoRld_path = ".", ...)
```

## Arguments

- chat:

  An ellmer chat object, e.g.
  `ellmer::chat_openai(model = "gpt-4.1-mini")`, or a `"provider/model"`
  string for
  [`ellmer::chat()`](https://ellmer.tidyverse.org/reference/chat-any.html).

- identifiers:

  Character vector of DOIs/PMIDs.

- metawoRld_path:

  Project root.

- mode:

  `"parallel"` (default; many requests at once), `"sequential"` (one by
  one), or `"batch"` (the provider's batch API: about half the price,
  but results can take up to 24 hours; supported by OpenAI, Anthropic
  and Google Gemini).

- force:

  Re-assess even if a current cached result exists.

- force_fetch:

  Re-fetch metadata even if cached.

- max_active:

  Maximum simultaneous requests in parallel mode.

- batch_file:

  File used to track a batch job (mode `"batch"`). Re-run the same call
  to resume. Defaults to a file in the cache folder.

- wait:

  In batch mode, wait for the results? If `FALSE` and the batch is not
  finished, the function returns early; call it again later.

- email, api_key:

  Passed to
  [`df_fetch_metadata()`](https://andjar.github.io/DataFindR/reference/df_fetch_metadata.md).

- identifier:

  A single DOI/PMID.

- ...:

  Further arguments passed to `df_assess_batch()`.

## Value

A tibble with one row per identifier: `identifier`, `status`
(`"assessed"`, `"cached"`, `"failed"` or `"pending"`), `decision`,
`score`, `rationale`, `exclusion_reason`, `error`, `input_tokens`,
`output_tokens` and `cost`.

`df_assess_relevance()`: the assessment as a list (`decision`, `score`,
`rationale`, ...), or an error.

## Details

Cached results are reused unless they were produced with a different
prompt, schema, criteria or model (then they are redone), or
`force = TRUE`. Manual decisions (e.g. saved from
[`df_shiny_assess()`](https://andjar.github.io/DataFindR/reference/df_shiny_assess.md))
are never overwritten unless `force = TRUE`.

## Examples

``` r
if (FALSE) { # \dontrun{
chat <- ellmer::chat_openai(model = "gpt-4.1-mini")
hits <- df_search_pubmed("biomarker X AND pregnancy", "my_review")
res <- df_assess_batch(chat, hits$identifier, "my_review")
table(res$decision)
} # }
```
