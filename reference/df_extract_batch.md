# Extract data from full texts with an LLM

Sends each full text with the project's extraction prompt and asks for a
structured result following `_extraction_schema.yml`. The result is
validated against the schema (including the `_ref` links between data
points and metadata) and cached with its provenance and any validation
problems. Use
[`df_import_batch()`](https://andjar.github.io/DataFindR/reference/df_import_batch.md)
to move the results into the project.

## Usage

``` r
df_extract_batch(
  chat,
  files,
  metawoRld_path = ".",
  mode = c("sequential", "parallel", "batch"),
  force = FALSE,
  max_active = 3,
  batch_file = NULL,
  wait = TRUE
)

df_extract(chat, identifier, file, metawoRld_path = ".", ...)
```

## Arguments

- chat:

  An ellmer chat object or `"provider/model"` string. Use a model that
  accepts PDFs if you pass PDFs.

- files:

  Named character vector of file paths, names being the study
  identifiers; or a single folder, in which case files named after the
  identifiers are used (see
  [`df_find_fulltexts()`](https://andjar.github.io/DataFindR/reference/df_find_fulltexts.md)).

- metawoRld_path:

  Project root.

- mode:

  `"sequential"` (default), `"parallel"` or `"batch"` (see
  [`df_assess_batch()`](https://andjar.github.io/DataFindR/reference/df_assess_batch.md)).
  Full texts are large, so mind your provider's rate limits when using
  `"parallel"`.

- force:

  Re-extract even if a current cached result exists.

- max_active, batch_file, wait:

  See
  [`df_assess_batch()`](https://andjar.github.io/DataFindR/reference/df_assess_batch.md).

- identifier:

  A single study identifier.

- file:

  Path to its full text.

- ...:

  Further arguments passed to `df_extract_batch()`.

## Value

A tibble with `identifier`, `status` (`"extracted"`, `"cached"`,
`"failed"` or `"pending"`), `n_data_points`, `n_problems` (schema
validation problems), `error`, `input_tokens`, `output_tokens`, `cost`.

`df_extract()`: the extracted data as a list (`metadata`,
`data_points`), or an error.

## Details

Full texts can be PDF (sent as a document to models that support it),
plain text/Markdown, or HTML/XML (e.g. JATS from PubMed Central), whose
text is extracted first.

## Examples

``` r
if (FALSE) { # \dontrun{
chat <- ellmer::chat_anthropic()
todo <- df_pending_extraction("my_review")
# PDFs saved as fulltext/<identifier>.pdf (DOIs with "/" replaced, see df_find_fulltexts())
res <- df_extract_batch(chat, "my_review/fulltext", "my_review")
df_import_batch(metawoRld_path = "my_review")
} # }
```
