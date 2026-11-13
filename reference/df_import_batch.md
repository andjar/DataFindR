# Import several cached extractions

Import several cached extractions

## Usage

``` r
df_import_batch(
  identifiers = NULL,
  metawoRld_path = ".",
  overwrite = FALSE,
  merge_metadata = TRUE,
  canonical_names = TRUE
)
```

## Arguments

- identifiers:

  Identifiers to import. Defaults to every cached extraction that is not
  yet in the project.

- metawoRld_path:

  Project root.

- overwrite, merge_metadata, canonical_names:

  See
  [`df_import_extraction()`](https://andjar.github.io/DataFindR/reference/df_import_extraction.md).

## Value

A tibble with `identifier`, `status` (`"imported"` or `"failed"`),
`path` and `error`. Validation problems are listed in `error`.
