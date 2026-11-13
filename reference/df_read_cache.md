# Read cached results

Read cached results

## Usage

``` r
df_read_cache(
  type = c("assessment", "extraction", "metadata"),
  metawoRld_path = ".",
  identifiers = NULL
)
```

## Arguments

- type:

  `"metadata"`, `"assessment"` or `"extraction"`.

- metawoRld_path:

  Project root.

- identifiers:

  Optional identifiers to read; defaults to all cached.

## Value

A named list (names are identifiers). For assessments and extractions
each element has `result` and `provenance`.
