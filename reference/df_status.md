# Status of every study in the pipeline

Combines the metadata, assessment and extraction caches with the studies
already imported into the metawoRld project.

## Usage

``` r
df_status(metawoRld_path = ".", chat = NULL)
```

## Arguments

- metawoRld_path:

  Project root.

- chat:

  Optional ellmer chat used for assessment/extraction. If given,
  `assessment_stale`/`extraction_stale` show whether the cached result
  was produced with a different prompt, schema, criteria or model.

## Value

A tibble with one row per identifier: `has_metadata`, `has_abstract`,
`decision`, `score`, `assessment_source`, `extracted`,
`extraction_errors` (number of schema problems), `imported`, and the
staleness columns.
