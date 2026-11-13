# Build the extraction prompt for one study

Build the extraction prompt for one study

## Usage

``` r
df_extraction_prompt(identifier, metawoRld_path = ".", metadata = NULL)
```

## Arguments

- identifier:

  Study identifier.

- metawoRld_path:

  Project root.

- metadata:

  Optional known bibliographic metadata; by default taken from the
  metadata cache.

## Value

A single string.

## Details

The variable names and group categories defined in the project's
`_rules.yml` are included (placeholders `{{known_variables}}` and
`{{group_categories}}`), so the LLM uses the project's vocabulary.
Adding synonyms does not make cached extractions outdated, because that
would trigger costly re-extraction of every study; use `force = TRUE` in
[`df_extract_batch()`](https://andjar.github.io/DataFindR/reference/df_extract_batch.md)
to redo studies deliberately.
