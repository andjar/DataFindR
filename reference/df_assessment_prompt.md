# Build the screening prompt for one study

Fills the project's `_assessment_prompt.txt` with the project
description, criteria and the study's title and abstract. Useful to
inspect exactly what is sent to the LLM.

## Usage

``` r
df_assessment_prompt(metadata, metawoRld_path = ".")
```

## Arguments

- metadata:

  Study metadata (list with `title` and `abstract`), e.g. from
  [`df_fetch_metadata()`](https://andjar.github.io/DataFindR/reference/df_fetch_metadata.md).

- metawoRld_path:

  Project root.

## Value

A single string.
