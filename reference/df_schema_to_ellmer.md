# Convert a metawoRld schema to an ellmer type specification

Convert a metawoRld schema to an ellmer type specification

## Usage

``` r
df_schema_to_ellmer(schema)
```

## Arguments

- schema:

  A schema list (e.g. from
  [`metawoRld::get_schema()`](https://rdrr.io/pkg/metawoRld/man/get_schema.html))
  or the path to a schema YAML file.

## Value

An ellmer type object for `chat$chat_structured()` and friends.
