# Clear cached results

Clear cached results

## Usage

``` r
df_clear_cache(
  identifiers = NULL,
  type = c("metadata", "assessment", "extraction"),
  metawoRld_path = ".",
  ask = interactive()
)
```

## Arguments

- identifiers:

  Identifiers to clear. `NULL` clears everything of the given types
  (asks for confirmation in interactive sessions).

- type:

  One or more of `"metadata"`, `"assessment"`, `"extraction"`.

- metawoRld_path:

  Project root.

- ask:

  Ask before clearing everything? Defaults to
  [`interactive()`](https://rdrr.io/r/base/interactive.html).

## Value

Number of files deleted, invisibly.
