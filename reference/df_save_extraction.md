# Save a manually written or corrected extraction

Save a manually written or corrected extraction

## Usage

``` r
df_save_extraction(identifier, extraction, metawoRld_path = ".")
```

## Arguments

- identifier:

  Study identifier.

- extraction:

  List with `metadata` and `data_points` (as returned by
  [`df_extract()`](https://andjar.github.io/DataFindR/reference/df_extract_batch.md)),
  or a JSON string.

- metawoRld_path:

  Project root.

## Value

The validation problems (character vector), invisibly. The result is
saved even if there are problems, so it can be fixed later.
