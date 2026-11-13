# Studies waiting for data extraction

Studies waiting for data extraction

## Usage

``` r
df_pending_extraction(metawoRld_path = ".", decisions = "Include")
```

## Arguments

- metawoRld_path:

  Project root.

- decisions:

  Assessment decisions that qualify. Defaults to `"Include"`.

## Value

Character vector of identifiers that qualify but have no cached
extraction yet.
