# Counts for a PRISMA flow diagram

Counts for a PRISMA flow diagram

## Usage

``` r
df_prisma_counts(metawoRld_path = ".")
```

## Arguments

- metawoRld_path:

  Project root.

## Value

A tibble with `stage` and `n`: records identified, screened, excluded at
screening (with a row per exclusion reason when available), needing
manual review, included for full text, extracted and included in the
review (imported).
