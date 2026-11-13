# Save a manual screening decision

Manual decisions are never overwritten by
[`df_assess_batch()`](https://andjar.github.io/DataFindR/reference/df_assess_batch.md)
unless `force = TRUE` is used there.

## Usage

``` r
df_save_assessment(
  identifier,
  decision,
  rationale = "",
  score = NULL,
  exclusion_reason = NULL,
  metawoRld_path = ".",
  reviewer = NULL
)
```

## Arguments

- identifier:

  Study identifier.

- decision:

  One of the decisions allowed by the assessment schema (by default
  `"Include"`, `"Exclude"`, `"Needs manual review"`).

- rationale:

  Short justification.

- score:

  Optional inclusion probability (0-1).

- exclusion_reason:

  Optional reason for exclusion.

- metawoRld_path:

  Project root.

- reviewer:

  Optional reviewer name or initials.

## Value

The cache file, invisibly.
