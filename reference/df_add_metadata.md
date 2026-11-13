# Add metadata for a study by hand

Stores title, abstract, etc. in the metadata cache, e.g. for records
that are not in PubMed or Crossref, or to correct a fetched abstract.

## Usage

``` r
df_add_metadata(identifier, metadata, metawoRld_path = ".")
```

## Arguments

- identifier:

  Study identifier.

- metadata:

  Named list with at least `title`; ideally also `abstract`, `authors`,
  `year`, `journal`, `doi` and `pmid`.

- metawoRld_path:

  Project root.

## Value

The cache file path, invisibly.
