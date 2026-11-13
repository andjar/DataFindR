# Import a cached extraction into the metawoRld project

Takes the cached extraction, optionally replaces bibliographic fields
with the fetched metadata (which is usually more reliable than the LLM's
reading), and stores the study with
[`metawoRld::add_study_data()`](https://rdrr.io/pkg/metawoRld/man/add_study_data.html),
which validates it against the schema. The study is stored under its
DOI/PMID, so cache and project stay linked.

## Usage

``` r
df_import_extraction(
  identifier,
  metawoRld_path = ".",
  overwrite = FALSE,
  merge_metadata = TRUE
)
```

## Arguments

- identifier:

  Study identifier.

- metawoRld_path:

  Project root.

- overwrite:

  Replace the study if it was imported before?

- merge_metadata:

  Use fetched title, authors, year, journal, DOI, PMID and abstract when
  available? Defaults to `TRUE`.

## Value

The study folder, invisibly.
