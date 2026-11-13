# Find full-text files named after study identifiers

File names are the identifier encoded with
[`metawoRld::sanitize_id()`](https://rdrr.io/pkg/metawoRld/man/sanitize_id.html)
(e.g. `10.1000_fslash_abc.pdf` for DOI `10.1000/abc`, or `12345678.pdf`
for a PMID).

## Usage

``` r
df_find_fulltexts(dir)
```

## Arguments

- dir:

  Folder to search (not recursive).

## Value

Named character vector of file paths; names are identifiers.
