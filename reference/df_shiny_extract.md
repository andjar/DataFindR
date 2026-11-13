# Interactive extraction app

Pick an included study, run the LLM on its full text, review and correct
the extracted JSON with live schema validation, save it, and import it
into the metawoRld project.

## Usage

``` r
df_shiny_extract(
  metawoRld_path = ".",
  chat = NULL,
  fulltext_dir = NULL,
  launch.browser = TRUE
)
```

## Arguments

- metawoRld_path:

  Project root.

- chat:

  Optional ellmer chat object. If `NULL`, a `"provider/model"` string
  can be entered in the app.

- fulltext_dir:

  Optional folder with full texts named after the identifiers (see
  [`df_find_fulltexts()`](https://andjar.github.io/DataFindR/reference/df_find_fulltexts.md)).

- launch.browser:

  Passed to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html).

## Value

Runs the app.
