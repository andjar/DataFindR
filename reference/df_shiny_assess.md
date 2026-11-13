# Interactive screening app

Browse studies, edit titles/abstracts, preview the exact prompt, run the
LLM for one study, and record or override decisions by hand. Manual
decisions are kept by later
[`df_assess_batch()`](https://andjar.github.io/DataFindR/reference/df_assess_batch.md)
runs.

## Usage

``` r
df_shiny_assess(metawoRld_path = ".", chat = NULL, launch.browser = TRUE)
```

## Arguments

- metawoRld_path:

  Project root.

- chat:

  Optional ellmer chat object. If `NULL`, a `"provider/model"` string
  can be entered in the app.

- launch.browser:

  Passed to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html).

## Value

Runs the app.
