test_that("PubMed XML is parsed with structured abstracts, collective authors and MedlineDate", {
  local_mocked_bindings(.http_get = fake_http)
  m <- suppressWarnings(df_fetch_metadata(c("PMID: 11111111", "22222222"), email = "a@b.c", api_key = ""))
  expect_named(m, c("11111111", "22222222"))
  expect_equal(m[["11111111"]]$abstract, "BACKGROUND: X may matter.\nRESULTS: X was higher in cases.")
  expect_equal(m[["11111111"]]$authors, c("Smith J", "The X Group"))
  expect_equal(m[["11111111"]]$doi, "10.1000/abc.1")
  expect_equal(m[["22222222"]]$year, 2019L)
  expect_true(is.na(m[["22222222"]]$abstract))
})

test_that("DOIs use PubMed when indexed and Crossref otherwise", {
  local_mocked_bindings(.http_get = fake_http)
  m <- df_fetch_metadata(c("https://doi.org/10.1000/ABC.1", "10.2000/xyz"), email = "", api_key = "")
  expect_equal(m[["10.1000/abc.1"]]$source, "pubmed")
  expect_equal(m[["10.1000/abc.1"]]$pmid, "11111111")
  expect_equal(m[["10.2000/xyz"]]$source, "crossref")
  expect_equal(m[["10.2000/xyz"]]$abstract, "We measured X .")
  expect_equal(m[["10.2000/xyz"]]$year, 2023L)
})

test_that("df_search_pubmed caches records and flags new ones", {
  local_mocked_bindings(.http_get = fake_http)
  p <- local_project()
  r1 <- suppressMessages(df_search_pubmed("X AND pregnancy", p, email = "", api_key = ""))
  expect_equal(r1$identifier, c("11111111", "22222222"))
  expect_true(all(r1$new))
  r2 <- suppressMessages(df_search_pubmed("X AND pregnancy", p, email = "", api_key = ""))
  expect_false(any(r2$new))
  # NA is stored as JSON null, not the string "NA"
  raw <- readLines(DataFindR:::.cache_file("22222222", "metadata", p))
  expect_true(any(grepl('"abstract": null', raw, fixed = TRUE)))
})
