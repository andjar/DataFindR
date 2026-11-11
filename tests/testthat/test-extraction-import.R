test_that("extract -> import -> metawoRld, with merged metadata and status/PRISMA", {
  local_mocked_bindings(.http_get = fake_http)
  p <- local_project()
  suppressMessages(df_search_pubmed("q", p, email = "", api_key = ""))
  suppressMessages(df_assess_batch(mock_chat(include_all), c("11111111", "22222222"), p, mode = "sequential"))
  expect_equal(df_pending_extraction(p), "11111111")

  ft <- file.path(dirname(p), "fulltext"); dir.create(ft)
  writeLines("Full text of the study. Table 2 ...", file.path(ft, df_fulltext_filename("11111111", "txt")))
  chat <- mock_chat(good_extraction)
  res <- suppressMessages(df_extract_batch(chat, ft, p))
  expect_equal(res$status, "extracted")
  expect_equal(res$n_problems, 0L)
  # the prompt includes the full text and known metadata
  sent <- chat$prompts[[1]]
  expect_match(sent[[1]], "Full text of the study")
  expect_match(sent[[2]], "title: Biomarker X in pregnant women")

  imp <- suppressMessages(df_import_batch(metawoRld_path = p))
  expect_equal(imp$status, "imported")
  w <- suppressMessages(metawoRld::load_metawoRld(p))
  expect_equal(names(w$studies_metadata), "11111111")                 # stored under the PMID
  expect_equal(w$studies_metadata[[1]]$title, "Biomarker X in pregnant women: a cohort study.")  # fetched title wins
  expect_equal(w$studies_data$group_name, c("Cases", "Controls"))

  st <- df_status(p)
  expect_true(st$imported[st$identifier == "11111111"])
  pc <- df_prisma_counts(p)
  expect_equal(pc$n[pc$stage == "Records screened"], 2)
  expect_equal(pc$n[pc$stage == "  Excluded: Animal study"], 1)
  expect_equal(pc$n[pc$stage == "Included in review"], 1)
})

test_that("invalid extractions are flagged, saved, and fail import with clear messages", {
  p <- local_project()
  bad <- function(contents) { x <- good_extraction(contents); x$data_points[[1]]$group_id <- "g9"; x$data_points[[2]]$statistic_type <- "sd"; x }
  f <- tempfile(fileext = ".md"); writeLines("text", f)
  res <- suppressWarnings(suppressMessages(df_extract_batch(mock_chat(bad), c("10.1/x" = f), p)))
  expect_equal(res$n_problems, 2L)
  imp <- suppressMessages(df_import_batch("10.1/x", p))
  expect_equal(imp$status, "failed")
  expect_match(imp$error, "'g9' not defined")
  expect_match(imp$error, "'sd' is not one of")

  # fix by hand and import
  e <- df_read_cache("extraction", p)[["10.1/x"]]$result
  e$data_points[[1]]$group_id <- "g1"; e$data_points[[2]]$statistic_type <- "mean_sd"
  expect_length(df_save_extraction("10.1/x", e, p), 0)
  expect_equal(suppressMessages(df_import_batch("10.1/x", p))$status, "imported")
})

test_that("missing or unsupported full texts fail per study, not the batch", {
  p <- local_project()
  f <- tempfile(fileext = ".docx"); writeLines("x", f)
  res <- suppressMessages(df_extract_batch(mock_chat(good_extraction), c("a" = "/no/such.pdf", "b" = f), p))
  expect_equal(res$status, c("failed", "failed"))
  expect_match(res$error[1], "not found")
  expect_match(res$error[2], "Unsupported")
})
