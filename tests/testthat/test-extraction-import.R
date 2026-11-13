test_that("search -> screen -> extract -> submit -> accept, with registry, provenance and PRISMA", {
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

  # the search and screening are logged in the project
  s <- yaml::read_yaml(file.path(p, "searches.yml"))$searches
  expect_equal(s[[1]]$results, 2L)
  expect_equal(nrow(metawoRld::read_records(p)), 2)
  scr <- read.csv(file.path(p, "screening.csv"), colClasses = "character")
  expect_equal(unique(scr$person), "datafindr")
  expect_equal(sort(scr$decision), c("exclude", "include"))

  imp <- suppressMessages(df_import_batch(metawoRld_path = p))
  expect_equal(imp$status, "submitted")
  expect_true(metawoRld::is_study_id(imp$study_id))
  expect_equal(metawoRld::list_studies(p), character(0))              # waits for a person
  sub <- metawoRld::read_submission(p, imp$study_id, imp$extraction_id)$submission
  expect_equal(sub$person, "datafindr")
  expect_equal(sub$channel, "datafindr")
  expect_match(sub$model, "mock-model")
  expect_match(sub$source_sha256, "^sha256:")
  expect_match(sub$tool, "^DataFindR@")
  expect_equal(suppressMessages(df_import_batch(metawoRld_path = p))$identifier, character(0))  # nothing new

  suppressMessages(metawoRld::accept_submission(p, imp$study_id, imp$extraction_id, person = "ahj"))
  w <- suppressMessages(metawoRld::load_metawoRld(p))
  expect_equal(w$studies_metadata[[1]]$title, "Biomarker X in pregnant women: a cohort study.")  # fetched title wins
  expect_equal(w$studies_data$group_name, c("Cases", "Controls"))
  rec <- metawoRld::read_records(p)
  expect_equal(rec$study_id[rec$pmid == "11111111"], imp$study_id)   # record linked on acceptance

  st <- df_status(p)
  expect_true(st$submitted[st$identifier == "11111111"])
  expect_true(st$accepted[st$identifier == "11111111"])
  expect_equal(st$study_id[st$identifier == "11111111"], imp$study_id)
  pc <- metawoRld::prisma_counts(p)
  expect_equal(pc$n[pc$stage == "Records screened (title/abstract)"], 2)
  expect_equal(pc$n[pc$stage == "Records excluded (title/abstract)"], 1)
  expect_equal(pc$n[pc$stage == "Studies included"], 1)
  expect_equal(nrow(suppressMessages(metawoRld::check_project(p))), 0)
})

test_that("invalid extractions are flagged, saved, and fail import with clear messages", {
  p <- local_project()
  bad <- function(contents) { x <- good_extraction(contents); x$data_points[[1]]$group_id <- "g9"; x$data_points[[2]]$statistic_type <- "sd"; x }
  f <- tempfile(fileext = ".md"); writeLines("text", f)
  res <- suppressWarnings(suppressMessages(df_extract_batch(mock_chat(bad), c("10.1/x" = f), p)))
  expect_equal(res$n_problems, 2L)
  imp <- suppressMessages(df_import_batch("10.1/x", p))
  expect_equal(imp$status, "failed")
  expect_equal(nrow(metawoRld::list_submissions(p)), 0)                 # refused at the gate
  expect_match(imp$error, "'g9' not defined")
  expect_match(imp$error, "'sd' is not one of")

  # fix by hand and import
  e <- df_read_cache("extraction", p)[["10.1/x"]]$result
  e$data_points[[1]]$group_id <- "g1"; e$data_points[[2]]$statistic_type <- "mean_sd"
  expect_length(df_save_extraction("10.1/x", e, p), 0)
  expect_equal(suppressMessages(df_import_batch("10.1/x", p))$status, "submitted")
})

test_that("missing or unsupported full texts fail per study, not the batch", {
  p <- local_project()
  f <- tempfile(fileext = ".docx"); writeLines("x", f)
  res <- suppressMessages(df_extract_batch(mock_chat(good_extraction), c("a" = "/no/such.pdf", "b" = f), p))
  expect_equal(res$status, c("failed", "failed"))
  expect_match(res$error[1], "not found")
  expect_match(res$error[2], "Unsupported")
})

test_that("the project vocabulary reaches the prompt and synonyms are canonicalized on import", {
  p <- local_project()
  metawoRld::add_variable_synonym(p, "X-level", "Biomarker X", unit = "pg/mL")
  r <- metawoRld::read_rules(p)
  r$group_categories <- list(Case = list(synonyms = list("Cases")), Control = list(description = "Healthy pregnant women"))
  metawoRld:::.write_rules(file.path(p, "_rules.yml"), r)

  pr <- df_extraction_prompt("10.1/x", p, metadata = list(title = "T"))
  expect_match(pr, "- Biomarker X (X-level); unit: pg/mL", fixed = TRUE)
  expect_match(pr, "- Control: Healthy pregnant women", fixed = TRUE)
  expect_false(grepl("{{", pr, fixed = TRUE))

  syn <- function(contents) { x <- good_extraction(contents); x$data_points[[1]]$variable_name <- "x-level"; x }
  f <- tempfile(fileext = ".txt"); writeLines("text", f)
  suppressMessages(df_extract_batch(mock_chat(syn), c("10.1/x" = f), p))
  imp <- suppressMessages(df_import_batch("10.1/x", p))
  d <- metawoRld::read_submission(p, imp$study_id, imp$extraction_id)$data
  expect_equal(d$variable_name, c("Biomarker X", "X"))
  expect_equal(d$notes[1], "Reported as 'x-level'.")
})
