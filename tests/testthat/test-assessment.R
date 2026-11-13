seed_metadata <- function(p) {
  local_mocked_bindings(.http_get = fake_http, .env = parent.frame())
  suppressMessages(df_search_pubmed("q", p, email = "", api_key = ""))
}

test_that("sequential assessment validates, normalizes labels, caches and reuses", {
  local_mocked_bindings(.http_get = fake_http)
  p <- local_project()
  suppressMessages(df_search_pubmed("q", p, email = "", api_key = ""))
  chat <- mock_chat(include_all)
  res <- suppressMessages(df_assess_batch(chat, c("11111111", "22222222"), p, mode = "sequential"))
  expect_equal(res$status, c("assessed", "assessed"))
  expect_equal(res$decision, c("Include", "Exclude"))      # "exclude" normalized
  expect_equal(res$cost, c(0.002, 0.002))
  expect_equal(chat$n_calls, 2)

  e <- DataFindR:::.cache_get_entry("22222222", "assessment", p)
  expect_equal(e$provenance$source, "llm")
  expect_equal(e$provenance$input_tokens, 120)

  res2 <- suppressMessages(df_assess_batch(chat, c("11111111", "22222222"), p, mode = "sequential"))
  expect_equal(res2$status, c("cached", "cached"))
  expect_equal(chat$n_calls, 2)

  # changing the criteria makes cached results stale -> re-assessed
  cfg <- yaml::read_yaml(file.path(p, "_metawoRld.yml"))
  cfg$exclusion_criteria <- list("Animal study", "Case report")
  yaml::write_yaml(cfg, file.path(p, "_metawoRld.yml"))
  expect_true(all(df_status(p, chat)$assessment_stale))
  res3 <- suppressMessages(df_assess_batch(chat, "11111111", p, mode = "sequential"))
  expect_equal(res3$status, "assessed")
  expect_equal(chat$n_calls, 3)
})

test_that("manual decisions are kept and missing metadata is reported", {
  local_mocked_bindings(.http_get = fake_http)
  p <- local_project()
  suppressMessages(df_search_pubmed("q", p, email = "", api_key = ""))
  df_save_assessment("11111111", "Exclude", "Wrong sample type", exclusion_reason = "Wrong sample",
                     metawoRld_path = p, reviewer = "ahj")
  scr <- read.csv(file.path(p, "screening.csv"), colClasses = "character")
  expect_equal(scr$person, "ahj")
  expect_equal(scr$reason, "Wrong sample")
  expect_warning(df_save_assessment("22222222", "Include", metawoRld_path = p), "not logged")
  chat <- mock_chat(include_all)
  res <- suppressWarnings(suppressMessages(df_assess_batch(chat, c("11111111", "Smith2020"), p, mode = "sequential")))
  expect_equal(res$status, c("cached", "failed"))
  expect_equal(res$decision[1], "Exclude")
  expect_match(res$error[2], "No metadata")
  expect_error(df_save_assessment("1", "Maybe", metawoRld_path = p), "must be one of")
})

test_that("parallel mode uses ellmer::parallel_chat_structured and handles failures", {
  local_mocked_bindings(.http_get = fake_http)
  local_mocked_bindings(
    parallel_chat_structured = function(chat, prompts, type, ...) {
      data.frame(decision = c("Include", NA), score = c(0.8, NA), rationale = c("ok", NA),
                 exclusion_reason = c(NA, NA), input_tokens = c(10, 0), output_tokens = c(5, 0),
                 cost = c(0.001, 0)) |>
        transform(.error = I(list(NULL, simpleError("rate limited"))))
    },
    .package = "ellmer"
  )
  p <- local_project()
  suppressMessages(df_search_pubmed("q", p, email = "", api_key = ""))
  res <- suppressMessages(df_assess_batch(mock_chat(include_all), c("11111111", "22222222"), p))
  expect_equal(res$status, c("assessed", "failed"))
  expect_equal(res$error[2], "rate limited")
  expect_equal(res$input_tokens[1], 10)
})

test_that("df_assess_relevance returns the single result", {
  local_mocked_bindings(.http_get = fake_http)
  p <- local_project()
  r <- suppressMessages(df_assess_relevance(mock_chat(include_all), "11111111", p, email = "", api_key = ""))
  expect_equal(r$decision, "Include")
})
