# How DataFindR talks to a metawoRld project (metawoRld SPEC.md section 11).
#
# DataFindR is one input channel. It keeps its LLM results in its own cache,
# and writes to the project only through metawoRld functions:
#   - searches and records:  metawoRld::add_search(), metawoRld::add_records()
#   - screening decisions:   metawoRld::add_screening()
#   - extracted data:        metawoRld::submit_study() (a submission, never data/)

# Id of DataFindR in people.yml.
.tool_id <- function() getOption("DataFindR.tool_id", "datafindr")

.tool_string <- function() paste0("DataFindR@", utils::packageVersion("DataFindR"))

# Make sure DataFindR is registered as a tool in people.yml.
.ensure_tool <- function(proj) {
  if (!.tool_id() %in% metawoRld::read_people(proj)$id) {
    metawoRld::add_person(proj, .tool_id(), "DataFindR", tool = TRUE)
  }
  invisible(.tool_id())
}

# The person acting: explicit, else the metawoRld.person option.
.person <- function(person = NULL) person %||% getOption("metawoRld.person")

# Record id of an identifier in records.csv, adding the record if needed.
.ensure_record <- function(proj, id, search = "manual", meta = NULL) {
  meta <- meta %||% .cache_read(id, "metadata", proj) %||% list()
  doi <- tolower(as.character(meta$doi %||% NA))
  pmid <- as.character(meta$pmid %||% NA)
  if (metawoRld::identifier_type(id) == "doi") doi <- id
  if (metawoRld::identifier_type(id) == "pmid") pmid <- id
  doi[is.na(doi)] <- ""
  pmid[is.na(pmid)] <- ""
  rec <- metawoRld::read_records(proj)
  hit <- which((nzchar(doi) & rec$doi == doi) | (nzchar(pmid) & rec$pmid == pmid))
  if (length(hit)) return(rec$record_id[hit[1]])
  row <- data.frame(
    title = as.character(meta$title %||% id), doi = doi, pmid = pmid,
    year = as.character(meta$year %||% ""), journal = as.character(meta$journal %||% ""),
    authors = paste(unlist(meta$authors), collapse = "; "), stringsAsFactors = FALSE
  )
  row[is.na(row)] <- ""
  metawoRld::add_records(proj, row, search = search)$record_id[1]
}

# Map a screening-form decision to metawoRld's include/exclude/unsure.
.screening_decision <- function(decision) {
  d <- tolower(as.character(decision))
  ifelse(grepl("^incl", d), "include", ifelse(grepl("^excl", d), "exclude", "unsure"))
}

.log_screening <- function(proj, id, decision, reason, person, stage = "title_abstract") {
  rid <- .ensure_record(proj, id)
  reason <- as.character(reason %||% "")
  if (is.na(reason)) reason <- ""
  metawoRld::add_screening(proj, rid, stage = stage, decision = .screening_decision(decision),
                           reason = reason, person = person)
}

# Studies (accepted or submitted) and their reports, from study.yml files.
.report_index <- function(proj) {
  files <- c(Sys.glob(file.path(proj, "data", "*", "study.yml")),
             Sys.glob(file.path(proj, "submissions", "*", "*", "study.yml")))
  rows <- lapply(files, function(f) {
    y <- tryCatch(yaml::read_yaml(f), error = function(e) NULL)
    if (is.null(y) || !length(y$reports)) return(NULL)
    where <- if (basename(dirname(dirname(f))) == "data") "data" else "submission"
    dplyr::tibble(
      study_id = as.character(y$study_id %||% NA),
      doi = vapply(y$reports, function(r) as.character(r$doi %||% ""), character(1)),
      pmid = vapply(y$reports, function(r) as.character(r$pmid %||% ""), character(1)),
      where = where
    )
  })
  dplyr::bind_rows(c(list(dplyr::tibble(study_id = character(0), doi = character(0), pmid = character(0),
                                        where = character(0))), rows))
}

# Study id that a report (DOI/PMID) already belongs to, or NA.
.study_for_report <- function(index, doi = "", pmid = "", where = c("data", "submission")) {
  doi <- tolower(doi %||% ""); pmid <- as.character(pmid %||% "")
  doi[is.na(doi)] <- ""; pmid[is.na(pmid)] <- ""
  hit <- index$where %in% where & ((nzchar(doi) & index$doi == doi) | (nzchar(pmid) & index$pmid == pmid))
  if (any(hit)) index$study_id[hit][1] else NA_character_
}
