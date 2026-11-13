#' Fetch bibliographic metadata for DOIs and PMIDs
#'
#' Looks up title, abstract, authors, year, journal, DOI and PMID. PubMed is
#' tried first (structured abstracts are kept, with section labels); DOIs not
#' indexed in PubMed fall back to Crossref. Requests are batched and
#' rate-limited according to NCBI's rules.
#'
#' @param identifiers Character vector of DOIs and/or PMIDs. Prefixes such as
#'   `https://doi.org/` or `PMID:` are removed.
#' @param email Contact e-mail sent to NCBI and Crossref (recommended; NCBI
#'   may block anonymous heavy use). Defaults to the `DATAFINDR_EMAIL`
#'   environment variable.
#' @param api_key NCBI API key (allows 10 instead of 3 requests per second).
#'   Defaults to the `NCBI_API_KEY` environment variable.
#' @return A named list (names are normalized identifiers). Each element is a
#'   list with `identifier`, `doi`, `pmid`, `title`, `abstract`, `authors`,
#'   `year`, `journal`, `source` and `fetched`, or `NULL` if nothing was found.
#' @export
#' @examples
#' \dontrun{
#' df_fetch_metadata(c("31772108", "10.1038/nature14539"), email = "me@example.org")
#' }
df_fetch_metadata <- function(identifiers,
                              email = Sys.getenv("DATAFINDR_EMAIL"),
                              api_key = Sys.getenv("NCBI_API_KEY")) {
  ids <- unique(metawoRld::normalize_identifier(identifiers))
  ids <- ids[nzchar(ids)]
  out <- rlang::set_names(vector("list", length(ids)), ids)
  if (!length(ids)) return(out)
  type <- metawoRld::identifier_type(ids)
  if (any(type == "other")) {
    rlang::warn(paste0("Not a DOI or PMID, skipped: ", paste(ids[type == "other"], collapse = ", ")))
  }

  # PMIDs
  pmids <- ids[type == "pmid"]
  if (length(pmids)) {
    recs <- .pubmed_fetch(pmids, email, api_key)
    for (r in recs) if (r$pmid %in% pmids) out[[r$pmid]] <- c(list(identifier = r$pmid), r)
  }

  # DOIs: PubMed via DOI search, then Crossref for the rest
  dois <- ids[type == "doi"]
  if (length(dois)) {
    found <- .pubmed_pmids_for_dois(dois, email, api_key)
    if (length(found)) {
      recs <- .pubmed_fetch(unname(found), email, api_key)
      for (r in recs) {
        d <- tolower(r$doi %||% "")
        if (d %in% dois) out[[d]] <- c(list(identifier = d), r)
      }
    }
    for (d in dois[vapply(out[dois], is.null, logical(1))]) {
      r <- tryCatch(.crossref_fetch(d, email), error = function(e) {
        rlang::warn(paste0("Crossref lookup failed for ", d, ": ", conditionMessage(e)))
        NULL
      })
      if (!is.null(r)) out[[d]] <- c(list(identifier = d), r)
    }
  }

  missing <- names(out)[vapply(out, is.null, logical(1)) & type != "other"]
  if (length(missing)) rlang::warn(paste0("No metadata found for: ", paste(missing, collapse = ", ")))
  out
}

#' Search PubMed, log the search and register the records
#'
#' Runs a PubMed query, fetches metadata for every hit and stores it in the
#' metadata cache, ready for [df_assess_batch()]. The search is logged in the
#' project's `searches.yml` ([metawoRld::add_search()]) and every hit is
#' added to `records.csv` ([metawoRld::add_records()]), so PRISMA counts
#' include it. Records that are already cached are not fetched again, which
#' makes repeated searches cheap in a living review.
#'
#' @param query PubMed query string.
#' @param metawoRld_path Project root.
#' @param max_results Maximum number of records. Defaults to 10000.
#' @param mindate,maxdate Optional publication date limits (`"YYYY/MM/DD"`,
#'   `"YYYY/MM"` or `"YYYY"`).
#' @param person Id (from `people.yml`) of the person running the search.
#'   Defaults to the `metawoRld.person` option, else DataFindR itself.
#' @param search_id Id for the search log. Defaults to
#'   `pubmed-<date>-<time>`.
#' @inheritParams df_fetch_metadata
#' @return A tibble with `identifier` (PMID), `record_id`, `title`, `year`
#'   and `new` (`TRUE` if the record was not in the registry before).
#' @export
df_search_pubmed <- function(query,
                             metawoRld_path = ".",
                             max_results = 10000,
                             mindate = NULL,
                             maxdate = NULL,
                             person = NULL,
                             search_id = NULL,
                             email = Sys.getenv("DATAFINDR_EMAIL"),
                             api_key = Sys.getenv("NCBI_API_KEY")) {
  proj <- .check_project(metawoRld_path)
  if (!rlang::is_scalar_character(query) || !nzchar(query)) rlang::abort("`query` must be a single string.")
  params <- list(db = "pubmed", term = query, retmax = max_results, retmode = "json")
  if (!is.null(mindate) || !is.null(maxdate)) {
    params <- c(params, list(datetype = "pdat", mindate = mindate %||% "1800", maxdate = maxdate %||% "3000"))
  }
  res <- jsonlite::fromJSON(.eutils("esearch.fcgi", params, email, api_key))
  pmids <- as.character(unlist(res$esearchresult$idlist))
  total <- as.integer(res$esearchresult$count %||% length(pmids))
  if (total > length(pmids)) {
    rlang::warn(sprintf("Query matched %d records; only the first %d were retrieved (see `max_results`).", total, length(pmids)))
  }
  .ensure_tool(proj)
  person <- .person(person) %||% .tool_id()
  if (is.null(search_id)) {
    taken <- vapply(yaml::read_yaml(fs::path(proj, "searches.yml"))$searches %||% list(), function(s) as.character(s$id), character(1))
    base <- paste0("pubmed-", format(Sys.time(), "%Y%m%d-%H%M%S", tz = "UTC"))
    search_id <- base
    k <- 2
    while (search_id %in% taken) { search_id <- paste0(base, "-", k); k <- k + 1 }
  }
  q <- query
  if (!is.null(mindate) || !is.null(maxdate)) q <- paste0(q, "  [publication date ", mindate %||% "", " to ", maxdate %||% "", "]")
  metawoRld::add_search(proj, search_id, "PubMed", q, results = total, person = person)
  if (!length(pmids)) return(dplyr::tibble(identifier = character(0), record_id = character(0), title = character(0),
                                           year = integer(0), new = logical(0)))

  cached <- vapply(pmids, function(p) !is.null(.cache_read(p, "metadata", proj)), logical(1))
  if (any(!cached)) {
    recs <- .pubmed_fetch(pmids[!cached], email, api_key)
    for (r in recs) .cache_write(r$pmid, c(list(identifier = r$pmid), r), "metadata", proj)
  }
  meta <- lapply(pmids, function(p) .cache_read(p, "metadata", proj))
  chr <- function(f) vapply(meta, function(m) { v <- m[[f]]; if (is.null(v) || all(is.na(v))) "" else paste(unlist(v), collapse = "; ") }, character(1))
  recs <- data.frame(pmid = pmids, doi = chr("doi"), title = chr("title"), year = chr("year"),
                     journal = chr("journal"), authors = chr("authors"), stringsAsFactors = FALSE)
  recs$title[!nzchar(recs$title)] <- paste0("PMID ", recs$pmid[!nzchar(recs$title)])
  reg <- metawoRld::add_records(proj, recs, search = search_id)
  rlang::inform(sprintf("%d records (%d new to the registry) logged as search '%s'.", length(pmids), sum(reg$new), search_id))
  dplyr::tibble(
    identifier = pmids,
    record_id = reg$record_id,
    title = recs$title,
    year = vapply(meta, function(m) as.integer(m$year %||% NA_integer_), integer(1)),
    new = reg$new
  )
}

#' Add metadata for a study by hand
#'
#' Stores title, abstract, etc. in the metadata cache, e.g. for records that
#' are not in PubMed or Crossref, or to correct a fetched abstract.
#'
#' @param identifier Study identifier.
#' @param metadata Named list with at least `title`; ideally also `abstract`,
#'   `authors`, `year`, `journal`, `doi` and `pmid`.
#' @param metawoRld_path Project root.
#' @return The cache file path, invisibly.
#' @export
df_add_metadata <- function(identifier, metadata, metawoRld_path = ".") {
  proj <- .check_project(metawoRld_path)
  id <- metawoRld::normalize_identifier(identifier)
  if (!is.list(metadata) || is.null(metadata$title)) rlang::abort("`metadata` must be a list with at least `title`.")
  metadata$identifier <- id
  metadata$source <- metadata$source %||% "manual"
  metadata$fetched <- metadata$fetched %||% as.character(Sys.Date())
  invisible(.cache_write(id, metadata, "metadata", proj))
}

# ---- metadata from cache or web ---------------------------------------------------

# Returns named list of metadata (NULL where unavailable), fetching and caching
# anything missing.
.get_metadata <- function(ids, proj, force = FALSE, email = "", api_key = "") {
  out <- rlang::set_names(vector("list", length(ids)), ids)
  if (!force) for (id in ids) out[id] <- list(.cache_read(id, "metadata", proj))
  todo <- ids[vapply(out, is.null, logical(1))]
  if (length(todo)) {
    fetched <- df_fetch_metadata(todo, email = email, api_key = api_key)
    for (id in names(fetched)) {
      if (!is.null(fetched[[id]])) {
        .cache_write(id, fetched[[id]], "metadata", proj)
        out[id] <- list(fetched[[id]])
      }
    }
  }
  out
}

# ---- HTTP ---------------------------------------------------------------------

.http_get <- function(url, query = list(), realm = NULL, rate = 3) {
  req <- httr2::request(url) |>
    httr2::req_url_query(!!!Filter(function(v) !is.null(v) && !identical(v, ""), query)) |>
    httr2::req_user_agent("DataFindR (https://github.com/andjar/DataFindR)") |>
    httr2::req_retry(max_tries = 4) |>
    httr2::req_timeout(60)
  if (!is.null(realm)) req <- httr2::req_throttle(req, rate = rate, realm = realm)
  httr2::resp_body_string(httr2::req_perform(req))
}

.eutils <- function(endpoint, params, email, api_key) {
  rate <- if (nzchar(api_key %||% "")) 10 else 3
  .http_get(
    paste0("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/", endpoint),
    c(params, list(tool = "DataFindR", email = email, api_key = api_key)),
    realm = "ncbi-eutils", rate = rate
  )
}

.pubmed_fetch <- function(pmids, email, api_key, chunk = 200) {
  chunks <- split(pmids, ceiling(seq_along(pmids) / chunk))
  unlist(lapply(chunks, function(ids) {
    xml <- .eutils("efetch.fcgi", list(db = "pubmed", id = paste(ids, collapse = ","), retmode = "xml"), email, api_key)
    .parse_pubmed_xml(xml)
  }), recursive = FALSE, use.names = FALSE) %||% list()
}

.pubmed_pmids_for_dois <- function(dois, email, api_key, chunk = 50) {
  chunks <- split(dois, ceiling(seq_along(dois) / chunk))
  pm <- unlist(lapply(chunks, function(d) {
    term <- paste0('"', d, '"[DOI]', collapse = " OR ")
    res <- tryCatch(
      jsonlite::fromJSON(.eutils("esearch.fcgi", list(db = "pubmed", term = term, retmax = length(d) * 2, retmode = "json"), email, api_key)),
      error = function(e) NULL
    )
    as.character(unlist(res$esearchresult$idlist))
  }))
  unique(pm)
}

.parse_pubmed_xml <- function(xml) {
  doc <- xml2::read_xml(xml)
  arts <- xml2::xml_find_all(doc, "//PubmedArticle")
  lapply(arts, function(a) {
    txt <- function(xp) {
      n <- xml2::xml_find_first(a, xp)
      if (inherits(n, "xml_missing")) NA_character_ else trimws(xml2::xml_text(n))
    }
    abs_nodes <- xml2::xml_find_all(a, ".//Article/Abstract/AbstractText")
    abstract <- if (length(abs_nodes)) {
      labels <- xml2::xml_attr(abs_nodes, "Label")
      parts <- trimws(xml2::xml_text(abs_nodes))
      paste(ifelse(is.na(labels), parts, paste0(labels, ": ", parts)), collapse = "\n")
    } else NA_character_
    authors <- vapply(xml2::xml_find_all(a, ".//Article/AuthorList/Author"), function(au) {
      coll <- xml2::xml_find_first(au, "CollectiveName")
      if (!inherits(coll, "xml_missing")) return(xml2::xml_text(coll))
      trimws(paste(xml2::xml_text(xml2::xml_find_first(au, "LastName")),
                   xml2::xml_text(xml2::xml_find_first(au, "Initials"))))
    }, character(1))
    year <- txt(".//Article/Journal/JournalIssue/PubDate/Year")
    if (is.na(year)) year <- substr(txt(".//Article/Journal/JournalIssue/PubDate/MedlineDate"), 1, 4)
    doi <- txt(".//PubmedData/ArticleIdList/ArticleId[@IdType='doi']")
    if (is.na(doi)) doi <- txt(".//Article/ELocationID[@EIdType='doi']")
    list(
      pmid = txt(".//MedlineCitation/PMID"),
      doi = if (is.na(doi)) NA_character_ else tolower(doi),
      title = txt(".//Article/ArticleTitle"),
      abstract = abstract,
      authors = authors,
      year = suppressWarnings(as.integer(year)),
      journal = txt(".//Article/Journal/Title"),
      source = "pubmed",
      fetched = as.character(Sys.Date())
    )
  })
}

.crossref_fetch <- function(doi, email) {
  body <- .http_get(paste0("https://api.crossref.org/works/", utils::URLencode(doi)),
                    list(mailto = email), realm = "crossref", rate = 10)
  m <- jsonlite::fromJSON(body, simplifyVector = FALSE)$message
  authors <- vapply(m$author %||% list(), function(a) {
    trimws(paste(a$family %||% a$name %||% "", a$given %||% ""))
  }, character(1))
  parts <- m$issued$`date-parts`[[1]] %||% m$published$`date-parts`[[1]]
  abstract <- m$abstract %||% NA_character_
  if (!is.na(abstract)) abstract <- trimws(gsub("\\s+", " ", gsub("<[^>]+>", " ", abstract)))
  list(
    pmid = NA_character_,
    doi = tolower(m$DOI %||% doi),
    title = trimws(unlist(m$title)[1] %||% NA_character_),
    abstract = abstract,
    authors = authors,
    year = if (length(parts)) as.integer(parts[[1]]) else NA_integer_,
    journal = unlist(m$`container-title`)[1] %||% NA_character_,
    source = "crossref",
    fetched = as.character(Sys.Date())
  )
}
