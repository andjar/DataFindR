# DataFindR

LLM-assisted screening and data extraction for living systematic reviews
managed with [metawoRld](https://github.com/andjar/metawoRld).

* **Find**: search PubMed or look up DOIs/PMIDs; titles and structured
  abstracts are fetched from PubMed, with Crossref as a fallback.
* **Screen**: title/abstract decisions against the project's criteria, using
  any LLM supported by [ellmer](https://ellmer.tidyverse.org) (OpenAI,
  Anthropic, Gemini, local models, ...). Run requests in parallel, or through
  provider batch APIs at about half the price.
* **Extract**: structured data from full texts (PDF, text, HTML or JATS XML)
  following the project's extraction form.
* **Submit**: extractions go through metawoRld's one door
  (`metawoRld::submit_study()`) as submissions from the tool `datafindr`,
  with the model and the hash of the full text recorded. A person accepts or
  reconciles them; searches and screening decisions are logged in the
  project's registry, so PRISMA counts include them.
* **Review**: Shiny apps to check, correct and override LLM output.
* **Reproduce**: every result is cached in the project with the model, token
  use, cost and a fingerprint of the prompt, schema and criteria. Re-runs only
  process new or outdated work; manual decisions are never overwritten.

DataFindR is one input channel among several: it writes to the project only
through metawoRld's functions. Prompts, forms and criteria are files in the
metawoRld project, so you can edit them.

## Installation

```r
# install.packages("remotes")
remotes::install_github("andjar/DataFindR")   # also installs metawoRld
```

Set your LLM provider's API key (e.g. `OPENAI_API_KEY`, `ANTHROPIC_API_KEY`
or `GOOGLE_API_KEY`) in `.Renviron`. Optionally set `DATAFINDR_EMAIL` and
`NCBI_API_KEY` for faster, polite PubMed access.

## Workflow

```r
library(metawoRld)
library(DataFindR)

create_metawoRld("my_review", "Biomarker X in pregnancy",
                 "Living review of maternal serum levels of X.",
                 inclusion_criteria = c("Human pregnancy", "Serum or plasma X reported"),
                 exclusion_criteria = c("Animal study", "Review or case report"))
metawoRld::add_person("my_review", "ahj", "Anders Hagen Jarmund",
                      roles = c("admin", "screener", "extractor", "verifier", "arbiter"))
options(metawoRld.person = "ahj")

# 1. Records: logged in searches.yml and records.csv (re-run regularly; only new records are fetched)
hits <- df_search_pubmed('"biomarker X" AND pregnancy', "my_review")

# 2. Screening: the LLM's decisions are logged in screening.csv as the tool "datafindr"
chat <- ellmer::chat_openai(model = "gpt-4.1-mini")   # or ellmer::chat_ollama() for a local model
screen <- df_assess_batch(chat, hits$identifier, "my_review")   # mode = "batch" for half price
df_shiny_assess("my_review", chat)                               # check borderline cases

# 3. Extraction: put full texts in my_review/fulltext/ named df_fulltext_filename(id)
df_pending_extraction("my_review")
df_extract_batch(ellmer::chat_anthropic(), "my_review/fulltext", "my_review")
df_shiny_extract("my_review", fulltext_dir = "my_review/fulltext")

# 4. Submit through metawoRld's one door, then accept after review
sub <- df_import_batch(metawoRld_path = "my_review")
metawoRld::accept_submission("my_review", sub$study_id[1], sub$extraction_id[1])
df_status("my_review")
metawoRld::prisma_counts("my_review")
```

## Main functions

| Step | Functions |
|---|---|
| Records | `df_search_pubmed()`, `df_fetch_metadata()`, `df_add_metadata()` |
| Screening | `df_assess_batch()`, `df_assess_relevance()`, `df_save_assessment()`, `df_shiny_assess()` |
| Extraction | `df_extract_batch()`, `df_extract()`, `df_save_extraction()`, `df_shiny_extract()`, `df_find_fulltexts()` |
| Submit | `df_import_batch()`, `df_import_extraction()` |
| Progress and cache | `df_status()`, `df_pending_extraction()`, `df_read_cache()`, `df_clear_cache()` |
| Inspect prompts | `df_assessment_prompt()`, `df_extraction_prompt()`, `df_schema_to_ellmer()` |

LLM output can be wrong. Treat screening decisions and extracted values as
suggestions to be verified; the Shiny apps and the `source` field recorded
for each data point are there to make that quick.
