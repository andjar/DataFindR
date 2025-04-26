# DataFindR: AI-Assisted Literature Screening and Extraction

<!-- badges: start -->
[![Conceptual Overview](https://img.shields.io/badge/Overview-Conceptual%20Pipeline%20(metawoRld)-blue)](https://yourusername.github.io/metawoRld/articles/conceptual_overview.html) <!-- Link to metawoRld's vignette -->
<!-- badges: end -->

The `DataFindR` package provides tools leveraging Large Language Models (LLMs) to semi-automate the screening and data extraction steps for systematic reviews. It is designed to populate a project created and managed by the `metawoRld` package.

Key features include:
*   Fetching metadata (Title/Abstract) for DOIs/PMIDs.
*   Assessing study relevance using LLMs guided by project criteria stored in `metawoRld`.
*   Extracting specific data points from full text using LLMs guided by a data schema stored in `metawoRld`.
*   Caching results to avoid redundant API calls and costs.
*   An interactive Shiny app for screening assistance.

**Please see the [Conceptual Overview vignette hosted by the `metawoRld` package](https://andjar.github.io/metawoRld/articles/conceptual_overview.html) for a detailed explanation of the entire living review pipeline and how `DataFindR` fits in.** (Replace link with actual URL).

## Installation

```r
# Assuming DataFindR is on GitHub (replace 'andjar/DataFindR')
# install.packages("remotes")
remotes::install_github("andjar/DataFindR")

# DataFindR requires metawoRld
# remotes::install_github("andjar/metawoRld")
```

## Example: Assessing a Paper

```r
library(DataFindR)

# --- Prerequisites ---
# 1. API key set (e.g., OPENAI_API_KEY in .Renviron, then restart R)
# 2. A metawoRld project exists at the specified path
# Example metawoRld setup (see metawoRld documentation):
proj_path <- file.path(tempdir(), "assess_example_proj")
if(!dir.exists(proj_path)) {
   try(metawoRld::create_metawoRld(proj_path, "Assess Example", "Desc",
      inclusion_criteria=c("Human","Pregnancy","Cytokine"), exclusion_criteria=c("Animal","Review")))
}


# --- Assess a study ---
pmid <- "31772108" # Example relevant PMID
if(dir.exists(proj_path)) {
   assessment_result <- tryCatch(
     df_assess_relevance(
        identifier = pmid,
        metawoRld_path = proj_path,
        email = "your.email@example.com" # Replace if needed
     ),
     error = function(e) print(paste("Assessment failed:", e$message))
   )
   print(assessment_result)
} else {
   print("Skipping assessment example: metawoRld project setup failed.")
}


# Or use the Shiny App
# df_shiny_assess()

# --- Clean up ---
# unlink(proj_path, recursive = TRUE)
```
