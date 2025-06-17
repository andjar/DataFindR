#' General Utility Functions for DataFindR
#'
#' This file contains miscellaneous utility functions that support the DataFindR package,
#' such as copying template files.
#' @title Copy templates for DataFindR
#'
#' @description
#' Imports templates for prompts and schemas
#'
#' @param path Character vector. Target path
#' @param overwrite Boolean. Default `FALSE`
#'
#' @return No return value, but copies files to the specified path
#'
#' @export
copy_datafindr_files <- function(path, overwrite = FALSE) {

  files_to_copy <- c(
    system.file(fs::path("prompts", "_extraction_prompt.txt"), package = "DataFindR"),
    system.file(fs::path("prompts", "_assessment_prompt.txt"), package = "DataFindR"),
    system.file(fs::path("prompts", "_extraction_schema.yml"), package = "DataFindR"),
    system.file(fs::path("prompts", "_assessment_schema.yml"), package = "DataFindR")
  )

  # Copy files to the destination directory
  for (file in files_to_copy) {
    dest_file <- file.path(path, basename(file))
    if (!file.exists(dest_file) || overwrite) {
      file.copy(file, dest_file, overwrite = overwrite)
    }
  }

  message("Files copied successfully from DataFindR.")

}
