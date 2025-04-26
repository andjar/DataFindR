#' @title Get API Key from Environment Variable
#'
#' @description
#' Retrieves an API key for a specified service from environment variables.
#' It checks for `SERVICE_API_KEY` (e.g., `OPENAI_API_KEY`). If the key is not
#' found or is empty, it stops with an informative error message guiding the
#' user to set the environment variable using `usethis::edit_r_environ()`.
#'
#' @param service Character string. The name of the service (e.g., "openai",
#'   "anthropic", "google"). Case-insensitive.
#'
#' @return The API key as a character string.
#' @noRd
#' @keywords internal
#'
#' @importFrom rlang abort inform glue
#' @importFrom tools R_user_dir
.get_api_key <- function(service) {
  # Construct the environment variable name (e.g., OPENAI_API_KEY)
  var_name <- paste0(toupper(service), "_API_KEY")

  api_key <- Sys.getenv(var_name)

  if (api_key == "") {
    # Provide helpful error message
    usethis_cmd_proj <- glue::glue('usethis::edit_r_environ("project")')
    usethis_cmd_user <- glue::glue('usethis::edit_r_environ()')
    config_file_proj <- ".Renviron in your project directory"
    config_file_user <- ".Renviron in your home directory"

    rlang::abort(
      c(
        glue::glue("API key for service '{service}' not found."),
        "i" = glue::glue("Please set the environment variable '{var_name}'."),
        "i" = glue::glue("You can add '{var_name}=YOUR_API_KEY_HERE' to:"),
        " " = glue::glue(" - {config_file_proj} (using `{usethis_cmd_proj}`) for this project only, OR"),
        " " = glue::glue(" - {config_file_user} (using `{usethis_cmd_user}`) for all projects."),
        "i" = "Make sure to **restart your R session** after modifying .Renviron for changes to take effect.",
        "i" = glue::glue("Ensure the '{config_file_proj}' or your global gitignore prevents committing the key.")
      )
    )
  }

  # Optional: Add a message indicating the key was found (for debugging?)
  # rlang::inform(glue::glue("Using API key found for '{service}' from environment variable '{var_name}'."))

  return(api_key)
}

# Example internal usage (would be called within API request functions)
# .call_openai_api <- function(prompt) {
#   api_key <- .get_api_key("openai")
#   # ... construct httr2 request using api_key ...
# }
