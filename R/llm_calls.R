#' Make an API Call to an LLM Service (OpenAI Example)
#'
#' @param prompt The prompt string to send to the LLM.
#' @param service Currently only supports "openai".
#' @param model The specific model name (e.g., "gpt-4-turbo", "gpt-3.5-turbo").
#' @param temperature Model temperature (0-1). Lower is more deterministic.
#' @param max_tokens Maximum tokens to generate in the response.
#' @param instructions System message/instructions (optional).
#' @param response_format Specify "json_object" if the response MUST be JSON.
#' @param timeout Request timeout in seconds.
#'
#' @return A list containing the parsed JSON response from the API (typically
#'   with `$choices[[1]]$message$content`) or raises an error on failure.
#' @noRd
#' @keywords internal
#'
#' @import httr2
#' @import jsonlite
#' @import rlang
#' @import glue
.call_llm_openai <- function(prompt,
                             model = "gpt-4-turbo", # Or a reasonable default
                             instructions = "You are a helpful assistant.",
                             temperature = 0.2, # Lower temp for factual extraction
                             max_tokens = 4000,
                             response_format = NULL, # Set to list(type = "json_object") for JSON mode
                             timeout = 180 # 3 minutes, extraction can be long
) {

  api_key <- .get_api_key("openai") # Uses the function from api_keys.R
  api_url <- "https://api.openai.com/v1/chat/completions"

  headers <- list(
    `Authorization` = paste("Bearer", api_key),
    `Content-Type` = "application/json"
  )

  # Construct message list
  messages <- list(
    list(role = "system", content = instructions),
    list(role = "user", content = prompt)
  )

  # Construct body
  body <- list(
    model = model,
    messages = messages,
    temperature = temperature,
    max_tokens = max_tokens
    # Potential future options: seed, top_p etc.
  )

  # Add response_format if requested (for models supporting JSON mode)
  if (!is.null(response_format) && response_format == "json_object") {
    body$response_format <- list(type = "json_object")
    # Also update system prompt to mention JSON output explicitly
    # This is crucial for JSON mode to work reliably
    body$messages[[1]]$content <- paste(
      instructions,
      "Your response MUST be a single, valid JSON object, and nothing else. Do not include ```json markdown delimiters."
    )
  }

  # Build request
  req <- httr2::request(api_url) |>
    httr2::req_headers(!!!headers) |>
    httr2::req_body_json(body) |>
    httr2::req_timeout(timeout) |>
    httr2::req_retry(max_tries = 3, backoff = ~ 2^.x) # Basic exponential backoff
  # Add user agent?
  # httr2::req_user_agent("DataFindR R Package (github.com/your_repo)")


  # Perform request and handle errors
  rlang::inform(glue::glue("Sending request to OpenAI model '{model}'..."))
  resp <- tryCatch({
    httr2::req_perform(req)
  }, error = function(e) {
    # Try to extract more detail from httr2 errors if possible
    http_error_detail <- NULL
    if(inherits(e, "httr2_http_error")) {
      http_error_detail <- tryCatch({
        httr2::resp_body_string(e$resp)
      }, error = function(e2){ "Could not retrieve error body."})
    }
    rlang::abort(c(
      glue::glue("LLM API request failed."),
      "!" = glue::glue("Original error: {e$message}"),
      if(!is.null(http_error_detail)) c("!" = glue::glue("API Response Body: {http_error_detail}"))
    ), parent = e)
  })

  # Check response status explicitly (redundant with tryCatch on httr2_http_error, but safe)
  if (httr2::resp_is_error(resp)) {
    # This part might be less likely to be reached due to req_perform erroring first
    error_body <- tryCatch(httr2::resp_body_json(resp), error = function(e) httr2::resp_body_string(resp))
    rlang::abort(glue::glue("LLM API request returned error status {httr2::resp_status(resp)}: {httr2::resp_status_desc(resp)}\nResponse body: {error_body}"))
  }

  # Parse successful response
  resp_body <- tryCatch({
    httr2::resp_body_json(resp, simplifyVector = FALSE) # Keep list structure
  }, error = function(e) {
    rlang::abort(c("Failed to parse JSON response from LLM API.",
                   "i" = glue::glue("Response Body: {httr2::resp_body_string(resp)}")),
                 parent = e)
  })

  rlang::inform("LLM response received successfully.")
  return(resp_body)
}

# Example internal wrapper for different services
# .call_llm_api <- function(prompt, service = "openai", ...) {
#   if(service == "openai") {
#      .call_llm_openai(prompt, ...)
#   } else if (service == "anthropic") {
#      # .call_llm_anthropic(prompt, ...) # TODO
#   } else {
#      rlang::abort("Unsupported LLM service specified.")
#   }
# }

# Example helper to extract content easily
# .get_llm_content <- function(resp_body, service = "openai") {
#   if(service == "openai") {
#      return(resp_body$choices[[1]]$message$content)
#   } # else if ...
# }
