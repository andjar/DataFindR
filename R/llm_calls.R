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

#' Make an API Call to the Google Gemini LLM Service (with File Support)
#'
#' @param prompt The prompt string to send to the LLM.
#' @param files Optional: A list (or single item) containing file information
#'   returned by `.upload_file_google`. Each item should be a list with `uri`
#'   and `mime_type` elements. Example: `list(list(uri="files/abc", mime_type="application/pdf"))`.
#' @param model The specific Gemini model name (e.g., "gemini-1.5-flash-latest",
#'   "gemini-1.5-pro-latest"). Ensure the model supports file input.
#' @param instructions System message/instructions (optional).
#' @param temperature Model temperature (0-1).
#' @param max_tokens Maximum *output* tokens.
#' @param response_format Specify "json_object" for JSON response.
#' @param timeout Request timeout in seconds.
#'
#' @return A list containing the parsed JSON response from the API.
#' @noRd
#' @keywords internal
#'
#' @import httr2
#' @import jsonlite
#' @import rlang
#' @import glue
.call_llm_google <- function(prompt,
                             files = NULL, # New argument for uploaded files
                             model = "gemini-1.5-flash-latest", # Model supporting files
                             instructions = NULL,
                             temperature = 0.2,
                             max_tokens = 4000,
                             response_format = NULL,
                             timeout = 180
) {

  api_key <- .get_api_key("google")
  if (is.null(api_key) || api_key == "") {
    rlang::abort("Google Gemini API key not found. Set the GOOGLE_API_KEY environment variable.")
  }

  # Check if model likely supports multimodal input (heuristic)
  if (!is.null(files) && !grepl("1\\.5|flash|pro", model)) {
    rlang::warn(glue::glue("Model '{model}' might not support file inputs. Consider using 'gemini-2.5-flash-latest' or 'gemini-2.5-pro-latest'."))
  }

  api_url <- glue::glue("https://generativelanguage.googleapis.com/v1beta/models/{model}:generateContent")

  headers <- list(`Content-Type` = "application/json")

  # --- Construct 'parts' dynamically ---
  # Start with the text prompt part
  prompt_part <- list(text = prompt)

  # Prepare file parts if files are provided
  file_parts <- list()
  if (!is.null(files)) {
    # Ensure 'files' is a list of lists, even if only one file is passed
    if (!is.list(files)) {
      rlang::abort("'files' argument must be a list.")
    }
    if (!is.null(names(files)) && all(c("uri", "mime_type") %in% names(files))) {
      # Looks like a single file object was passed directly, wrap it in a list
      files <- list(files)
    }

    file_parts <- lapply(files, function(file) {
      if (!is.list(file) || is.null(file$uri) || is.null(file$mime_type)) {
        rlang::abort("Each item in the 'files' list must be a list with 'uri' and 'mime_type' elements.")
      }
      list(fileData = list(mimeType = file$mime_type, fileUri = file$uri))
    })
  }

  # Combine parts: Text prompt should generally come first, then files.
  # However, the API supports interleaving. For simplicity, text then files.
  all_parts <- c(list(prompt_part), file_parts) # prompt_part first

  # --- Handle instructions and potential JSON mode instruction ---
  json_instruction <- NULL
  if (!is.null(response_format) && response_format == "json_object") {
    json_instruction <- "Your response MUST be a single, valid JSON object, and nothing else. Do not include ```json markdown delimiters or any other text outside the JSON object."
  }

  # Combine instructions, main prompt text, and JSON instruction (if any)
  # We modify the *first* text part.
  combined_text <- ""
  if (!is.null(instructions) && nzchar(instructions)) {
    combined_text <- paste(instructions, prompt, sep = "\n\n")
  } else {
    combined_text <- prompt
  }
  if (!is.null(json_instruction)) {
    combined_text <- paste(combined_text, json_instruction, sep = "\n\n")
  }
  all_parts[[1]]$text <- combined_text # Update the text in the first part


  # Construct the final request body contents
  body_contents <- list(
    # For multimodal, role is usually 'user'
    list(role = "user", parts = all_parts)
  )

  # Construct generationConfig
  generation_config <- list(
    temperature = temperature,
    maxOutputTokens = max_tokens
  )
  if (!is.null(response_format) && response_format == "json_object") {
    generation_config$responseMimeType <- "application/json"
  }

  # Construct the final request body
  body <- list(
    contents = body_contents,
    generationConfig = generation_config
    # safetySettings could be added here if needed
  )

  # --- Build and Perform Request (rest of the function is mostly the same) ---
  req <- httr2::request(api_url) |>
    httr2::req_url_query(key = api_key) |>
    httr2::req_headers(!!!headers) |>
    httr2::req_body_json(body) |>
    httr2::req_timeout(timeout) |>
    httr2::req_retry(max_tries = 3, backoff = ~ 2^.x)

  rlang::inform(glue::glue("Sending request to Google Gemini model '{model}' ({ifelse(is.null(files), 'text only', paste(length(files), 'file(s) included'))})..."))
  resp <- tryCatch({
    httr2::req_perform(req)
  }, error = function(e) {
    # ... (keep your existing error handling for req_perform) ...
    http_error_detail <- NULL
    if (inherits(e, "httr2_http_error")) {
      http_error_detail <- tryCatch({
        err_body <- httr2::resp_body_json(e$resp)
        paste(err_body$error$message %||% "No message field.",
              "Status:", err_body$error$status %||% "N/A",
              "Code:", err_body$error$code %||% "N/A")
      }, error = function(e2) { tryCatch(httr2::resp_body_string(e$resp), error = function(e3){ NA }) })
    }
    rlang::abort(c(
      glue::glue("LLM API request failed for Gemini model '{model}'."),
      "!" = glue::glue("Original error: {e$message}"),
      if (!is.null(http_error_detail) && !is.na(http_error_detail)) c("!" = glue::glue("API Response Detail: {http_error_detail}"))
    ), parent = e)
  })

  # ... (keep your existing response status check and parsing logic) ...
  if (httr2::resp_is_error(resp)) {
    error_body_content <- tryCatch({
      err_body <- httr2::resp_body_json(resp)
      paste(err_body$error$message %||% "No message field.",
            "Status:", err_body$error$status %||% "N/A",
            "Code:", err_body$error$code %||% "N/A")
    }, error = function(e) { tryCatch(httr2::resp_body_string(resp), error = function(e2){ NA }) })
    rlang::abort(glue::glue("LLM API request returned error status {httr2::resp_status(resp)}: {httr2::resp_status_desc(resp)}\nResponse detail: {error_body_content}"))
  }

  resp_body <- tryCatch({
    httr2::resp_body_json(resp, simplifyVector = FALSE)
  }, error = function(e) {
    resp_string <- tryCatch(httr2::resp_body_string(resp), error = function(e2) {"<Could not read response body>"})
    rlang::abort(c("Failed to parse JSON response from Gemini API.",
                   "i" = glue::glue("Response Body: {resp_string}")),
                 parent = e)
  })

  # ... (keep your existing safety checks / warnings) ...
  if (is.null(resp_body$candidates) || length(resp_body$candidates) == 0) {
    finish_reason <- resp_body$promptFeedback$blockReason %||% "Unknown reason (no candidates returned)"
    safety_ratings_json <- tryCatch(jsonlite::toJSON(resp_body$promptFeedback$safetyRatings, auto_unbox = TRUE, pretty = FALSE), error = function(e) "N/A")
    rlang::warn(glue::glue("Gemini API returned no candidates. Block Reason: {finish_reason}. Safety Ratings: {safety_ratings_json}"))
    # Check usage metadata even if blocked
    usage <- resp_body$usageMetadata
    if(!is.null(usage)) {
      rlang::inform(glue::glue("Usage: Prompt={usage$promptTokenCount %||% 'NA'}, Candidates={usage$candidatesTokenCount %||% 'NA'}, Total={usage$totalTokenCount %||% 'NA'}"))
    }
    return(resp_body) # Return body for inspection
  }
  candidate_finish_reason <- resp_body$candidates[[1]]$finishReason %||% "N/A"
  if(candidate_finish_reason != "STOP" && candidate_finish_reason != "MAX_TOKENS") {
    rlang::warn(glue::glue("Gemini generation finished for reason: {candidate_finish_reason} (Expected STOP or MAX_TOKENS)"))
  }
  # Add usage info
  usage <- resp_body$usageMetadata
  if(!is.null(usage)) {
    rlang::inform(glue::glue("Usage: Prompt={usage$promptTokenCount %||% 'NA'}, Candidates={usage$candidatesTokenCount %||% 'NA'}, Total={usage$totalTokenCount %||% 'NA'}"))
  }


  rlang::inform("Gemini LLM response received successfully.")
  return(resp_body)
}


# Example helper function to extract content (adapt based on actual Gemini response)
.get_llm_content_gemini <- function(resp_body) {
  # Check for safety blocks first
  if (is.null(resp_body$candidates) || length(resp_body$candidates) == 0) {
     warning("No candidates found in Gemini response, possibly blocked.")
     return(NULL)
  }
  # Check if parts exist
  if (is.null(resp_body$candidates[[1]]$content$parts) || length(resp_body$candidates[[1]]$content$parts) == 0) {
     warning("No parts found in the first candidate's content.")
     # Maybe return the finish reason or empty string?
     return(paste("Finished Reason:", resp_body$candidates[[1]]$finishReason %||% "N/A"))
  }
  # Extract text from the first part
  content <- resp_body$candidates[[1]]$content$parts[[1]]$text
  return(content)
}

#' Guess MIME Type from File Extension
#'
#' @param file_path Path to the file.
#' @param call Calling environment for error reporting.
#' @return Character string with the guessed MIME type.
#' @noRd
#' @keywords internal
#' @importFrom tools file_ext
#' @importFrom rlang caller_env abort inform warn
#' @importFrom glue glue
guess_mime_type <- function(file_path, call = rlang::caller_env()) {
  if (!file.exists(file_path)) {
    rlang::abort(glue::glue("File not found at path: '{file_path}'"), call = call)
  }

  ext <- tolower(tools::file_ext(file_path))

  # Expanded list based on Gemini supported types + common types
  mime_types <- list(
    # Images (subset supported by Gemini API, check latest docs)
    png = "image/png",
    jpeg = "image/jpeg",
    jpg = "image/jpeg",
    webp = "image/webp",
    heic = "image/heic",
    heif = "image/heif",
    # Audio (subset supported by Gemini API)
    mp3 = "audio/mp3",
    wav = "audio/wav",
    aiff = "audio/aiff",
    aac = "audio/aac",
    ogg = "audio/ogg",
    flac = "audio/flac",
    # Video (subset supported by Gemini API)
    mp4 = "video/mp4",
    mpeg = "video/mpeg",
    mov = "video/mov",
    avi = "video/avi",
    flv = "video/flv",
    mpg = "video/mpeg",
    webm = "video/webm",
    wmv = "video/wmv",
    "3gp" = "video/3gpp",
    # Text
    txt = "text/plain",
    css = "text/css",
    csv = "text/csv",
    html = "text/html",
    htm = "text/html",
    js = "application/javascript",
    json = "application/json",
    md = "text/markdown",
    py = "text/x-python",
    py3 = "text/x-python",
    rb = "text/x-ruby",
    rtf = "application/rtf",
    tsv = "text/tab-separated-values",
    xml = "application/xml",
    yaml = "text/yaml",
    yml = "text/yaml",
    # Documents
    pdf = "application/pdf",
    doc = "application/msword",
    docx = "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
    xls = "application/vnd.ms-excel",
    xlsx = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    ppt = "application/vnd.ms-powerpoint",
    pptx = "application/vnd.openxmlformats-officedocument.presentationml.presentation",
    # Code / Other
    c = "text/x-c",
    cpp = "text/x-c++",
    cs = "text/x-csharp",
    h = "text/x-c",
    hpp = "text/x-c++",
    java = "text/x-java",
    sh = "text/x-shellscript",
    sql = "application/sql",
    swift = "text/x-swift",
    tex = "application/x-tex",
    php = "application/x-httpd-php" # Or text/x-php
  )

  if (nzchar(ext) && !is.na(ext) && ext %in% names(mime_types)) {
    mime_types[[ext]]
  } else {
    # Fallback using the mime package if available, otherwise warn/error
    if (requireNamespace("mime", quietly = TRUE)) {
      guessed <- tryCatch(mime::guess_type(file_path), error = function(e) NULL)
      if (!is.null(guessed)) {
        rlang::inform(glue::glue("Guessed MIME type '{guessed}' for extension '{ext}' using 'mime' package."))
        return(guessed)
      }
    }
    rlang::abort(
      c(
        "x" = glue::glue("Couldn't determine MIME type for file '{basename(file_path)}'."),
        "i" = glue::glue("It has an unrecognized or missing file extension ('{ext}')."),
        "i" = "Please supply the `mime_type` argument manually.",
        "i" = "Check Google Gemini API documentation for supported file types."
      ),
      call = call
    )
  }
}

#' Check the Status of an Uploaded Google File
#'
#' Internal function to poll the status of a file being processed by Google.
#'
#' @param file_uri The full URI of the file (e.g., "https://generativelanguage.googleapis.com/v1beta/files/...")
#' @param api_key Google API Key.
#' @return A list containing the parsed JSON response body for the file status.
#' @noRd
#' @keywords internal
#' @import httr2
#' @import rlang
#' @import glue
.check_file_status_google <- function(file_uri, api_key) {
  req <- httr2::request(file_uri) |>
    httr2::req_url_query(key = api_key) |>
    httr2::req_retry(max_tries = 3) # Retry on transient errors

  resp <- tryCatch({
    httr2::req_perform(req)
  }, error = function(e) {
    # Extract details if possible
    http_error_detail <- NULL
    if (inherits(e, "httr2_http_error")) {
      http_error_detail <- tryCatch({
        err_body <- httr2::resp_body_json(e$resp)
        paste(err_body$error$message %||% "No message field.",
              "Status:", err_body$error$status %||% "N/A",
              "Code:", err_body$error$code %||% "N/A")
      }, error = function(e2) { httr2::resp_body_string(e$resp) })
    }
    rlang::abort(c(
      glue::glue("Failed to check file status for URI: {file_uri}"),
      "!" = glue::glue("Original error: {e$message}"),
      if (!is.null(http_error_detail)) c("!" = glue::glue("API Response Detail: {http_error_detail}"))
    ), parent = e)
  })

  # Check response status explicitly
  if (httr2::resp_is_error(resp)) {
    error_body_content <- tryCatch({
      err_body <- httr2::resp_body_json(resp)
      paste(err_body$error$message %||% "No message.", "Status:", err_body$error$status %||% "N/A")
    }, error = function(e) { httr2::resp_body_string(resp) })
    rlang::abort(glue::glue("File status check returned error {httr2::resp_status(resp)}: {error_body_content}"))
  }

  # Parse successful response
  resp_body <- tryCatch({
    httr2::resp_body_json(resp, simplifyVector = FALSE)
  }, error = function(e) {
    rlang::abort(c("Failed to parse JSON response from file status check.",
                   "i" = glue::glue("Response Body: {httr2::resp_body_string(resp)}")),
                 parent = e)
  })

  return(resp_body)
}

#' Upload a File to Google Gemini File API
#'
#' Uploads a file and waits for processing, returning info needed for prompts.
#' Uploaded files are currently deleted by Google after 2 days.
#'
#' @param path Path to the local file to upload.
#' @param mime_type Optional: Manually specify the MIME type. If NULL, it's guessed.
#' @param api_key Optional: Google API key. If NULL, uses `.get_api_key("google")`.
#' @param base_url Google Generative Language API base URL.
#' @param timeout Max time (seconds) to wait for file processing after upload.
#' @param poll_interval Interval (seconds) between status checks.
#'
#' @return A list containing the `uri` and `mime_type` of the successfully
#'   uploaded and processed file. Raises an error on failure.
#'   Example: `list(uri = "files/your-file-id", mime_type = "application/pdf")`
#' @noRd
#' @keywords internal
#' @import httr2
#' @import jsonlite
#' @import rlang
#' @import glue
#' @importFrom tools file_ext
.upload_file_google <- function(path,
                                mime_type = NULL,
                                api_key = NULL,
                                base_url = "https://generativelanguage.googleapis.com/",
                                timeout = 120, # 2 minutes total wait time
                                poll_interval = 2 # Check status every 2 seconds
) {

  if (is.null(api_key)) {
    api_key <- .get_api_key("google") # Assumes this helper exists
    if (is.null(api_key) || api_key == "") {
      rlang::abort("Google Gemini API key not found or empty.")
    }
  }

  if (!file.exists(path)) {
    rlang::abort(glue::glue("File not found: '{path}'"))
  }
  file_size <- file.size(path)
  display_name <- basename(path)

  # Max file size check (Google limit is often around 2GB, but API might have others)
  # Let's set a practical limit here, e.g., 1GB, can be adjusted
  if (file_size > 1 * 1024 * 1024 * 1024) {
    rlang::abort(glue::glue("File size ({round(file_size / 1024^2, 1)} MB) exceeds the practical limit of 1 GB for this function."))
  }
  if (file_size == 0) {
    rlang::abort(glue::glue("File '{display_name}' is empty (0 bytes)."))
  }


  mime_type <- mime_type %||% guess_mime_type(path)
  rlang::inform(glue::glue("Attempting to upload '{display_name}' ({round(file_size / 1024^2, 2)} MB) with MIME type '{mime_type}'..."))

  # === Step 1: Initiate Upload ===
  rlang::inform("Step 1: Initiating resumable upload...")
  init_url <- file.path(base_url, "upload/v1beta/files")

  init_req <- httr2::request(init_url) |>
    httr2::req_url_query(key = api_key) |>
    httr2::req_headers(
      "X-Goog-Upload-Protocol" = "resumable",
      "X-Goog-Upload-Command" = "start",
      "X-Goog-Upload-Header-Content-Length" = toString(file_size),
      "X-Goog-Upload-Header-Content-Type" = mime_type,
      "Content-Type" = "application/json" # Body is JSON
    ) |>
    httr2::req_body_json(list(file = list(display_name = display_name))) |>
    httr2::req_retry(max_tries = 2)

  init_resp <- tryCatch({
    httr2::req_perform(init_req)
  }, error = function(e) {
    http_error_detail <- NULL
    if (inherits(e, "httr2_http_error")) {
      http_error_detail <- tryCatch(httr2::resp_body_string(e$resp), error=function(e2) NA)
    }
    rlang::abort(c("Failed to initiate file upload.",
                   "!" = glue::glue("Original error: {e$message}"),
                   if(!is.null(http_error_detail) && !is.na(http_error_detail)) c("!"=glue::glue("API Response: {http_error_detail}"))
    ), parent = e)
  })

  if (httr2::resp_is_error(init_resp)) {
    error_body <- tryCatch(httr2::resp_body_string(init_resp), error=function(e) NA)
    rlang::abort(glue::glue("Upload initiation failed with status {httr2::resp_status(init_resp)}.\nResponse: {error_body}"))
  }

  upload_url <- httr2::resp_header(init_resp, "x-goog-upload-url")
  if (is.null(upload_url)) {
    rlang::abort("Did not receive 'x-goog-upload-url' header from initiation step.")
  }
  rlang::inform("Step 1: Success. Received upload URL.")


  # === Step 2: Send File Content ===
  rlang::inform("Step 2: Uploading file content...")
  send_req <- httr2::request(upload_url) |>
    httr2::req_url_query(key = api_key) |> # Key might be needed here too? Docs are unclear, safe to add.
    httr2::req_headers(
      "X-Goog-Upload-Offset" = "0",
      "X-Goog-Upload-Command" = "upload, finalize"
      # Content-Length is automatically added by req_body_file
      # Content-Type should also be automatically set based on mime_type by httr2 with req_body_file
    ) |>
    httr2::req_body_file(path, type = mime_type) |>
    # Add progress bar if desired (requires cli)
    # httr2::req_progress(type = "up") |>
    httr2::req_timeout(300) # 5 minutes for potentially large uploads

  send_resp <- tryCatch({
    httr2::req_perform(send_req)
  }, error = function(e) {
    http_error_detail <- NULL
    if (inherits(e, "httr2_http_error")) {
      http_error_detail <- tryCatch(httr2::resp_body_string(e$resp), error=function(e2) NA)
    }
    rlang::abort(c("Failed during file content upload.",
                   "!" = glue::glue("Original error: {e$message}"),
                   if(!is.null(http_error_detail) && !is.na(http_error_detail)) c("!"=glue::glue("API Response: {http_error_detail}"))
    ), parent = e)
  })

  if (httr2::resp_is_error(send_resp)) {
    error_body <- tryCatch(httr2::resp_body_string(send_resp), error=function(e) NA)
    rlang::abort(glue::glue("File content upload failed with status {httr2::resp_status(send_resp)}.\nResponse: {error_body}"))
  }

  # Parse response from sending data
  upload_result <- tryCatch({
    httr2::resp_body_json(send_resp)$file
  }, error = function(e) {
    rlang::abort(c("Failed to parse JSON response after sending file data.",
                   "i" = glue::glue("Response Body: {httr2::resp_body_string(send_resp)}")),
                 parent = e)
  })

  if (is.null(upload_result) || is.null(upload_result$uri)) {
    rlang::abort(glue::glue("Upload response did not contain expected file information (missing 'file' or 'file$uri').\nResponse Body: {httr2::resp_body_string(send_resp)}"))
  }
  rlang::inform("Step 2: Success. File content uploaded.")


  # === Step 3: Wait for Processing ===
  rlang::inform("Step 3: Waiting for Google to process the file...")
  file_api_uri_base <- file.path(base_url, "v1beta/") # Base for GETting file status
  file_status_uri <- paste0(file_api_uri_base, upload_result$uri) # Construct full GET URL

  start_time <- Sys.time()
  status <- upload_result # Initial status from the upload response

  while (status$state == "PROCESSING") {
    if (difftime(Sys.time(), start_time, units = "secs") > timeout) {
      rlang::abort(glue::glue("Timeout ({timeout}s) exceeded while waiting for file processing. Last state: PROCESSING. URI: {status$uri}"))
    }

    Sys.sleep(poll_interval)
    rlang::inform(glue::glue("Checking status for {status$uri}... (Elapsed: {round(difftime(Sys.time(), start_time, units = 'secs'))}s)"))
    status_check_resp <- .check_file_status_google(file_status_uri, api_key)
    status <- status_check_resp # Update status object
  }

  # Check final state
  if (status$state == "FAILED") {
    error_message <- status$error$message %||% "Unknown processing error."
    rlang::abort(glue::glue("File processing failed on Google's side. URI: {status$uri}. Error: {error_message}"))
  } else if (status$state != "ACTIVE") {
    rlang::abort(glue::glue("File processing ended in an unexpected state: '{status$state}'. URI: {status$uri}"))
  }

  rlang::inform(glue::glue("Step 3: Success. File '{display_name}' is ACTIVE. URI: {status$uri}"))

  # Return the necessary info for the generateContent call
  return(list(
    uri = status$uri, # e.g., "files/your-file-id"
    mime_type = status$mimeType %||% mime_type # Use confirmed type if available
  ))
}

