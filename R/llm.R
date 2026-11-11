# Thin wrappers around ellmer so the rest of the package (and the tests) have
# one place that talks to the LLM.

#' Convert a metawoRld schema to an ellmer type specification
#'
#' @param schema A schema list (e.g. from [metawoRld::get_schema()]) or the
#'   path to a schema YAML file.
#' @return An ellmer type object for `chat$chat_structured()` and friends.
#' @export
df_schema_to_ellmer <- function(schema) {
  if (rlang::is_scalar_character(schema)) schema <- yaml::read_yaml(schema)
  convert <- function(node, path) {
    type <- node$`_type`
    if (is.null(type)) rlang::abort(paste0("Schema node '", path, "' has no `_type`."))
    desc <- node$`_description`
    req <- !identical(node$`_required`, FALSE)
    switch(
      type,
      object = {
        fields <- names(node)[!startsWith(names(node), "_")]
        props <- lapply(fields, function(f) convert(node[[f]], paste0(path, ".", f)))
        names(props) <- fields
        do.call(ellmer::type_object, c(list(.description = desc, .required = req), props))
      },
      array = {
        if (is.null(node$`_items`)) rlang::abort(paste0("Array '", path, "' has no `_items`."))
        ellmer::type_array(items = convert(node$`_items`, paste0(path, "[]")), description = desc, required = req)
      },
      string = ellmer::type_string(description = desc, required = req),
      integer = ellmer::type_integer(description = desc, required = req),
      number = ellmer::type_number(description = desc, required = req),
      boolean = ellmer::type_boolean(description = desc, required = req),
      enum = ellmer::type_enum(values = as.character(unlist(node$`_values`)), description = desc, required = req),
      rlang::abort(paste0("Unsupported `_type` '", type, "' at '", path, "'."))
    )
  }
  convert(schema, "root")
}

.model_name <- function(chat) {
  if (is.character(chat)) return(chat)
  model <- tryCatch(chat$get_model(), error = function(e) NA_character_)
  provider <- tryCatch(chat$get_provider()@name, error = function(e) NA_character_)
  paste0(provider %||% "?", "/", model %||% "?")
}

.as_chat <- function(chat) {
  if (is.character(chat)) return(ellmer::chat(chat))
  if (!is.environment(chat) || is.null(chat$chat_structured)) {
    rlang::abort("`chat` must be an ellmer chat object (e.g. `ellmer::chat_openai()`) or a string like \"openai/gpt-4.1\".")
  }
  chat
}

# One structured request on a fresh copy of `chat`.
# Returns list(result, usage) or signals an error.
.llm_structured <- function(chat, contents, type, convert = FALSE) {
  chat <- chat$clone()
  chat$set_turns(list())
  result <- do.call(chat$chat_structured, c(contents, list(type = type, convert = convert)))
  usage <- tryCatch({
    tok <- chat$get_tokens()
    last <- tok[nrow(tok), ]
    list(input_tokens = as.numeric(last$input), output_tokens = as.numeric(last$output),
         cost = as.numeric(chat$get_cost("last")))
  }, error = function(e) list())
  list(result = result, usage = usage)
}

# Many requests at once. `mode` is "parallel" (fast, same price) or "batch"
# (provider batch API: ~50% cheaper, can take up to 24h; resumable via `path`).
# Returns list(results = list (NULL = failed), usage = list, errors = chr)
# or NULL if a batch is still running and wait = FALSE.
.llm_many <- function(chat, prompts, type, mode = c("parallel", "batch"), flat = TRUE,
                      max_active = 10, path = NULL, wait = TRUE) {
  mode <- match.arg(mode)
  chat <- chat$clone()
  chat$set_turns(list())
  out <- if (mode == "parallel") {
    ellmer::parallel_chat_structured(chat, prompts, type = type, convert = flat,
                                     include_tokens = flat, include_cost = flat,
                                     max_active = max_active, on_error = "continue")
  } else {
    ellmer::batch_chat_structured(chat, prompts, path = path, type = type, wait = wait,
                                  convert = flat, include_tokens = flat, include_cost = flat)
  }
  if (is.null(out)) return(NULL)
  .normalize_many(out, length(prompts))
}

.normalize_many <- function(out, n) {
  if (is.data.frame(out)) {
    err <- if (".error" %in% names(out)) out$.error else vector("list", n)
    usage_cols <- intersect(c("input_tokens", "output_tokens", "cost"), names(out))
    fields <- setdiff(names(out), c(".error", "input_tokens", "output_tokens", "cached_input_tokens", "cost"))
    results <- lapply(seq_len(n), function(i) {
      if (!is.null(err[[i]])) return(NULL)
      lapply(out[i, fields, drop = FALSE], function(col) if (is.list(col)) col[[1]] else col[[1]])
    })
    usage <- lapply(seq_len(n), function(i) lapply(out[i, usage_cols, drop = FALSE], function(v) as.numeric(v[[1]])))
    errors <- vapply(seq_len(n), function(i) {
      e <- err[[i]]
      if (is.null(e)) NA_character_ else if (inherits(e, "condition")) conditionMessage(e) else "Request failed"
    }, character(1))
  } else {
    results <- out
    usage <- rep(list(list()), n)
    errors <- ifelse(vapply(out, is.null, logical(1)), "Request failed", NA_character_)
  }
  list(results = results, usage = usage, errors = errors)
}
