#' Construct a one-row structured error tibble
#'
#' Create a consistent tibble used across the package to represent
#' errors from validation, HTTP requests, or processing steps.
#'
#' The returned tibble has these columns:
#' - **context**: short label describing where the error occurred
#' - **error**: human-readable plain-text error message
#' - **status**: numeric HTTP status or NA
#' - **url**: request URL when applicable
#' - **params**: short summary of request parameters or other context
#' - **resp**: short snippet of the response body
#' - **timestamp**: POSIXct timestamp when the tibble was created
#'
#' @param context character short label for where the error came from
#' @param error character human-readable error message
#' @param status integer HTTP status code or NA (optional)
#' @param url character request URL (optional)
#' @param params character parameters summary (optional)
#' @param resp character response body snippet (optional)
#' @param max_resp_chars integer maximum characters to include from resp (default 400)
#'
#' @return one-row tibble with columns context, error, status, url, params, resp, timestamp
#' @noRd
cvd_error_tibble <- function(
  context,
  error,
  status = NA_integer_,
  url = NA_character_,
  params = NA_character_,
  resp = NA_character_,
  max_resp_chars = 400L
) {
  # Coerce inputs safely
  context <- if (is.null(context)) NA_character_ else as.character(context)
  error <- if (is.null(error)) NA_character_ else as.character(error)
  status <- if (is.null(status)) NA_integer_ else as.integer(status)
  url <- if (is.null(url)) NA_character_ else as.character(url)
  params <- if (is.null(params)) NA_character_ else as.character(params)
  resp <- if (is.null(resp)) NA_character_ else as.character(resp)

  # Truncate response snippet if too long
  resp_trunc <- if (!is.na(resp) && nzchar(resp)) {
    if (nchar(resp) > max_resp_chars) {
      paste0(substr(resp, 1, max_resp_chars), " [truncated]")
    } else {
      resp
    }
  } else {
    NA_character_
  }

  tibble::tibble(
    context = context,
    error = error,
    status = status,
    url = url,
    params = params,
    resp = resp_trunc,
    timestamp = as.POSIXct(Sys.time(), tz = "UTC")
  )
}


#' Safe API call with optional processing
#'
#' @param req httr2 request object or URL string
#' @param process_fn function(parsed_json) -> processed object, or NULL
#' @param context character label for error tibble
#' @param timeout numeric seconds for request
#' @return list(success = TRUE, result = ...) or list(success = FALSE, tibble = ...)
#' @noRd
safe_api_call <- function(
  req,
  process_fn = NULL,
  context = "safe_api_call",
  timeout = 30
) {
  # Build request if URL string provided
  if (is.character(req)) {
    req <- httr2::request(req) |>
      httr2::req_method("GET") |>
      httr2::req_timeout(timeout)
  } else if (!inherits(req, "httr2_request")) {
    stop("req must be an httr2 request or a URL string")
  }

  warnings <- character()
  perf <- withCallingHandlers(
    tryCatch(httr2::req_perform(req), error = function(e) e),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  # transport error
  if (inherits(perf, "error")) {
    tib <- cvd_error_tibble(
      context = context,
      error = paste0("Transport error: ", conditionMessage(perf)),
      status = NA_integer_,
      url = NA_character_,
      params = NA_character_,
      resp = NA_character_
    )
    return(list(success = FALSE, tibble = tib))
  }

  status <- httr2::resp_status(perf)
  url <- tryCatch(httr2::resp_url(perf), error = function(e) NA_character_)
  resp_text <- tryCatch(httr2::resp_body_string(perf), error = function(e) "")

  # non-success HTTP
  if (is.na(status) || !(status >= 200 && status < 300)) {
    tib <- cvd_error_tibble(
      context = context,
      error = paste0("HTTP ", status, ": ", substr(resp_text, 1, 400)),
      status = status,
      url = url,
      params = NA_character_,
      resp = substr(resp_text, 1, 400)
    )
    return(list(success = FALSE, tibble = tib))
  }

  # parse JSON
  parsed <- tryCatch(
    httr2::resp_body_json(perf, simplifyVector = TRUE),
    error = function(e) list(.raw = resp_text)
  )

  # apply processing if provided
  if (!is.null(process_fn)) {
    processed <- tryCatch(process_fn(parsed), error = function(e) e)
    if (inherits(processed, "error")) {
      tib <- cvd_error_tibble(
        context = context,
        error = paste0("Processing error: ", conditionMessage(processed)),
        status = status,
        url = url,
        params = NA_character_,
        resp = substr(resp_text, 1, 400)
      )
      return(list(success = FALSE, tibble = tib))
    }
    return(list(success = TRUE, result = processed))
  }

  list(success = TRUE, result = parsed)
}
