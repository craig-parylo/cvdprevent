#' Build an error tibble for validators
#'
#' Internal helper that constructs a one-row error tibble. If a package-level
#' cvd_error_tibble() exists, it will be used; otherwise a minimal tibble is
#' created here.
#'
#' @param context character short label describing validation context
#' @param message character error message (plain text)
#' @param status integer HTTP status (optional)
#' @param url character request URL (optional)
#' @param params character parameters summary (optional)
#' @param resp character response snippet (optional)
#' @return one-row tibble with columns context, error, status, url, params, resp, timestamp
#' @noRd
ensure_error_tibble <- function(
  context,
  message,
  status = NA_integer_,
  url = NA_character_,
  params = NA_character_,
  resp = NA_character_
) {
  if (exists("cvd_error_tibble", mode = "function")) {
    return(cvd_error_tibble(
      context = context,
      error = message,
      status = status,
      url = url,
      params = params,
      resp = resp
    ))
  }
  tibble::tibble(
    context = as.character(context),
    error = as.character(message),
    status = as.integer(status),
    url = as.character(url),
    params = as.character(params),
    resp = as.character(resp),
    timestamp = Sys.time()
  )
}

#' Validate a single integer id
#'
#' Validate that a scalar id is present (if required), numeric and whole,
#' optionally within a supplied set of valid ids. On success returns TRUE.
#' On failure returns a one-row error tibble describing the problem.
#'
#' @param id scalar value to validate (numeric expected)
#' @param param_name character name shown in messages (default "id")
#' @param required logical whether the value must be provided (default TRUE)
#' @param valid_ids optional numeric vector of allowed ids
#' @param context character short label for the validation context (default "validate_input_id")
#' @param suppress_cli logical; when FALSE (default) a cli danger alert is emitted on failure
#' @return TRUE on success, or a one-row tibble describing the validation error on failure
#' @examples
#' validate_input_id(5, param_name = "time_period_id")
#' validate_input_id(NA, required = FALSE)
#' @noRd
validate_input_id <- function(
  id,
  param_name = "id",
  required = TRUE,
  valid_ids = NULL,
  context = "validate_input_id",
  suppress_cli = FALSE
) {
  show_msg_plain <- function(msg_text) {
    if (!isTRUE(suppress_cli)) {
      cli::cli_alert_danger(
        "{.strong Validation error}: {.field {param_name}} — {.emph {msg_text}}"
      )
    }
  }
  show_msg_vals <- function(vals_text) {
    if (!isTRUE(suppress_cli)) {
      cli::cli_alert_danger(
        "{.strong Validation error}: {.field {param_name}} — must be one of: {.val {vals_text}}"
      )
    }
  }

  # presence
  if (
    required && (missing(id) || is.null(id) || (length(id) == 1L && is.na(id)))
  ) {
    msg_text <- glue::glue("{param_name} is required but missing")
    tib <- ensure_error_tibble(context, as.character(msg_text))
    show_msg_plain(msg_text)
    return(tib)
  }
  if (
    !required && (missing(id) || is.null(id) || (length(id) == 1L && is.na(id)))
  ) {
    return(TRUE)
  }

  # scalar length
  if (length(id) != 1L) {
    msg_text <- glue::glue("{param_name} must be a single value")
    tib <- ensure_error_tibble(context, as.character(msg_text))
    show_msg_plain(msg_text)
    return(tib)
  }

  # type & whole-number check
  if (!is.numeric(id) || is.nan(id)) {
    msg_text <- glue::glue("{param_name} must be a whole number")
    tib <- ensure_error_tibble(context, as.character(msg_text))
    show_msg_plain(msg_text)
    return(tib)
  }
  if (is.na(id)) {
    return(TRUE)
  } # allowed only when not required (handled above)
  if (floor(id) != id) {
    msg_text <- glue::glue(
      "{param_name} must be a whole number (no fractional part)"
    )
    tib <- ensure_error_tibble(context, as.character(msg_text))
    show_msg_plain(msg_text)
    return(tib)
  }

  # domain check
  if (!is.null(valid_ids) && !(id %in% valid_ids)) {
    vals_text <- paste(valid_ids, collapse = ", ")
    msg_text <- glue::glue("{param_name} must be one of: {vals_text}")
    tib <- ensure_error_tibble(context, as.character(msg_text))
    show_msg_vals(vals_text)
    return(tib)
  }

  TRUE
}

#' Validate a single string
#'
#' Validate that a scalar string is present (if required), non-empty, and
#' optionally one of a set of allowed values. On success returns TRUE. On
#' failure returns a one-row error tibble describing the problem.
#'
#' @param value value to validate (character expected)
#' @param param_name character name shown in messages (default "value")
#' @param required logical whether the value must be provided (default TRUE)
#' @param valid_values optional character vector of allowed values
#' @param context character short label for the validation context (default "validate_input_string")
#' @param suppress_cli logical; when FALSE (default) a cli danger alert is emitted on failure
#' @return TRUE on success, or a one-row tibble describing the validation error on failure
#' @examples
#' validate_input_string("leicester", param_name = "area_name")
#' validate_input_string(NA, required = FALSE)
#' @noRd
validate_input_string <- function(
  value,
  param_name = "value",
  required = TRUE,
  valid_values = NULL,
  context = "validate_input_string",
  suppress_cli = FALSE
) {
  show_msg_plain <- function(msg_text) {
    if (!isTRUE(suppress_cli)) {
      cli::cli_alert_danger(
        "{.strong Validation error}: {.field {param_name}} — {.emph {msg_text}}"
      )
    }
  }
  show_msg_vals <- function(vals_text) {
    if (!isTRUE(suppress_cli)) {
      cli::cli_alert_danger(
        "{.strong Validation error}: {.field {param_name}} — must be one of: {.val {vals_text}}"
      )
    }
  }

  # presence
  if (
    required &&
      (missing(value) ||
        is.null(value) ||
        (length(value) == 1L && is.na(value)))
  ) {
    msg_text <- glue::glue("{param_name} is required but missing")
    tib <- ensure_error_tibble(context, as.character(msg_text))
    show_msg_plain(msg_text)
    return(tib)
  }
  if (
    !required &&
      (missing(value) ||
        is.null(value) ||
        (length(value) == 1L && is.na(value)))
  ) {
    return(TRUE)
  }

  # coerce factor
  if (is.factor(value)) {
    value <- as.character(value)
  }

  # type and scalar
  if (!is.character(value) || length(value) != 1L) {
    msg_text <- glue::glue("{param_name} must be a single character string")
    tib <- ensure_error_tibble(context, as.character(msg_text))
    show_msg_plain(msg_text)
    return(tib)
  }

  str <- trimws(value)
  if (nchar(str) == 0L) {
    msg_text <- glue::glue("{param_name} must not be empty")
    tib <- ensure_error_tibble(context, as.character(msg_text))
    show_msg_plain(msg_text)
    return(tib)
  }

  if (!is.null(valid_values) && !(str %in% valid_values)) {
    vals_text <- paste(valid_values, collapse = ", ")
    msg_text <- glue::glue("{param_name} must be one of: {vals_text}")
    tib <- ensure_error_tibble(context, as.character(msg_text))
    show_msg_vals(vals_text)
    return(tib)
  }

  TRUE
}

#' Validate a vector of integer ids
#'
#' Validate that a vector of ids is provided (if required), and that each
#' element is integer-like. Optionally checks membership against valid_ids.
#' On success returns TRUE. On failure returns a one-row error tibble.
#'
#' @param ids vector of ids to validate (numeric or character allowed)
#' @param param_name character name shown in messages (default "ids")
#' @param required logical whether the value must be provided (default TRUE)
#' @param valid_ids optional numeric vector of allowed ids
#' @param context character short label for the validation context (default "validate_input_id_vector")
#' @param suppress_cli logical; when FALSE (default) a cli danger alert is emitted on failure
#' @return TRUE on success, or a one-row tibble describing the validation error on failure
#' @examples
#' validate_input_id_vector(c(1,2,3), param_name = "tag_ids")
#' validate_input_id_vector(character(0), required = FALSE)
#' @noRd
validate_input_id_vector <- function(
  ids,
  param_name = "ids",
  required = TRUE,
  valid_ids = NULL,
  context = "validate_input_id_vector",
  suppress_cli = FALSE
) {
  show_msg_plain <- function(msg_text) {
    if (!isTRUE(suppress_cli)) {
      cli::cli_alert_danger(
        "{.strong Validation error}: {.field {param_name}} — {.emph {msg_text}}"
      )
    }
  }
  show_msg_vals <- function(vals_text, bad_idx = NULL) {
    if (!isTRUE(suppress_cli)) {
      if (is.null(bad_idx)) {
        cli::cli_alert_danger(
          "{.strong Validation error}: {.field {param_name}} — must be one of: {.val {vals_text}}"
        )
      } else {
        cli::cli_alert_danger(
          "{.strong Validation error}: {.field {param_name}} — contains invalid ids at positions: {bad_idx}. Allowed: {.val {vals_text}}"
        )
      }
    }
  }

  # presence
  if (
    required &&
      (missing(ids) || is.null(ids) || (length(ids) == 0L) || all(is.na(ids)))
  ) {
    msg_text <- glue::glue("{param_name} is required but missing or empty")
    tib <- ensure_error_tibble(context, as.character(msg_text))
    show_msg_plain(msg_text)
    return(tib)
  }
  if (
    !required &&
      (missing(ids) || is.null(ids) || (length(ids) == 0L) || all(is.na(ids)))
  ) {
    return(TRUE)
  }

  # allow factor -> character
  if (is.factor(ids)) {
    ids <- as.character(ids)
  }

  # allowed types
  if (!(is.numeric(ids) || is.character(ids))) {
    msg_text <- glue::glue(
      "{param_name} must be a numeric or character vector of ids"
    )
    tib <- ensure_error_tibble(context, as.character(msg_text))
    show_msg_plain(msg_text)
    return(tib)
  }

  # element-wise checks
  if (length(ids) == 0L) {
    return(TRUE)
  }
  for (i in seq_along(ids)) {
    xi <- ids[[i]]
    if (is.na(xi)) {
      next
    }
    if (is.character(xi)) {
      xi_trim <- trimws(xi)
      if (xi_trim == "") {
        msg_text <- glue::glue(
          "{param_name} contains an empty string at position {i}"
        )
        tib <- ensure_error_tibble(context, as.character(msg_text))
        show_msg_plain(msg_text)
        return(tib)
      }
      if (!grepl("^[-+]?[0-9]+$", xi_trim)) {
        msg_text <- glue::glue(
          "{param_name} contains non-integer-like value at position {i}"
        )
        tib <- ensure_error_tibble(context, as.character(msg_text))
        show_msg_plain(msg_text)
        return(tib)
      }
      next
    }
    if (is.numeric(xi)) {
      if (is.nan(xi)) {
        msg_text <- glue::glue("{param_name} contains NaN at position {i}")
        tib <- ensure_error_tibble(context, as.character(msg_text))
        show_msg_plain(msg_text)
        return(tib)
      }
      if (floor(xi) != xi) {
        msg_text <- glue::glue(
          "{param_name} contains fractional value at position {i}"
        )
        tib <- ensure_error_tibble(context, as.character(msg_text))
        show_msg_plain(msg_text)
        return(tib)
      }
      next
    }
    msg_text <- glue::glue(
      "{param_name} contains unsupported type at position {i}"
    )
    tib <- ensure_error_tibble(context, as.character(msg_text))
    show_msg_plain(msg_text)
    return(tib)
  }

  # domain check (coerce character to numeric where possible)
  if (!is.null(valid_ids)) {
    numeric_ids <- suppressWarnings(as.numeric(ids))
    bad_idx <- which(!is.na(numeric_ids) & !(numeric_ids %in% valid_ids))
    if (length(bad_idx) > 0) {
      vals_text <- paste(valid_ids, collapse = ", ")
      msg_text <- glue::glue(
        "{param_name} contains invalid ids at positions: {paste(bad_idx, collapse = \", \")}. Allowed: {vals_text}"
      )
      tib <- ensure_error_tibble(context, as.character(msg_text))
      show_msg_vals(vals_text, bad_idx = paste(bad_idx, collapse = ", "))
      return(tib)
    }
  }

  TRUE
}
