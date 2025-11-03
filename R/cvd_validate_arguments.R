# validation ------------------------------------------------------------------

#' Validate an 'id' input
#'
#' @description
#' Validates the supplied 'id' parameter to ensure it meets expected criteria:
#' presence (if required), type, length, and optionally, value domain.
#'
#' @details
#' This validation function can be used to validate any type of 'id' value, such as:
#' - `time_period_id`
#' - `area_id`
#'
#' @param id Value provided by the user.
#' @param param_name Character. Name of the parameter being validated (e.g. "time_period_id")
#' @param required Logical. If `TRUE`, the parameter must be provided. Defaults to `TRUE`.
#' @param valid_ids Optional numeric vector of acceptable values. If provided, `id` must match one of them.
#'
#' @return Invisibly returns `TRUE` if all checks pass. Otherwise, aborts with a descriptive error.
#' @noRd
validate_input_id <- function(
  id = NA,
  param_name = "id",
  required = TRUE,
  valid_ids = NULL
) {
  # Check for missing or NULL when required
  if (required && (missing(id) || is.null(id) || is.na(id))) {
    cli::cli_abort("{.arg {param_name}} is required but was not provided.")
  }

  # Skip further checks if not required and NULL
  if (!required && (missing(id) || is.null(id) || is.na(id))) {
    return(invisible(TRUE))
  }

  # Length check: must be scalar
  if (length(id) != 1) {
    cli::cli_abort("{.arg {param_name}} must be a single value.")
  }

  # Type check: must be numeric and whole number
  if (!is.numeric(id) || floor(id) != id) {
    cli::cli_abort("{.arg {param_name}} must be a whole number.")
  }

  # Domain check: must be in valid_ids if provided
  if (!is.null(valid_ids) && !(id %in% valid_ids)) {
    cli::cli_abort("{.arg {param_name}} must be one of: {.val {valid_ids}}")
  }

  # return
  invisible(TRUE)
}

#' Validate a string input
#'
#' @description
#' Validates a string parameter to ensure it is present (if required), is a character scalar,
#' and optionally matches a set of allowed values.
#'
#' @param value The input value to validate.
#' @param param_name Character. Name of the parameter being validated (e.g., "area_name").
#' @param required Logical. If `TRUE`, the parameter must be provided. Defaults to `TRUE`.
#' @param valid_values Optional character vector of acceptable values.
#'
#' @return Invisibly returns `TRUE` if all checks pass. Otherwise, aborts with a descriptive error.
#' @noRd
validate_input_string <- function(
  value,
  param_name = "value",
  required = TRUE,
  valid_values = NULL
) {
  # Check for missing or NULL when required
  if (required && (missing(value) || is.null(value) || is.na(value))) {
    cli::cli_abort("{.arg {param_name}} is required but was not provided.")
  }

  # Skip further checks if not required and NULL
  if (!required && (missing(value) || is.null(value) || is.na(value))) {
    return(invisible(TRUE))
  }

  # Type check: must be character
  if (!is.character(value)) {
    cli::cli_abort("{.arg {param_name}} must be a character string.")
  }

  # Length check: must be scalar
  if (length(value) != 1) {
    cli::cli_abort("{.arg {param_name}} must be a single string.")
  }

  # Domain check: must be in valid_values if provided
  if (!is.null(valid_values) && !(value %in% valid_values)) {
    cli::cli_abort("{.arg {param_name}} must be one of: {.val {valid_values}}")
  }

  # return
  invisible(TRUE)
}

#' Validate a vector of numeric tag IDs
#'
#' @description
#' Validates the `tag_id` parameter to ensure it is a numeric vector of whole numbers.
#' Optionally checks for presence, length, and allowed values.
#'
#' @param tag_id The input vector to validate.
#' @param required Logical. If `TRUE`, the parameter must be provided. Defaults to `TRUE`.
#' @param valid_ids Optional numeric vector of acceptable values.
#'
#' @return Invisibly returns `TRUE` if all checks pass. Otherwise, aborts with a descriptive error.
#' @noRd
validate_input_id_vector <- function(
  ids,
  param_name = "ids",
  required = TRUE,
  valid_ids = NULL
) {
  # Check for missing or NULL when required
  if (required && (missing(ids) || is.null(ids) || all(is.na(ids)))) {
    cli::cli_abort("{.arg param_name} is required but was not provided.")
  }

  # Skip further checks if not required and NULL
  if (!required && (missing(ids) || is.null(ids) || all(is.na(ids)))) {
    return(invisible(TRUE))
  }

  # Type check: must be numeric
  if (!is.numeric(ids)) {
    cli::cli_abort("{.arg param_name} must be a numeric vector.")
  }

  # Whole number check
  if (any(floor(ids) != ids, na.rm = TRUE)) {
    cli::cli_abort("{.arg param_name} must contain only whole numbers.")
  }

  # Domain check: must be in valid_ids if provided
  if (!is.null(valid_ids) && any(!ids %in% valid_ids, na.rm = TRUE)) {
    cli::cli_abort(
      "{.arg param_name} contains invalid values. Must be one of: {.val {valid_ids}}"
    )
  }

  # return
  invisible(TRUE)
}
