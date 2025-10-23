# cache -----------------------------------------------------------------------

#' Create a persistent cache for memoisation
#'
#' @description
#' Initialises a disk-based cache for storing memoised data across R sessions.
#' The cache is stored in a user-specific directory and configured to expire items after one week.
#'
#' @returns A `cachem::cache_disk` object
#' @noRd
set_cache <- function() {
  cache <-
    cachem::cache_disk(
      rappdirs::user_cache_dir("R_cvdprevent"),
      max_age = 60 * 60 * 24 * 7 # 1 week in seconds
    )

  return(cache)
}

delayedAssign("m_cache", set_cache())

## memoised functions ---------------------------------------------------------
# set as delayed assign to avoid issues on load
delayedAssign(
  "m_get_valid_time_period_ids",
  memoise::memoise(
    get_valid_time_period_ids,
    cache = m_cache
  )
)

delayedAssign(
  "m_get_valid_tag_ids",
  memoise::memoise(
    get_valid_tag_ids,
    cache = m_cache
  )
)

delayedAssign(
  "m_get_valid_system_level_id_for_time_period_id",
  memoise::memoise(
    get_valid_system_level_id_for_time_period_id,
    cache = m_cache
  )
)

#' Clear memoised caches for CVD input validation functions
#'
#' @description
#' Clears the memoised caches used by input validation functions in the `cvdprevent` package,
#' such as `m_get_valid_time_period_ids()` and `m_get_valid_tag_ids()`.
#'
#' @details
#' The `cvdprevent` package validates user inputs like `time_period_id` and `tag_id` by
#' retrieving valid values from an external API. To improve responsiveness and reduce
#' unnecessary API calls, these values are cached using memoisation and remain valid for up to 7 days.
#'
#' This function clears any existing caches, forcing the validation functions to re-fetch
#' fresh data from the API. This is especially useful when new audit data has been published
#' since the last cache was created, and stale values are causing validation errors (e.g.,
#' an apparently invalid `time_period_id` that is actually valid in the latest dataset).
#'
#' @return Invisibly returns `TRUE` after clearing all relevant caches.
#'
#' @examples
#' # clear all caches used to validate user inputs
#' \donttest{cvd_clear_cache()}
#'
#' @export
cvd_clear_cache <- function() {
  # List of memoised functions to forget
  memoised_fns <- list(
    m_get_valid_time_period_ids,
    m_get_valid_tag_ids
  )

  # Validate all are memoised functions
  purrr::walk(memoised_fns, function(fn) {
    if (!inherits(fn, "memoised_function")) {
      cli::cli_warn(
        "One or more functions in {.arg memoised_fns} are not memoised."
      )
    }
  })

  # Clear their caches
  purrr::walk(memoised_fns, memoise::forget)

  # return
  invisible(TRUE)
}

#' Get one or more random ID values
#'
#' @description
#' Randomly selects one or more valid ID values from a supplied vector.
#'
#' @param n Integer. Number of IDs to return. Must be a single positive whole number. Defaults to 1.
#' @param valid_ids A numeric vector of valid IDs to return from. Typically returned by a memoised lookup function.
#'
#' @returns A numeric vector of `n` randomly selected valid IDs
#' @noRd
get_random_ids <- function(
  n = 1,
  valid_ids
) {
  # validate `n`
  if (!is.numeric(n) || length(n) != 1 || n < 1 || floor(n) != n) {
    cli::cli_abort("{.arg n} must be a single positive whole number.")
  }

  # validate `valid_ids`
  if (missing(valid_ids) || !is.numeric(valid_ids) || length(valid_ids) == 0) {
    cli::cli_abort("{.arg valid_ids} must be a non-empty numeric vector.")
  }

  # check sample size
  if (n > length(valid_ids)) {
    cli::cli_abort(
      "Requested {.val n} IDs exceeds available valid IDs ({.val {length(ids)}})."
    )
  }

  # select randomly
  id <- sample(valid_ids, size = n)

  # return
  return(id)
}

## time_period_id ------
#' Get valid time period IDs
#'
#' @description
#' Retrieves a unique list of valid `time_period_id` values from the time period metadata.
#' Results are memoised for performance.
#'
#' @return A numeric vector of unique `time_period_id` values.
#' @noRd
get_valid_time_period_ids <- function() {
  # get a tibble containing valid time periods from the API
  df_time_periods <- cvd_time_period_list()

  # check the tibble contains a column called 'TimePeriodID'
  if (!"TimePeriodID" %in% names(df_time_periods)) {
    cli::cli_abort(
      "Column {.val TimePeriodID} not found in the time period data."
    )
  }

  # collect a distinct list of time period ids
  ids <- df_time_periods |>
    dplyr::pull(.data$TimePeriodID) |>
    unique()

  # checking the ids are numeric type
  if (!is.numeric(ids)) {
    cli::cli_warn(
      "Returned Time Period IDs are not numeric. Coercing to numeric."
    )
    ids <- as.numeric(ids)
  }

  # return the result
  return(ids)
}

# #' Memoised version of `get_valid_time_period_ids()`
# #' @noRd
# m_get_valid_time_period_ids <- memoise::memoise(
#   get_valid_time_period_ids,
#   cache = m_cache
# )

#' Get one or more random valid time period IDs
#'
#' @description
#' Randomly selects `n` valid `time_period_id` values from the available list.
#'
#' @param n Integer. Number of IDs to return. Defaults to 1.
#'
#' @return A numeric vector of `n` randomly selected valid time period IDs.
#' @noRd
get_random_valid_time_period_id <- function(n = 1) {
  id <-
    get_random_ids(
      n = n,
      valid_ids = m_get_valid_time_period_ids() # cached list of time period ids
    )

  # return
  return(id)
}

## indicator_tag_id ----
#' Get valid indicator tag IDs
#'
#' @description
#' Retrieves a unique list of valid `tag_id` values from the time period metadata.
#' Results are memoised for performance.
#'
#' @return A numeric vector of unique `tag_id` values.
#' @noRd
get_valid_tag_ids <- function() {
  # get a tibble containing valid time periods from the API
  df_tags <- cvd_indicator_tags()

  # check the tibble contains a column called 'IndicatorTagID'
  if (!"IndicatorTagID" %in% names(df_tags)) {
    cli::cli_abort(
      "Column {.val IndicatorTagID} not found in the indicator tag data."
    )
  }

  # collect a distinct list of time period ids
  ids <- df_tags |>
    dplyr::pull(.data$IndicatorTagID) |>
    unique()

  # checking the ids are numeric type
  if (!is.numeric(ids)) {
    cli::cli_warn(
      "Returned Indicator Tag IDs are not numeric. Coercing to numeric."
    )
    ids <- as.numeric(ids)
  }

  # return the result
  return(ids)
}

# #' Memoised version of `get_valid_tag_ids()`
# #' @noRd
# m_get_valid_tag_ids <- memoise::memoise(
#   get_valid_tag_ids,
#   cache = m_cache
# )

#' Get one or more random indicator tag IDs
#'
#' @description
#' Randomly selects `n` valid `tag_id` values from the available list.
#'
#' @param n Integer. Number of IDs to return. Defaults to 1.
#'
#' @return A numeric vector of `n` randomly selected valid indicator tag IDs.
#' @noRd
get_random_valid_tag_id <- function(n = 1) {
  id <-
    get_random_ids(
      n = n,
      valid_ids = m_get_valid_tag_ids() # cached list of ids
    )

  # return
  return(id)
}

## system_level_id -----

#' Get valid system level IDs for a given time period
#'
#' @description
#' Retrieves a unique list of valid `system_level_id` values for a given value of `time_period_id`.
#' Results are memoised for performance.
#'
#' @return A numeric vector of unique `system_level_id` values.
#' @noRd
get_valid_system_level_id_for_time_period_id <- function(time_period_id) {
  # validate input
  validate_input_id(
    id = time_period_id,
    param_name = "time_period_id",
    required = TRUE,
    valid_ids = m_get_valid_time_period_ids()
  )

  # get a tibble containing valid system levels from the API
  df_system_levels <- cvd_area_system_level(time_period_id = time_period_id)

  # check the tibble contains a column called 'SystemLevelID'
  if (!"SystemLevelID" %in% names(df_system_levels)) {
    cli::cli_abort(
      "Column {.val SystemLevelID} not found in the system level data."
    )
  }

  # collect a distinct list of system level ids
  ids <- df_system_levels |>
    dplyr::pull(.data$SystemLevelID) |>
    unique()

  # checking the ids are numeric type
  if (!is.numeric(ids)) {
    cli::cli_warn(
      "Returned System Level IDs are not numeric. Coercing to numeric."
    )
    ids <- as.numeric(ids)
  }

  # return the result
  return(ids)
}

# #' Memoised version of `get_valid_system_level_id_for_time_period_id()`
# #' @noRd
# m_get_valid_system_level_id_for_time_period_id <- memoise::memoise(
#   get_valid_system_level_id_for_time_period_id,
#   cache = m_cache
# )

#' Get one or more random indicator tag IDs
#'
#' @description
#' Randomly selects `n` valid `tag_id` values from the available list.
#'
#' @param n Integer. Number of IDs to return. Defaults to 1.
#'
#' @return A numeric vector of `n` randomly selected valid indicator tag IDs.
#' @noRd
get_random_system_level_for_time_period_id <- function(n = 1, time_period_id) {
  # validate input
  validate_input_id(
    id = time_period_id,
    param_name = "time_period_id",
    required = TRUE,
    valid_ids = m_get_valid_time_period_ids()
  )

  id <-
    get_random_ids(
      n = n,
      valid_ids = m_get_valid_system_level_id_for_time_period_id(
        time_period_id = time_period_id
      ) # cached list of ids
    )

  # return
  return(id)
}


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

# API handling ----------------------------------------------------------------
#' INTERNAL FUNCTION - Catch html 500 errors
#'
#' Outputs a console message and returns a Tibble containing the error message
#'
#' @return Tibble containing the error message
#' @noRd
internal_try_catch_html500 <- function(error, msg) {
  cli::cli_alert_danger(msg)
  return(dplyr::tibble(result = msg))
}
