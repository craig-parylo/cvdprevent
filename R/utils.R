# General functions -----------------------------------------------------------

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
      "Requested {.val n} IDs exceeds available valid IDs ({.val {length(valid_ids)}})."
    )
  }

  # select randomly
  id <- sample(valid_ids, size = n)

  # return
  return(id)
}

# Lookup values ---------------------------------------------------------------

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
  # get a tibble containing valid time periods from the API (cached)
  # df_time_periods <- cvd_time_period_list()
  df_time_periods <- cvd_time_period_system_levels()

  # check the tibble contains a column called 'TimePeriodID'
  if (!"TimePeriodID" %in% names(df_time_periods)) {
    cli::cli_abort(
      "Column {.val TimePeriodID} not found in the time period data."
    )
  }

  # collect a distinct list of time period ids
  ids <- df_time_periods |>
    dplyr::pull(.data$TimePeriodID) |>
    unique() |>
    sort()

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
  v <- validate_input_id(
    id = time_period_id,
    param_name = "time_period_id",
    required = TRUE,
    valid_ids = m_get_valid_time_period_ids()
  )
  if (!identical(v, TRUE)) {
    return(v)
  }

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
    unique() |>
    sort()

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

#' Get one or more random system level IDs
#'
#' @description
#' Randomly selects `n` valid `system_level_id` values from the available list.
#'
#' @param n Integer. Number of IDs to return. Defaults to 1.
#'
#' @return A numeric vector of `n` randomly selected valid indicator tag IDs.
#' @noRd
get_random_valid_system_level_id_for_time_period_id <- function(
  n = 1,
  time_period_id
) {
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

# ## indicator_type_id ----
# #' Get valid indicator type IDs
# #'
# #' @description
# #' Retrieves a unique list of valid `indicator_type_id` values from the time period
# #' metadata. Results are memoised for performance.
# #'
# #' @return A numeric vector of unique `indicator_type_id` values.
# #' @noRd
# get_valid_indicator_type_ids <- function() {
#   # get a tibble containing valid time periods from the API (cached)
#   df_time_periods <- m_cvd_time_period_list()

#   # check the tibble contains a column called 'TimePeriodID'
#   if (!"IndicatorTypeID" %in% names(df_time_periods)) {
#     cli::cli_abort(
#       "Column {.val IndicatorTypeID} not found in the time period data."
#     )
#   }

#   # collect a distinct list of time period ids
#   ids <- df_time_periods |>
#     dplyr::pull(.data$IndicatorTypeID) |>
#     sort() |>
#     unique()

#   # checking the ids are numeric type
#   if (!is.numeric(ids)) {
#     cli::cli_warn(
#       "Returned Indicator Type IDs are not numeric. Coercing to numeric."
#     )
#     ids <- as.numeric(ids)
#   }

#   # return the result
#   return(ids)
# }

# #' Get one or more random indicator type IDs
# #'
# #' @description
# #' Randomly selects `n` valid `indicator_type_id` values from the available list.
# #'
# #' @param n Integer. Number of IDs to return. Defaults to 1.
# #'
# #' @return A numeric vector of `n` randomly selected valid indicator type IDs.
# #' @noRd
# get_random_valid_indicator_type_id <- function(n = 1) {
#   id <-
#     get_random_ids(
#       n = n,
#       valid_ids = m_get_valid_indicator_type_ids() # cached list of indicator type ids
#     )

#   # return
#   return(id)
# }

# ## indicator_tag_id ----
# #' Get valid indicator tag IDs
# #'
# #' @description
# #' Retrieves a unique list of valid `tag_id` values from the time period metadata.
# #' Results are memoised for performance.
# #'
# #' @return A numeric vector of unique `tag_id` values.
# #' @noRd
# get_valid_tag_ids <- function() {
#   # get a tibble containing valid time periods from the API
#   df_tags <- cvd_indicator_tags()

#   # check the tibble contains a column called 'IndicatorTagID'
#   if (!"IndicatorTagID" %in% names(df_tags)) {
#     cli::cli_abort(
#       "Column {.val IndicatorTagID} not found in the indicator tag data."
#     )
#   }

#   # collect a distinct list of time period ids
#   ids <- df_tags |>
#     dplyr::pull(.data$IndicatorTagID) |>
#     unique()

#   # checking the ids are numeric type
#   if (!is.numeric(ids)) {
#     cli::cli_warn(
#       "Returned Indicator Tag IDs are not numeric. Coercing to numeric."
#     )
#     ids <- as.numeric(ids)
#   }

#   # return the result
#   return(ids)
# }

# #' Get one or more random indicator tag IDs
# #'
# #' @description
# #' Randomly selects `n` valid `tag_id` values from the available list.
# #'
# #' @param n Integer. Number of IDs to return. Defaults to 1.
# #'
# #' @return A numeric vector of `n` randomly selected valid indicator tag IDs.
# #' @noRd
# get_random_valid_tag_id <- function(n = 1) {
#   id <-
#     get_random_ids(
#       n = n,
#       valid_ids = m_get_valid_tag_ids() # cached list of ids
#     )

#   # return
#   return(id)
# }

# ## area_ids ----

# #' Get valid area IDs for a given time period
# #'
# #' @description
# #' Retrieves a unique list of valid `area_id` values for a given value of `time_period_id`.
# #' Results are memoised for performance.
# #'
# #' @return A numeric vector of unique `area_id` values.
# #' @noRd
# get_valid_area_ids_for_time_period_id <- function(time_period_id) {
#   # validate input
#   validate_input_id(
#     id = time_period_id,
#     param_name = "time_period_id",
#     required = TRUE,
#     valid_ids = m_get_valid_time_period_ids()
#   )

#   # list the system levels for this time period id
#   valid_system_level_ids <-
#     m_get_valid_system_level_id_for_time_period_id(
#       time_period_id = time_period_id
#     )

#   # for each system level id, get a list of area ids and collate them
#   df_area_ids <-
#     purrr::map_dfr(
#       .x = valid_system_level_ids,
#       .f = \(.x) {
#         dat <-
#           m_cvd_area_list(
#             time_period_id = time_period_id,
#             system_level_id = .x
#           ) |>
#           dplyr::select(dplyr::any_of("AreaID"))
#       }
#     )

#   # check the tibble contains a column called 'AreaID'
#   if (!"AreaID" %in% names(df_area_ids)) {
#     cli::cli_abort("Column {.val AreaID} not found in the area data.")
#   }

#   # collect a distinct list of area ids
#   ids <-
#     df_area_ids |>
#     dplyr::pull(dplyr::any_of("AreaID")) |>
#     unique() |>
#     sort()

#   # checking the ids are numeric type
#   if (!is.numeric(ids)) {
#     cli::cli_warn("Returned Area IDs are not numeric. Coercing to numeric.")
#     ids <- as.numeric(ids)
#   }

#   return(ids)
# }

# #' Get one or more random area IDs
# #'
# #' @description
# #' Randomly selects `n` valid `area_id` values from the available list.
# #'
# #' @param n Integer. Number of IDs to return. Defaults to 1.
# #'
# #' @return A numeric vector of `n` randomly selected valid area IDs.
# #' @noRd
# get_random_valid_area_id_for_time_period_id <- function(n = 1, time_period_id) {
#   # validate input
#   validate_input_id(
#     id = time_period_id,
#     param_name = "time_period_id",
#     required = TRUE,
#     valid_ids = m_get_valid_time_period_ids()
#   )

#   id <-
#     get_random_ids(
#       n = n,
#       valid_ids = m_get_valid_area_ids_for_time_period_id(
#         time_period_id = time_period_id
#       ) # cached list of area ids
#     )

#   # return
#   return(id)
# }

# ## indicator_ids ----

# #' Get valid indicator IDs for a given time period
# #'
# #' @description
# #' Retrieves a unique list of valid `indicator_id` values for a given value of `time_period_id`.
# #' Results are memoised for performance.
# #'
# #' @return A numeric vector of unique `indicator_id` values.
# #' @noRd
# get_valid_indicator_ids_for_time_period_id <- function(time_period_id) {
#   # validate input
#   validate_input_id(
#     id = time_period_id,
#     param_name = "time_period_id",
#     required = TRUE,
#     valid_ids = m_get_valid_time_period_ids()
#   )

#   # list the system levels for this time period id
#   valid_system_level_ids <-
#     m_get_valid_system_level_id_for_time_period_id(
#       time_period_id = time_period_id
#     )

#   # for each system level id, get a list of indicator ids and collate them
#   df_indicator_ids <-
#     purrr::map_dfr(
#       .x = valid_system_level_ids,
#       .f = \(.x) {
#         dat <-
#           m_cvd_indicator_list(
#             time_period_id = time_period_id,
#             system_level_id = .x
#           ) |>
#           dplyr::select(dplyr::any_of("IndicatorID"))
#       }
#     )

#   # check the tibble contains a column called 'IndicatorID'
#   if (!"IndicatorID" %in% names(df_indicator_ids)) {
#     cli::cli_abort("Column {.val IndicatorID} not found in the indicator data.")
#   }

#   # collect a distinct list of indicator ids
#   ids <-
#     df_indicator_ids |>
#     dplyr::pull(dplyr::any_of("IndicatorID")) |>
#     unique() |>
#     sort()

#   # checking the ids are numeric type
#   if (!is.numeric(ids)) {
#     cli::cli_warn(
#       "Returned Indicator IDs are not numeric. Coercing to numeric."
#     )
#     ids <- as.numeric(ids)
#   }

#   return(ids)
# }

# #' Get one or more random indicator IDs for a given time period ID
# #'
# #' @description
# #' Randomly selects `n` valid `indicator_id` values from the available list for a
# #' given time period ID.
# #'
# #' @param n Integer. Number of IDs to return. Defaults to 1.
# #'
# #' @return A numeric vector of `n` randomly selected valid indicator IDs.
# #' @noRd
# get_random_valid_indicator_id_for_time_period_id <- function(
#   n = 1,
#   time_period_id
# ) {
#   # validate input
#   validate_input_id(
#     id = time_period_id,
#     param_name = "time_period_id",
#     required = TRUE,
#     valid_ids = m_get_valid_time_period_ids()
#   )

#   id <-
#     get_random_ids(
#       n = n,
#       valid_ids = m_get_valid_indicator_ids_for_time_period_id(
#         time_period_id = time_period_id
#       ) # cached list of indicator ids
#     )

#   # return
#   return(id)
# }

# #' Get valid indicator IDs for a given time period
# #'
# #' @description
# #' Retrieves a unique list of valid `indicator_id` values for a given combination of `time_period_id` and `area_id`.
# #' Results are memoised for performance.
# #'
# #' @return A numeric vector of unique `indicator_id` values.
# #' @noRd
# get_valid_indicator_ids_for_time_period_id_and_area_id <- function(
#   time_period_id,
#   area_id
# ) {
#   # validate input
#   validate_input_id(
#     id = time_period_id,
#     param_name = "time_period_id",
#     required = TRUE,
#     valid_ids = m_get_valid_time_period_ids()
#   )
#   validate_input_id(
#     id = area_id,
#     param_name = "area_id",
#     required = TRUE,
#     valid_ids = m_get_valid_area_ids_for_time_period_id(
#       time_period_id = time_period_id
#     )
#   )

#   # get a list of indicator data for this combination of time period and area
#   df_indicator_data <-
#     m_cvd_indicator(time_period_id = time_period_id, area_id = area_id)

#   # check the return includes indicators
#   if (!"indicators" %in% names(df_indicator_data)) {
#     cli::cli_abort("Tibble {.val indicators} not found in the indicator data.")
#   }

#   # get a distinct list of indicator ids
#   df_indicator_ids <-
#     df_indicator_data$indicators

#   # check the tibble contains a column called 'IndicatorID'
#   if (!"IndicatorID" %in% names(df_indicator_ids)) {
#     cli::cli_abort("Column {.val IndicatorID} not found in the indicator data.")
#   }

#   # collect a distinct list of indicator ids
#   ids <-
#     df_indicator_ids |>
#     dplyr::pull(dplyr::any_of("IndicatorID")) |>
#     unique() |>
#     sort()

#   # checking the ids are numeric type
#   if (!is.numeric(ids)) {
#     cli::cli_warn(
#       "Returned Indicator IDs are not numeric. Coercing to numeric."
#     )
#     ids <- as.numeric(ids)
#   }

#   return(ids)
# }

# #' Get one or more random indicator IDs for a given combination of time period ID and area ID
# #'
# #' @description
# #' Randomly selects `n` valid `indicator_id` values from the available list for a
# #' given combination of time period ID and area ID.
# #'
# #' @param n Integer. Number of IDs to return. Defaults to 1.
# #'
# #' @return A numeric vector of `n` randomly selected valid indicator IDs.
# #' @noRd
# get_random_valid_indicator_id_for_time_period_id_and_area_id <- function(
#   n = 1,
#   time_period_id,
#   area_id
# ) {
#   # validate input
#   validate_input_id(
#     id = time_period_id,
#     param_name = "time_period_id",
#     required = TRUE,
#     valid_ids = m_get_valid_time_period_ids()
#   )
#   validate_input_id(
#     id = area_id,
#     param_name = "area_id",
#     required = TRUE,
#     valid_ids = m_get_valid_area_ids_for_time_period_id(
#       time_period_id = time_period_id
#     )
#   )

#   id <-
#     get_random_ids(
#       n = n,
#       valid_ids = m_get_valid_indicator_ids_for_time_period_id_and_area_id(
#         time_period_id = time_period_id,
#         area_id = area_id
#       ) # cached list of indicator ids
#     )

#   # return
#   return(id)
# }

# ## metric_ids ----

# #' Get valid metric IDs for a given time period
# #'
# #' @description
# #' Retrieves a unique list of valid `metric_id` values for a given value of `time_period_id`.
# #' Results are memoised for performance.
# #'
# #' @return A numeric vector of unique `metric_id` values.
# #' @noRd
# get_valid_metric_ids_for_time_period_id <- function(time_period_id) {
#   # validate input
#   validate_input_id(
#     id = time_period_id,
#     param_name = "time_period_id",
#     required = TRUE,
#     valid_ids = m_get_valid_time_period_ids()
#   )

#   # list the system levels for this time period id
#   valid_system_level_ids <-
#     m_get_valid_system_level_id_for_time_period_id(
#       time_period_id = time_period_id
#     )

#   # for each system level id, get a list of metric ids and collate them
#   df_metric_ids <-
#     purrr::map_dfr(
#       .x = valid_system_level_ids,
#       .f = \(.x) {
#         dat <-
#           m_cvd_indicator_metric_list(
#             time_period_id = time_period_id,
#             system_level_id = .x
#           ) |>
#           dplyr::select(dplyr::any_of("MetricID"))
#       }
#     )

#   # check the tibble contains a column called 'MetricID'
#   if (!"MetricID" %in% names(df_metric_ids)) {
#     cli::cli_abort("Column {.val MetricID} not found in the metric data.")
#   }

#   # collect a distinct list of metric ids
#   ids <-
#     df_metric_ids |>
#     dplyr::pull(dplyr::any_of("MetricID")) |>
#     unique() |>
#     sort()

#   # checking the ids are numeric type
#   if (!is.numeric(ids)) {
#     cli::cli_warn(
#       "Returned Metric IDs are not numeric. Coercing to numeric."
#     )
#     ids <- as.numeric(ids)
#   }

#   return(ids)
# }

# #' Get one or more random metric IDs for a given time period ID
# #'
# #' @description
# #' Randomly selects `n` valid `metric_id` values from the available list for a given
# #' time period ID.
# #'
# #' @param n Integer. Number of IDs to return. Defaults to 1.
# #'
# #' @return A numeric vector of `n` randomly selected valid indicator IDs.
# #' @noRd
# get_random_valid_metric_id_for_time_period_id <- function(
#   n = 1,
#   time_period_id
# ) {
#   # validate input
#   validate_input_id(
#     id = time_period_id,
#     param_name = "time_period_id",
#     required = TRUE,
#     valid_ids = m_get_valid_time_period_ids()
#   )

#   id <-
#     get_random_ids(
#       n = n,
#       valid_ids = m_get_valid_metric_ids_for_time_period_id(
#         time_period_id = time_period_id
#       ) # cached list of metric ids
#     )

#   # return
#   return(id)
# }

# #' Get valid metric IDs for a combination of time period and area
# #'
# #' @description
# #' Retrieves a unique list of valid `metric_id` values for a given combination of `time_period_id` and `area_id`.
# #' Results are memoised for performance.
# #'
# #' @return A numeric vector of unique `metric_id` values.
# #' @noRd
# get_valid_metric_ids_for_time_period_id_and_area_id <- function(
#   time_period_id,
#   area_id
# ) {
#   # validate input
#   validate_input_id(
#     id = time_period_id,
#     param_name = "time_period_id",
#     required = TRUE,
#     valid_ids = m_get_valid_time_period_ids()
#   )
#   validate_input_id(
#     id = area_id,
#     param_name = "area_id",
#     required = TRUE,
#     valid_ids = m_get_valid_area_ids_for_time_period_id(
#       time_period_id = time_period_id
#     )
#   )

#   # get a list of metric data for this combination of time period and area
#   df_metric_data <-
#     m_cvd_indicator(time_period_id = time_period_id, area_id = area_id)

#   # check the return includes metrics
#   if (!"metric_categories" %in% names(df_metric_data)) {
#     cli::cli_abort(
#       "Tibble {.val metric_categories} not found in the metric data."
#     )
#   }

#   # get a distinct list of metric ids
#   df_metric_ids <-
#     df_metric_data$metric_categories

#   # check the tibble contains a column called 'MetricID'
#   if (!"MetricID" %in% names(df_metric_ids)) {
#     cli::cli_abort("Column {.val MetricID} not found in the metric data.")
#   }

#   # collect a distinct list of indicator ids
#   ids <-
#     df_metric_ids |>
#     dplyr::pull(dplyr::any_of("MetricID")) |>
#     unique() |>
#     sort()

#   # checking the ids are numeric type
#   if (!is.numeric(ids)) {
#     cli::cli_warn(
#       "Returned Metric IDs are not numeric. Coercing to numeric."
#     )
#     ids <- as.numeric(ids)
#   }

#   return(ids)
# }

# #' Get one or more random metric IDs for a given combination of time period ID and area ID
# #'
# #' @description
# #' Randomly selects `n` valid `metric_id` values from the available list for a given
# #' combination of time period ID and area ID.
# #'
# #' @param n Integer. Number of IDs to return. Defaults to 1.
# #'
# #' @return A numeric vector of `n` randomly selected valid metric IDs.
# #' @noRd
# get_random_valid_metric_id_for_time_period_id_and_area_id <- function(
#   n = 1,
#   time_period_id,
#   area_id
# ) {
#   # validate input
#   validate_input_id(
#     id = time_period_id,
#     param_name = "time_period_id",
#     required = TRUE,
#     valid_ids = m_get_valid_time_period_ids()
#   )
#   validate_input_id(
#     id = area_id,
#     param_name = "area_id",
#     required = TRUE,
#     valid_ids = m_get_valid_area_ids_for_time_period_id(
#       time_period_id = time_period_id
#     )
#   )

#   id <-
#     get_random_ids(
#       n = n,
#       valid_ids = m_get_valid_metric_ids_for_time_period_id_and_area_id(
#         time_period_id = time_period_id,
#         area_id = area_id
#       ) # cached list of metric ids
#     )

#   # return
#   return(id)
# }

# ## pathway_group_ids ----

# #' Get valid pathway group IDs
# #'
# #' @description
# #' Retrieves a unique list of valid `pathway_group_id` values.
# #' Results are memoised for performance.
# #'
# #' @return A numeric vector of unique `pathway_group_id` values.
# #' @noRd
# get_valid_pathway_group_ids <- function() {
#   # get the pathway groups
#   df_pathway_groups <- m_cvd_indicator_priority_groups()

#   # check the tibble contains pathway group id
#   if (!"PathwayGroupID" %in% names(df_pathway_groups)) {
#     cli::cli_abort(
#       "Column {.val PathwayGroupID} not found in the pathway group data."
#     )
#   }

#   # collect a distinct list of pathway group ids
#   ids <-
#     df_pathway_groups |>
#     dplyr::pull(dplyr::any_of("PathwayGroupID")) |>
#     unique() |>
#     na.omit() |>
#     sort()

#   # checking the ids are numeric type
#   if (!is.numeric(ids)) {
#     cli::cli_warn(
#       "Returned Pathway Group IDs are not numeric. Coercing to numeric."
#     )
#     ids <- as.numeric(ids)
#   }

#   return(ids)
# }

# #' Get one or more random pathway group IDs
# #'
# #' @description
# #' Randomly selects `n` valid `pathway_group_id` values from the available list.
# #'
# #' @param n Integer. Number of IDs to return. Defaults to 1.
# #'
# #' @return A numeric vector of `n` randomly selected valid pathway group IDs.
# #' @noRd
# get_random_valid_pathway_group_id <- function(n = 1) {
#   id <-
#     get_random_ids(
#       n = n,
#       valid_ids = m_get_valid_pathway_group_ids() # cached list of pathway ids
#     )

#   # return
#   return(id)
# }

# # validation ------------------------------------------------------------------

# #' Validate an 'id' input
# #'
# #' @description
# #' Validates the supplied 'id' parameter to ensure it meets expected criteria:
# #' presence (if required), type, length, and optionally, value domain.
# #'
# #' @details
# #' This validation function can be used to validate any type of 'id' value, such as:
# #' - `time_period_id`
# #' - `area_id`
# #'
# #' @param id Value provided by the user.
# #' @param param_name Character. Name of the parameter being validated (e.g. "time_period_id")
# #' @param required Logical. If `TRUE`, the parameter must be provided. Defaults to `TRUE`.
# #' @param valid_ids Optional numeric vector of acceptable values. If provided, `id` must match one of them.
# #'
# #' @return Invisibly returns `TRUE` if all checks pass. Otherwise, aborts with a descriptive error.
# #' @noRd
# validate_input_id <- function(
#   id = NA,
#   param_name = "id",
#   required = TRUE,
#   valid_ids = NULL
# ) {
#   # Check for missing or NULL when required
#   if (required && (missing(id) || is.null(id) || is.na(id))) {
#     cli::cli_abort("{.arg {param_name}} is required but was not provided.")
#   }

#   # Skip further checks if not required and NULL
#   if (!required && (missing(id) || is.null(id) || is.na(id))) {
#     return(invisible(TRUE))
#   }

#   # Length check: must be scalar
#   if (length(id) != 1) {
#     cli::cli_abort("{.arg {param_name}} must be a single value.")
#   }

#   # Type check: must be numeric and whole number
#   if (!is.numeric(id) || floor(id) != id) {
#     cli::cli_abort("{.arg {param_name}} must be a whole number.")
#   }

#   # Domain check: must be in valid_ids if provided
#   if (!is.null(valid_ids) && !(id %in% valid_ids)) {
#     cli::cli_abort("{.arg {param_name}} must be one of: {.val {valid_ids}}")
#   }

#   # return
#   invisible(TRUE)
# }

# #' Validate a string input
# #'
# #' @description
# #' Validates a string parameter to ensure it is present (if required), is a character scalar,
# #' and optionally matches a set of allowed values.
# #'
# #' @param value The input value to validate.
# #' @param param_name Character. Name of the parameter being validated (e.g., "area_name").
# #' @param required Logical. If `TRUE`, the parameter must be provided. Defaults to `TRUE`.
# #' @param valid_values Optional character vector of acceptable values.
# #'
# #' @return Invisibly returns `TRUE` if all checks pass. Otherwise, aborts with a descriptive error.
# #' @noRd
# validate_input_string <- function(
#   value,
#   param_name = "value",
#   required = TRUE,
#   valid_values = NULL
# ) {
#   # Check for missing or NULL when required
#   if (required && (missing(value) || is.null(value) || is.na(value))) {
#     cli::cli_abort("{.arg {param_name}} is required but was not provided.")
#   }

#   # Skip further checks if not required and NULL
#   if (!required && (missing(value) || is.null(value) || is.na(value))) {
#     return(invisible(TRUE))
#   }

#   # Type check: must be character
#   if (!is.character(value)) {
#     cli::cli_abort("{.arg {param_name}} must be a character string.")
#   }

#   # Length check: must be scalar
#   if (length(value) != 1) {
#     cli::cli_abort("{.arg {param_name}} must be a single string.")
#   }

#   # Domain check: must be in valid_values if provided
#   if (!is.null(valid_values) && !(value %in% valid_values)) {
#     cli::cli_abort("{.arg {param_name}} must be one of: {.val {valid_values}}")
#   }

#   # return
#   invisible(TRUE)
# }

# #' Validate a vector of numeric tag IDs
# #'
# #' @description
# #' Validates the `tag_id` parameter to ensure it is a numeric vector of whole numbers.
# #' Optionally checks for presence, length, and allowed values.
# #'
# #' @param tag_id The input vector to validate.
# #' @param required Logical. If `TRUE`, the parameter must be provided. Defaults to `TRUE`.
# #' @param valid_ids Optional numeric vector of acceptable values.
# #'
# #' @return Invisibly returns `TRUE` if all checks pass. Otherwise, aborts with a descriptive error.
# #' @noRd
# validate_input_id_vector <- function(
#   ids,
#   param_name = "ids",
#   required = TRUE,
#   valid_ids = NULL
# ) {
#   # Check for missing or NULL when required
#   if (required && (missing(ids) || is.null(ids) || all(is.na(ids)))) {
#     cli::cli_abort("{.arg param_name} is required but was not provided.")
#   }

#   # Skip further checks if not required and NULL
#   if (!required && (missing(ids) || is.null(ids) || all(is.na(ids)))) {
#     return(invisible(TRUE))
#   }

#   # Type check: must be numeric
#   if (!is.numeric(ids)) {
#     cli::cli_abort("{.arg param_name} must be a numeric vector.")
#   }

#   # Whole number check
#   if (any(floor(ids) != ids, na.rm = TRUE)) {
#     cli::cli_abort("{.arg param_name} must contain only whole numbers.")
#   }

#   # Domain check: must be in valid_ids if provided
#   if (!is.null(valid_ids) && any(!ids %in% valid_ids, na.rm = TRUE)) {
#     cli::cli_abort(
#       "{.arg param_name} contains invalid values. Must be one of: {.val {valid_ids}}"
#     )
#   }

#   # return
#   invisible(TRUE)
# }

# # API handling ----------------------------------------------------------------

# #' INTERNAL FUNCTION - Catch html 500 errors
# #'
# #' Outputs a console message and returns a Tibble containing the error message
# #'
# #' @return Tibble containing the error message
# #' @noRd
# internal_try_catch_html500 <- function(error, msg) {
#   cli::cli_alert_danger(msg)
#   return(dplyr::tibble(result = msg))
# }

# #' Safely perform an httr2 API request with error handling and retries
# #'
# #' @param req httr2 request object
# #' @param parse_fn Function to parse the response body
# #' @param context Character string describing the API context (for error messages)
# #' @param html500_msg Optional message for HTML 500 errors (e.g. invalid ID)
# #' @param timeout_sec Integer specifying the maximum number of seconds to wait for the response (default = 5)
# #' @param max_attempts Integer specifying the number of retry attempts (default = 3)
# #' @param delay_sec Integer base delay between retries (default = 1)
# #'
# #' @return A tibble or error-safe fallback
# #' @noRd
# safe_api_call <- function(
#   req,
#   parse_fn,
#   context = "API call",
#   html500_msg = NULL,
#   timeout_sec = 5,
#   max_attempts = 3,
#   delay_sec = 1,
#   cache = NULL,
#   cache_key = NULL # optional key for caching
# ) {
#   force(req)
#   force(parse_fn)
#   force(context)

#   # Core worker that actually performs the request
#   worker <- function(req) {
#     safe_retry <- function(expr) {
#       attempt <- 1
#       while (attempt <= max_attempts) {
#         result <- tryCatch(expr, error = function(e) e)
#         if (!inherits(result, "error")) {
#           return(result)
#         }
#         Sys.sleep(delay_sec * attempt)
#         attempt <- attempt + 1
#       }
#       structure(
#         list(message = "All retry attempts failed"),
#         class = "retry_error"
#       )
#     }

#     req <- req |> httr2::req_timeout(seconds = timeout_sec)
#     resp <- safe_retry(httr2::req_perform(req))

#     if (inherits(resp, "retry_error")) {
#       return(tibble::tibble(
#         context = context,
#         result = if (!is.null(html500_msg)) {
#           glue::glue("All retry attempts failed — likely cause: {html500_msg}")
#         } else {
#           "All retry attempts failed"
#         },
#         timestamp = Sys.time()
#       ))
#     }

#     status <- httr2::resp_status(resp)
#     if (status >= 400) {
#       return(tibble::tibble(
#         context = context,
#         result = paste("HTTP error", status, httr2::resp_status_desc(resp)),
#         timestamp = Sys.time()
#       ))
#     }

#     parsed <- tryCatch(
#       parse_fn(httr2::resp_body_string(resp)),
#       error = function(e) {
#         tibble::tibble(
#           context = context,
#           result = paste("Parse error:", e$message),
#           timestamp = Sys.time()
#         )
#       }
#     )

#     if (is.null(parsed) || length(parsed) == 0) {
#       return(tibble::tibble(
#         context = context,
#         result = "Returned no data",
#         timestamp = Sys.time()
#       ))
#     }

#     parsed
#   }

#   # If caching is enabled, wrap worker in memoise
#   if (!is.null(cache)) {
#     # If user provides a cache_key, memoise on that instead of req
#     if (!is.null(cache_key)) {
#       key_worker <- function(key) worker(req)
#       key_worker <- memoise::memoise(key_worker, cache = cache)
#       return(key_worker(cache_key))
#     } else {
#       worker <- memoise::memoise(worker, cache = cache)
#     }
#   }

#   worker(req)
# }

# # #' Safely perform an httr2 API request with error handling
# # #'
# # #' @param req httr2 request object
# # #' @param parse_fn Function to parse the response body
# # #' @param context Character string describing the API context (for error messages)
# # #' @param html500_msg Optional message for HTML 500 errors (e.g. invalid ID)
# # #' @param timeout_sec Integer specifying the maximum number of seconds to wait for the response (default = 4)
# # #'
# # #' @return A tibble or error-safe fallback
# # #' @noRd
# # safe_api_call <- function(
# #   req,
# #   parse_fn,
# #   context = "API call",
# #   html500_msg = NULL,
# #   timeout_sec = 5
# # ) {
# #   # wrap in tryCatch to gracefully handle errors
# #   tryCatch(
# #     {
# #       # perform the request
# #       resp <- req |>
# #         httr2::req_timeout(seconds = timeout_sec) |>
# #         httr2::req_perform()

# #       # cache the request
# #       req |>
# #         httr2::req_cache(
# #           path = tempfile(),
# #           max_age = 60 * 60 * 24 # 1 day in seconds
# #         )

# #       # check the status code returned from the API call
# #       # NB,
# #       # 1xx - informational
# #       # 2xx - success, esp 200 = OK
# #       # 3xx - redirection
# #       # 4xx - client errors, e.g. malformed request
# #       # 5xx - server errors
# #       status <- httr2::resp_status(resp)
# #       if (status != 200) {
# #         # display an alert to the user
# #         cli::cli_alert_danger(
# #           "{.fn cvdprevent::{context}} failed with status {status}"
# #         )
# #         # return the result as a tibble
# #         return(tibble::tibble(
# #           context = context,
# #           result = paste("Failed with status:", status),
# #           timestamp = Sys.time()
# #         ))
# #       }

# #       # handle JSON parsing errors
# #       parsed <- tryCatch(
# #         {
# #           parse_fn(httr2::resp_body_string(resp))
# #         },
# #         error = function(e) {
# #           cli::cli_alert_danger(
# #             "{.fn cvdprevent::{context}} failed to parse response {e$message}"
# #           )
# #           return(tibble::tibble(
# #             context = context,
# #             result = paste("Data parse error:", e$message),
# #             timestamp = Sys.time()
# #           ))
# #         }
# #       )

# #       # handle where nothing returned, despite no errors
# #       if (is.null(parsed) || length(parsed) == 0) {
# #         cli::cli_alert_danger(
# #           "{.fn cvdprevent::{context}} returned no data"
# #         )
# #         return(tibble::tibble(
# #           context = context,
# #           result = "Returned no data",
# #           timestamp = Sys.time()
# #         ))
# #       }

# #       # return the parsed data
# #       parsed
# #     },
# #     httr2_http_404 = function(e) {
# #       cli::cli_alert_danger(
# #         "{.fn cvdprevent::{context}} failed with HTTP 404 error {e$message}"
# #       )
# #       return(tibble::tibble(
# #         context = context,
# #         result = paste("HTTP 404 not found {e$message}"),
# #         timestamp = Sys.time()
# #       ))
# #     },
# #     httr2_error = function(e) {
# #       cli::cli_alert_danger(
# #         "{.fn cvdprevent::{context}} failed with HTTP error: likely caused by {html500_msg}"
# #       )
# #       return(tibble::tibble(
# #         context = context,
# #         result = glue::glue(
# #           "Unknown HTTP error likely caused by {html500_msg}"
# #         ),
# #         timestamp = Sys.time()
# #       ))
# #     }
# #     # # handle html 500
# #     # httr2_error = function(e) {
# #     #   # msg <- paste(context, dplyr::coalesce(html500_msg, e$Message))
# #     #   cli::cli_alert_danger(
# #     #     "{.fn cvdprevent::{context}} failed with HTTP 500 error: likely caused by {dplyr::coalesce({html500_msg}, e$Message)}"
# #     #   )
# #     #   return(tibble::tibble(
# #     #     context = context,
# #     #     result = paste("HTTP 500 error", html500_msg),
# #     #     timestamp = Sys.time()
# #     #   ))
# #     # }
# #     # error = function(e) {
# #     #   # compose an error message
# #     #   msg <- "{context} failed: {e$message}"
# #     #   if (
# #     #     !is.null(html500_msg) && e$message == "HTTP 500 Internal Server Error."
# #     #   ) {
# #     #     msg <- paste(msg, "The likely cause is", html500_msg)
# #     #   }
# #     #   # notify the user
# #     #   cli::cli_alert_danger(msg)
# #     #   return(tibble::tibble(
# #     #     context = context,
# #     #     result = e$message,
# #     #     timestamp = Sys.time()
# #     #   ))
# #     # }
# #   )
# # }

# #' Safely arrange a data frame by a column
# #'
# #' @description
# #' Arranges a data frame by a given column name or symbol, skipping the operation
# #' if the column does not exist.
# #'
# #' @param df A data frame or tibble to arrange.
# #' @param col A column name (unquoted or string).
# #'
# #' @return A tibble, arranged if the column exists; otherwise returned unchanged.
# #' @noRd
# safe_arrange <- function(df, col) {
#   col_sym <- rlang::ensym(col)
#   col_name <- rlang::as_name(col_sym)

#   if (col_name %in% names(df)) {
#     dplyr::arrange(df, !!col_sym)
#   } else {
#     cli::cli_alert_warning("Column '{col_name}' not found — skipping arrange.")
#     df
#   }
# }

# #' Safely test a memoised API function
# #'
# #' @param expr Expression to evaluate (e.g. force(memoised_fn))
# #' @param name Optional name for logging/debugging
# #'
# #' @return None; runs testthat expectations
# #' @noRd
# test_memoised_api <- function(expr, name = "API function") {
#   result <- tryCatch(force(expr), error = function(e) NULL)

#   if (is.null(result)) {
#     testthat::skip(glue::glue("{name} unavailable or failed"))
#   } else {
#     testthat::expect_s3_class(result, "tbl_df")
#     # testthat::expect_true(nrow(result) > 0 || grepl("API", result$result[1]))
#   }
# }

# # compile a tibble of the functions in the package
# get_pkg_functions <- function() {
#   df <- do.call(
#     rbind,
#     lapply(names(formals_list), function(nm) {
#       f <- formals_list[[nm]]
#       if (is.null(f)) {
#         return(NULL)
#       }
#       tibble::tibble(
#         fn = nm,
#         arg = names(f),
#         default = vapply(
#           f,
#           function(x) {
#             if (is.symbol(x)) NA_character_ else deparse(x)[1]
#           },
#           character(1)
#         )
#       )
#     })
#   )
# }

# audit_call <- function() {
#   log_dir <- tools::R_user_dir("cvdprevent", which = "cache")
#   browseURL(log_dir)
#   dir.create(log_dir, showWarnings = FALSE, recursive = TRUE)
#   logfile <- file.path(log_dir, "function_log.txt")

#   call_info <- match.call(sys.function(-1), sys.call(-1))
#   log_lines <- c(
#     paste0("Timestamp: ", Sys.time()),
#     paste0("Function called: ", as.character(call_info[[1]])),
#     "Arguments and values:"
#   )

#   for (i in 2:length(call_info)) {
#     arg_name <- names(call_info)[i]
#     arg_expr <- call_info[[i]]
#     arg_value <- eval(arg_expr, parent.frame(2))
#     log_lines <- c(
#       log_lines,
#       sprintf("  %s = %s", arg_name, toString(arg_value))
#     )
#   }

#   log_lines <- c(log_lines, "")
#   writeLines(
#     log_lines,
#     con = logfile,
#     sep = "\n",
#     useBytes = TRUE
#   )
# }
