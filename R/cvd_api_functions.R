# setup ------------------------------------------------------------------------
url_base <- 'https://api.cvdprevent.nhs.uk'

globalVariables(
  c(
    'AreaData',
    'Areas',
    'Categories',
    'Categories_MetricID',
    'Categories_TimeSeries',
    'CategoryData',
    'Children',
    'Children_Children',
    'Children_Children_Children',
    'ComparisonData',
    'Data',
    'IndicatorID',
    'Indicators',
    'InequalityMarkers',
    'MetaData',
    'MetricList',
    'SubSystems',
    'SystemLevels',
    'TimeSeriesData',
    'IndicatorTypeID',
    'IndicatorTypeName',
    'TimePeriodID',
    'SystemLevelID',
    'IndicatorTagID',
    'MetricID',
    'setNames',
    ".data"
  )
)

## time period -----------------------------------------------------------------

#' List indicator types
#'
#' Returns IDs and descriptions for indicator types.
#' This is a helper function for the `cvd_time_period_list()` which permits the
#' optional parameter of `indicator_type_id`.
#'
#' @return Tibble of indicator types
#' @export
#' @seealso [cvd_time_period_list()]
#'
#' @examples
#' # NB, the following example is not tested because it takes longer than
#' # expected to return the results
#'
#' # List available indicator types
#' \donttest{cvd_indicator_types()}
cvd_indicator_types <- function() {
  # get the data from the function - no parameters to return all types
  data <- cvd_time_period_list() |>
    dplyr::select(IndicatorTypeID, IndicatorTypeName) |>
    dplyr::distinct()
}

#' List available time periods for CVD indicators
#'
#' @description
#' Retrieves all available reporting periods from the CVDPREVENT API. Optionally, you can filter periods by a specific indicator type (e.g., standard, outcome) using the `indicator_type_id` parameter.
#'
#' @section API Documentation:
#' See the [CVDPREVENT API documentation: Time period](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2FtimePeriod) for endpoint details.
#'
#' @param indicator_type_id Optional integer. If provided, restricts the returned time periods to those containing data of the given indicator type.
#'
#' @return A tibble with details of available time periods, including fields such as `TimePeriodID`, `TimePeriodName`, and associated indicator type information.
#' If no data is found, returns a tibble describing the error.
#'
#' @details
#' This function is often used to determine valid values for time period parameters in other API queries. It is a building block for most higher-level data retrieval functions in this package.
#'
#' @seealso [cvd_indicator_types()], [cvd_time_period_system_levels()]
#'
#' @examples
#' # NB, the following examples are not tested because they take longer than
#' # expected to return the results
#'
#' # List all available time periods
#' \donttest{cvd_time_period_list()}
#'
#' # List time periods with data for a specific indicator type (e.g., Standard)
#' \donttest{cvd_time_period_list(indicator_type_id = 1)}
#'
#' @export
cvd_time_period_list <- function(indicator_type_id) {
  # validate input
  validate_input_id(
    id = indicator_type_id,
    param_name = "indicator_type_id",
    required = FALSE
  )

  # compose the request
  req <-
    httr2::request(url_base) |>
    httr2::req_url_path_append('timePeriod')

  if (!missing(indicator_type_id)) {
    req <-
      req |>
      httr2::req_url_query(
        `indicatorTypeID` = indicator_type_id
      )
  }

  # perform the request
  resp <- req |>
    httr2::req_perform() |>
    httr2::resp_body_string()

  # wrangle for output
  data <- jsonlite::fromJSON(resp, flatten = T)$timePeriodList

  if (length(data) == 0) {
    cli::cli_alert_danger('No time periods returned')
    return(dplyr::tibble(result = 'No time periods returned'))
  } else {
    data <- data |>
      purrr::compact() |>
      dplyr::as_tibble() |>
      dplyr::arrange(TimePeriodID)
  }
}

#' List available time periods and associated system levels
#'
#' @description
#' Retrieves all available reporting periods from the CVDPREVENT API, along with the NHS system levels included in each time period.
#'
#' This function is useful to determine which system levels (e.g., national, region, ICB, PCN, practice) have data available for each reporting period.
#'
#' @section API Documentation:
#' See the [CVDPREVENT API documentation: Time period system levels](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#*Proposed*-%2FtimePeriod%2FsystemLevels) for technical details.
#'
#' @return
#' A tibble containing time periods and the corresponding system levels available for each, with columns including (but not limited to) `TimePeriodID`, `TimePeriodName`, `SystemLevelID` and `SystemLevelName`.
#'
#' @details
#' This function is helpful for understanding the data structure of each reporting period, especially if you need to filter or subset data by system level and time period in downstream API calls.
#'
#' @seealso [cvd_time_period_list()], [cvd_area_system_level_time_periods()]
#'
#' @examples
#' # Retrieve all time periods and associated system levels
#' periods_levels <- cvd_time_period_system_levels()
#'
#' # Show available system levels for the latest time period
#' periods_levels |>
#'   plyr::slice_max(order_by = TimePeriodID) |>
#'   dplyr::select(TimePeriodID, TimePeriodName, SystemLevelID, SystemLevelName)
#'
#' @export
cvd_time_period_system_levels <- function() {
  # compose the request
  req <-
    httr2::request(url_base) |>
    httr2::req_url_path_append('timePeriod/systemLevels')

  # perform the request
  resp <- req |>
    httr2::req_perform() |>
    httr2::resp_body_string()

  # wrangle for output
  data <- jsonlite::fromJSON(resp, flatten = T)[[2]] |>
    purrr::compact() |>
    dplyr::as_tibble() |>
    dplyr::select(
      -dplyr::any_of(c('NotificationCount', 'HighestPriorityNotificationType'))
    ) |> # prevents conflicts with column of same name in nested data
    dplyr::relocate(
      dplyr::any_of(c('SystemLevels')),
      .after = dplyr::last_col()
    ) |>
    tidyr::unnest(cols = dplyr::any_of(c('SystemLevels'))) |>
    dplyr::arrange(TimePeriodID, SystemLevelID)
}

## area ------------------------------------------------------------------------

#' List system levels available for a specific time period
#'
#' @description
#' Retrieves all available NHS system levels (e.g., National, Region, ICB, PCN, Practice) for a specified reporting time period from the CVDPREVENT API.
#'
#' This function helps users determine which system levels are available for data extraction in a given reporting period.
#'
#' @section API Documentation:
#' See the [CVDPREVENT API documentation: System levels per time period](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Farea%2FsystemLevel)
#'
#' @param time_period_id Integer (required). The ID of the reporting time period for which system levels should be returned. Use [cvd_time_period_list()] to find valid IDs.
#'
#' @return A tibble containing system level details for the specified time period, with columns such as `SystemLevelID` and `SystemLevelName`.
#' If there are no results, returns a tibble with a single column `result` describing the error.
#'
#' @details
#' This function is useful in workflows where you need to filter or subset NHS areas or data by both time period and system level. It is often used as a precursor to more detailed area or indicator queries.
#'
#' @seealso [cvd_area_details()], [cvd_area_unassigned()], [cvd_area_search()], [cvd_area_nested_subsystems()], [cvd_area_flat_subsystems()], [cvd_time_period_system_levels()]
#'
#' @examples
#' # List all system levels available for time period 4 (activity to March 2022)
#' cvd_area_system_level(time_period_id = 4) |>
#'   dplyr::select(SystemLevelID, SystemLevelName)
#'
#' # Find valid time period IDs, then get system levels for the latest one
#' latest_period <-
#'   cvd_time_period_list() |>
#'   dplyr::pull(TimePeriodID) |>
#'   max()
#'
#' cvd_area_system_level(time_period_id = latest_period) |>
#'   dplyr::select(SystemLevelID, SystemLevelName)
#'
#' @export
cvd_area_system_level <- function(time_period_id) {
  # validate input
  validate_input_id(
    id = time_period_id,
    param_name = "time_period_id",
    required = TRUE,
    valid_ids = m_get_valid_time_period_ids()
  )

  # compose the request
  req <-
    httr2::request(url_base) |>
    httr2::req_url_path_append('area/systemLevel') |>
    httr2::req_url_query(
      `timePeriodID` = time_period_id
    )

  # perform the request
  resp <- req |>
    httr2::req_perform() |>
    httr2::resp_body_string()

  # wrangle for output
  data <- jsonlite::fromJSON(resp, flatten = T)[[2]] |>
    purrr::compact() |>
    dplyr::as_tibble()
}

#' List all system levels and their available time periods
#'
#' @description
#' Retrieves all available NHS system levels from the CVDPREVENT API, along with the reporting periods (time periods) in which each system level has data available.
#'
#' This function is the inverse of [cvd_time_period_system_levels()], allowing you to see, for each system level (e.g., National, Region, ICB, PCN, Practice), the set of time periods for which data exists.
#'
#' @section API Documentation:
#' [CVDPREVENT API documentation: All system levels and time periods](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#*Proposed*%2Farea%2FsystemLevel%2FtimePeriods) for details.
#'
#' @return A tibble with one row per system level and time period, including columns such as `SystemLevelID`, `SystemLevelName`, `TimePeriodID` and `TimePeriodName`.
#'
#' @details
#' Use this function to determine which reporting periods are available for each NHS system level. This is useful for dynamically generating data selections or validating user input in dashboards or scripts.
#'
#' @seealso [cvd_time_period_system_levels()], [cvd_area_details()], [cvd_area_unassigned()], [cvd_area_search()], [cvd_area_nested_subsystems()], [cvd_area_flat_subsystems()]
#'
#' @examples
#' # List the latest four reporting periods at GP practice level
#' cvd_area_system_level_time_periods() |>
#'   dplyr::filter(SystemLevelName == "Practice") |>
#'   dplyr::slice_max(order_by = TimePeriodID, n = 4) |>
#'   dplyr::select(SystemLevelName, TimePeriodID, TimePeriodName)
#'
#' # Explore all system levels and their available time periods
#' cvd_area_system_level_time_periods()
#'
#' @export
cvd_area_system_level_time_periods <- function() {
  # compose the request
  req <-
    httr2::request(url_base) |>
    httr2::req_url_path_append('area/systemLevel/timePeriods')

  # perform the request
  resp <- req |>
    httr2::req_perform() |>
    httr2::resp_body_string()

  # wrangle for output
  data <- jsonlite::fromJSON(resp, flatten = T)[[2]] |>
    purrr::compact() |>
    dplyr::as_tibble() |>
    tidyr::unnest(cols = dplyr::any_of(c('TimePeriods')))
}


#' List NHS areas for a given time period and parent or system level
#'
#' @description
#' Retrieves all NHS geographical areas (e.g., England, Region, ICB, PCN, Practice) that have data avialable for a specified reporting period and either a pareent area or system level. Only areas with data for the chosen time period are returned.
#'
#' You must specify at least one of `parent_area_id` or `system_level_id`. If both are provided, `parent_area_id` takes precedence and `system_level_id` is ignored.
#' - If `parent_area_id` is specified, returns all child areas of the specified parent area.
#' - If `system_level_id` is specified, returns all areas within that system level.
#'
#' @section API Documentation:
#' See the [CVDPREVENT API documentation: Area lists](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Farea)
#'
#' @param time_period_id Integer (required). The reporting period ID for which to return areas. Use [cvd_time_period_list()] to find valid IDs.
#' @param parent_area_id Integer (optional). The AreaID for which all children will be returned. If provided, this takes precedence over `system_level_id`.
#' @param system_level_id Integer (optional). The system level ID for which to return all areas (e.g., Practice, PCN, ICB). Ignored if `parent_area_id` is specified. Use [cvd_area_system_level()] to find valid IDs for a given time period.
#'
#' @return
#' A tibble containing area details for the specified criteria. Columns typically include `SystemLevelName`, `AreaID`, `AreaCode`, `AreaName`, and (if available) parent area details.
#' If no areas are found, returns a tibble describing the error.
#'
#' @details
#' - At least one of `parent_area_id` or `system_level_id` must be supplied, otherwise an error is thrown.
#' - This function is commonly used to list all practices within a given PCN, all PCNs within an ICB, or all areas at a specific system level for a chosen time period.
#'
#' @seealso
#' [cvd_area_details()], [cvd_area_unassigned()], [cvd_area_search()], [cvd_area_nested_subsystems()], [cvd_area_flat_subsystems()]
#'
#' @examples
#' # List four PCNs (system level 4) with data available at time period 17
#' cvd_area_list(time_period_id = 17, system_level_id = 4) |>
#'   dplyr::select(SystemLevelName, AreaID, AreaCode, AreaName) |>
#'   dplyr::slice_head(n = 4)
#'
#' # List all child areas of parent area 8037 (e.g., an ICB) in time period 17
#' cvd_area_list(time_period_id = 17, parent_area_id = 8037)
#'
#' # Attempting to call without either optional argument will result in an error
#' # cvd_area_list(time_period_id = 17)
#'
#' @export
cvd_area_list <- function(time_period_id, parent_area_id, system_level_id) {
  # validate input
  validate_input_id(
    id = time_period_id,
    param_name = "time_period_id",
    required = TRUE,
    valid_ids = m_get_valid_time_period_ids()
  )
  validate_input_id(
    id = parent_area_id,
    param_name = "parent_area_id",
    required = FALSE
  )
  validate_input_id(
    id = system_level_id,
    param_name = "system_level_id",
    required = FALSE,
    valid_ids = m_get_valid_system_level_id_for_time_period_id(
      time_period_id = time_period_id
    )
  )

  # compose the request
  req <-
    httr2::request(url_base) |>
    httr2::req_url_path_append('area')

  if (base::missing(parent_area_id) && base::missing(system_level_id)) {
    # both optional arguments are missing - but we need at least one
    cli::cli_abort(
      "At least one of {.arg parent_area_id} or {.arg system_level_id} must be provided."
    )
  } else if (base::missing(parent_area_id)) {
    # system level id provided
    req <- req |>
      httr2::req_url_query(
        `timePeriodID` = time_period_id,
        `systemLevelID` = system_level_id
      )
  } else if (base::missing(system_level_id)) {
    # parent area id provided
    req <- req |>
      httr2::req_url_query(
        `timePeriodID` = time_period_id,
        `parentAreaID` = parent_area_id
      )
  } else {
    cli::cli_alert(
      text = 'Missing arguments'
    )
  }

  # perform the request
  resp <- req |>
    httr2::req_perform() |>
    httr2::resp_body_string()

  # wrangle for output
  data <- jsonlite::fromJSON(resp, flatten = T)$areaList
  if (length(data) == 0) {
    cli::cli_alert_danger('No areas returned')
    return(dplyr::tibble(result = 'No areas returned'))
  } else {
    data <- data |>
      purrr::compact() |>
      dplyr::as_tibble() |>
      dplyr::relocate(
        dplyr::any_of(c('Parents')),
        .after = dplyr::last_col()
      ) |>
      tidyr::unnest(col = dplyr::any_of(c('Parents')))
  }
}

#' Retrieve details for a specific NHS area and time period
#'
#' @description
#' Returns detailed information about a single NHS area for a given reporting period, including its own details, as well as any parent and child areas. This allows you to understand the heirarchical context (e.g., parent ICB, child PCNs or Practices) for the specified area.
#'
#' @section API Documentation:
#' See the [CVDPREVENT API documentation: Area details](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Farea%2F%3Carea_id%3E%2Fdetails)
#'
#' @param time_period_id Integer (required). The reporting period for which area details should be returned. Use [cvd_time_period_list()] to find valid IDs.
#' @param area_id Integer (required). The AreaID to return details for. Use [cvd_area_list()] or [cvd_area_search()] to find valid IDs.
#'
#' @return
#' A named list with up to three tibbles:
#' \describe{
#'   \item{area_details}{A tibble with details about the specified area.}
#'   \item{area_parent_details}{A tibble with details about the parent area(s), if available.}
#'   \item{area_child_details}{A tibble with details about child area(s), if available.}
#' }
#' If no details are found, returns a tibble describing the error.
#'
#' @details
#' This function is useful for navigating NHS area heirarchies, such as finding all practices within a PCN, or determining the parent ICB for a given area. The result is a list of tibbles, so you can extract and work with each component separately.
#'
#' @seealso
#' [cvd_area_list()], [cvd_area_unassigned()], [cvd_area_search()], [cvd_area_nested_subsystems()], [cvd_area_flat_subsystems()]
#'
#' @examples
#' # Retrieve details for 'Leicester, Leicestershire and Rutland ICB' (area_id = 8037) in time period 17
#' returned_list <- cvd_area_details(time_period_id = 17, area_id = 8037)
#'
#' # View details for the area
#' returned_list$area_details |> dplyr::select(AreaCode, AreaName)
#'
#' # View details for the parent area(s)
#' returned_list$area_parent_details |> dplyr::select(AreaID, AreaName, SystemLevelID)
#'
#' # View details for the child area(s)
#' returned_list$area_child_details |> dplyr::select(AreaID, AreaName, SystemLevelID)
#'
#' @export
cvd_area_details <- function(time_period_id, area_id) {
  # validate input
  validate_input_id(
    id = time_period_id,
    param_name = "time_period_id",
    required = TRUE,
    valid_ids = m_get_valid_time_period_ids()
  )
  validate_input_id(
    id = area_id,
    param_name = "area_id",
    required = TRUE
  )

  # compose the request
  req <-
    httr2::request(url_base) |>
    httr2::req_url_path_append(glue::glue('area/{area_id}/details')) |>
    httr2::req_url_query(
      `timePeriodID` = time_period_id
    )

  # perform the request
  resp <- req |>
    httr2::req_perform() |>
    httr2::resp_body_string()

  # wrangle to tibble for output
  data <- jsonlite::fromJSON(resp, flatten = T)$areaDetails |>
    purrr::compact() |>
    dplyr::as_tibble()

  # prepare the return object
  return <- list()

  # select the base fields, exclude any parent or child details
  area_details <- data |>
    dplyr::select(-dplyr::any_of(c('ChildAreaList', 'ParentAreaList'))) |>
    unique()

  return <- return |>
    append(list('area_details' = area_details))

  # extract any parent details
  if ('ParentAreaList' %in% names(data)) {
    area_parent_details <- data$ParentAreaList |>
      purrr::compact() |>
      dplyr::as_tibble() |>
      unique()

    return <- return |>
      append(list('area_parent_details' = area_parent_details))
  }

  # extract any child details
  if ('ChildAreaList' %in% names(data)) {
    area_child_details <- data$ChildAreaList |>
      purrr::compact() |>
      dplyr::as_tibble() |>
      unique()

    return <- return |>
      append(list('area_child_details' = area_child_details))
  }

  # add the full data return (for debugging)
  # return <- return |>
  #   append(list('all_data' = data))

  # return the result
  return(return)
}

#' List NHS areas without parent assignments for a given time period
#'
#' @description
#' Retrieves all NHS areas that have data in the specified reporting time period but do not have any parent areas assigned. These "unassigned" areas are unreachable via standard heirarchical navigation and may represent data issues or exceptional cases (e.g., England as the highest-level system).
#'
#' @section API Documentation:
#' See the [CVDPREVENT API documentation: Areas unassigned](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Farea%2Funassigned)
#'
#' @param time_period_id Integer (required). The reporting period (time period) for which to find unassigned areas. Use [cvd_time_period_list()] to obtain valid IDs.
#' @param system_level_id Integer (optional). Restrict the search to areas at a specific system level (e.g., Practice, PCN, ICB). Use [cvd_area_system_level()] to find valid IDs for a given time period.
#'
#' @return
#' A tibble containing details for all areas without parent assignments in the selected time period (and system leve, if specified). Typical columns include `SystemLevelName`, `AreaID`, `AreaName`, etc.
#' If no unassigned areas are found, returns a tibble describing the error.
#'
#' @details
#' - Use this function to identify "orphaned" NHS areas or to understand top-level areas (e.g., England).
#' - If `system_level_id = 1` (England), expect the only result to be England, since it has no parent.
#' - This can help with data quality checks or to ensure all areas are accessible via parent/child navigation.
#'
#' @seealso
#' [cvd_area_list()], [cvd_area_details()], [cvd_area_search()], [cvd_area_nested_subsystems()], [cvd_area_flat_subsystems()]
#'
#' @examples
#' # Report four GP practices (system level ID = 5) without parent PCN details for time period 17:
#' cvd_area_unassigned(time_period_id = 17, system_level_id = 5) |>
#'   dplyr::slice_head(n = 4) |>
#'   dplyr::select(SystemLevelName, AreaID, AreaName)
#'
#' # List unassigned top-level areas (system level ID = 1, England) for time period 17:
#' cvd_area_unassigned(time_period_id = 17, system_level_id = 1) |>
#'   dplyr::select(SystemLevelName, AreaID, AreaName)
#'
#' @export
cvd_area_unassigned <- function(time_period_id, system_level_id) {
  # validate input
  validate_input_id(
    id = time_period_id,
    param_name = "time_period_id",
    required = TRUE,
    valid_ids = m_get_valid_time_period_ids()
  )
  validate_input_id(
    id = system_level_id,
    param_name = "system_level_id",
    required = FALSE,
    valid_ids = m_get_valid_system_level_id_for_time_period_id(
      time_period_id = time_period_id
    )
  )

  # compose the request
  req <-
    httr2::request(url_base) |>
    httr2::req_url_path_append('area/unassigned')

  if (base::missing(system_level_id)) {
    # just use the time period
    req <- req |>
      httr2::req_url_query(
        `timePeriodID` = time_period_id
      )
  } else {
    req <- req |>
      httr2::req_url_query(
        `timePeriodID` = time_period_id,
        `systemLevelID` = system_level_id
      )
  }

  # perform the request
  resp <- req |>
    httr2::req_perform() |>
    httr2::resp_body_string()

  # wrangle to tibble for output
  data <- jsonlite::fromJSON(resp, flatten = T)$unassignedAreaList

  if (length(data) == 0) {
    cli::cli_alert_danger('No unassigned areas returned')
    return(dplyr::tibble(result = 'No unassigned areas returned'))
  } else {
    data <- data |>
      purrr::compact() |>
      dplyr::as_tibble()

    return(data)
  }
}

#' Search for NHS areas by partial name and time period
#'
#' @description
#' Searches for NHS areas whose names match a given partial string, within a specified reporting time period. This function uses a case-insensitive "LIKE" search (i.e., matches any area containing the search term) and returns only areas for which data is available in the specified period.
#'
#' @section API Documentation:
#' See the [CVDPREVENT API documentation: Area search](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Farea%2Fsearch)
#'
#' @param partial_area_name String (required). The substring to search for within area names (case-insensitive). This may be any part of an area name, e.g., "practice", "PCN", or a specific place.
#' @param time_period_id Integer (required). The reporting period (time period) to restrict the search to areas with data. use [cvd_time_period_list()] to obtain valid IDs.
#'
#' @return
#' A tibble containing details of areas matching the search term and having data for the specified time period. Typical columns include `AreaID`, `AreaName`, `AreaCode`, etc.
#' If no areas are found, returns a tibble describing the error.
#'
#' @details
#' - The search is case-insensitive and matches anywhere in the area name.
#' - Only areas with available data in the chosen time period will be returned.
#' - Use this function to quickly locate AreaIDs or codes for use in other `cvdprevent` API calls.
#'
#' @seealso
#' [cvd_area_list()], [cvd_area_details()], [cvd_area_unassigned()], [cvd_area_nested_subsystems()], [cvd_area_flat_subsystems()]
#'
#' @examples
#' # Search for areas containing "practice" in their name for time period 17
#' cvd_area_search(partial_area_name = "practice", time_period_id = 17) |>
#'   dplyr::select(AreaID, AreaName, AreaCode)
#'
#' # Search for areas containing "PCN" for time period 17
#' cvd_area_search(partial_area_name = "PCN", time_period_id = 17) |>
#'   dplyr::select(AreaID, AreaName, AreaCode)
#'
#' @export
cvd_area_search <- function(partial_area_name, time_period_id) {
  # validate input
  validate_input_string(
    value = partial_area_name,
    param_name = "partial_area_name",
    required = TRUE
  )
  validate_input_id(
    id = time_period_id,
    param_name = "time_period_id",
    required = TRUE,
    valid_ids = m_get_valid_time_period_ids()
  )

  # compose the request
  req <-
    httr2::request(url_base) |>
    httr2::req_url_path_append('area/search') |>
    httr2::req_url_query(
      `partialAreaName` = partial_area_name,
      `timePeriodID` = time_period_id
    )

  # perform the request
  resp <- req |>
    httr2::req_perform() |>
    httr2::resp_body_string()

  # wrangle to tibble for output
  data <- jsonlite::fromJSON(resp, flatten = T)$foundAreaList

  if (length(data) == 0) {
    cli::cli_alert_danger('No found areas returned')
    return(dplyr::tibble(result = 'No found areas returned'))
  } else {
    data <- data |>
      purrr::compact() |>
      dplyr::as_tibble()

    return(data)
  }
}

#' Retrieve nested sub-systems for an NHS area
#'
#' @description
#' Returns a hierarchical (nested) structure of the specified NHS area and all of its descendent (child) areas from the CVDPREVENT API. This function is useful for exploring the parent-child relationships within NHS geographies, such as seeing a PCN and all of its practices, or an ICB with all subordinate structures.
#'
#' The output is a list of tibbles, one for each "level" in the heirarchy, named as `level_1`, `level_2`, etc. Each tibble contains the details for the areas at that level.
#'
#' @section API Documentation:
#' See the [CVDPREVENT API documentation: Area nested subsystems](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Farea%2F%3Carea_id%3E%2FnestedSubSystems)
#'
#' @param area_id Integer (required). The AreaID for which to retrieve nested sub-system data. Use [cvd_area_list()] or [cvd_area_search()] to find valid IDs.
#'
#' @return
#' A named list of tibbles, where each element (`level_1`, `level_2`, etc.) contains details for the specified area and each subsequent child level. If no data is found, returns a tibble describing the error.
#'
#' @details
#' This function is helpful for visualising or programmatically traversing the full nested structure beneath a given NHS area. For example, given an ICB, you can see all PCNs, then all practices beneath those PCNs, and so on.
#'
#' @seealso
#' [cvd_area_list()], [cvd_area_details()], [cvd_area_unassigned()], [cvd_area_search()], [cvd_area_flat_subsystems()]
#'
#' @examples
#' # View the nested structure for Somerset STP (area_id = 5)
#' returned_list <- cvd_area_nested_subsystems(area_id = 5)
#' returned_list |> summary()
#'
#' # See details for the first five immediate children of Somerset STP
#' returned_list$level_2 |> dplyr::slice_head(n = 5)
#'
#' # View the nested structure for Leicester Central PCN (area_id = 701)
#' returned_list <- cvd_area_nested_subsystems(area_id = 701)
#' returned_list |> summary()
#'
#' # See the GP practice children of the PCN
#' returned_list$level_2
#'
#' @export
cvd_area_nested_subsystems <- function(area_id) {
  # validate input
  validate_input_id(
    id = area_id,
    param_name = "area_id",
    required = TRUE
  )

  # compose the request
  req <-
    httr2::request(url_base) |>
    httr2::req_url_path_append(glue::glue('area/{area_id}/nestedSubSystems'))

  # perform the request
  resp <- req |>
    httr2::req_perform() |>
    httr2::resp_body_string()

  # wrangle for output
  data <- jsonlite::fromJSON(resp, flatten = T)[[1]] |>
    purrr::compact() |>
    dplyr::as_tibble()

  if (length(data) == 0) {
    cli::cli_alert_danger('No area returned')
    return(dplyr::tibble(result = 'No area returned'))
  } else {
    # set up the return
    return <- list()

    # extract any children in a loop and add them to the return as named object
    iter <- 0
    while (length(data) > 0) {
      # count iterations
      iter <- iter + 1
      iter_name <- glue::glue('level_{iter}')

      # ensure data is a tibble
      data <- data |>
        purrr::compact() |>
        dplyr::as_tibble()

      # extract the details for the current level excluding children
      level <- data |>
        dplyr::select(-dplyr::any_of('Children')) |>
        dplyr::distinct()

      # get the children details
      child <- data |>
        dplyr::select(dplyr::any_of('Children')) |>
        tidyr::unnest(cols = dplyr::any_of('Children'))

      # add current level details to the return object if contains data
      if (length(data) > 0) {
        return <- return |>
          append(setNames(list(level), iter_name))
      }

      # set data to be children (for the loop)
      data <- child
    }

    return(return)
  }
}

#' Retieve flat sub-systems for an NHS area, grouped by system level
#'
#' @description
#' Returns a "flat" list of the specified NHS area and all its immediate child areas from the CVDPREVENT API, with child areas grouped by their system level rather than by strict heirarchical nesting. This function provides a convenient overview when you want to see all sub-areas organised by level (e.g., all PCNs and all GP practices beneath an ICB) without traversing the full heirarchy.
#'
#' The output is a tibble where each row represents an area or sub-area, and child areas are included as columns (with system level information).
#'
#' @section API Documentation:
#' See the [CVDPREVENT API documentation: Area flat subsystems](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Farea%2F%3Carea_id%3E%2FflatSubSystems)
#'
#' @param area_id Integer (required). The AreaID for which to retrieve flat sub-system data. use [cvd_area_list()] or [cvd_area_search()] to find valid IDs.
#'
#' @return
#' A tibble containing details for the specified area and its child areas, with columns such as `AreaID`, `AreaName`, `SystemLevelID`, `SystemLevelName`, and child area details (e.g., via `SubSystems_*` columns).
#' If no areas are found, returns a tibble describing the error.
#'
#' @details
#' This function is useful for quickly listing all areas beneath a parent, grouped by system level, for reporting or selection purposes. For a fully nested view, see [cvd_area_nested_subsystems()].
#'
#' @seealso
#' [cvd_area_list()], [cvd_area_details()], [cvd_area_unassigned()], [cvd_area_search()], [cvd_area_nested_subsystems()]
#'
#' @examples
#' # View flat sub-systems for Somerset STP (area_id = 5)
#' cvd_area_flat_subsystems(area_id = 5) |> dplyr::glimpse()
#'
#' # View flat sub-systems for Lincolnshire ICB (area_id = 8042)
#' cvd_area_flat_subsystems(area_id = 8042) |> dplyr::glimpse()
#'
#' @export
cvd_area_flat_subsystems <- function(area_id) {
  # validate input
  validate_input_id(
    id = area_id,
    param_name = "area_id",
    required = TRUE
  )

  # compose the request
  req <-
    httr2::request(url_base) |>
    httr2::req_url_path_append(glue::glue('area/{area_id}/flatSubSystems'))

  # perform the request
  resp <- req |>
    httr2::req_perform() |>
    httr2::resp_body_string()

  # wrangle for output
  data <- jsonlite::fromJSON(resp, flatten = T)[[1]]

  if (length(data) == 0) {
    cli::cli_alert_danger('No areas returned')
    return(dplyr::tibble(result = 'No areas returned'))
  } else {
    data <- data |>
      purrr::compact() |>
      dplyr::as_tibble() |>
      dplyr::relocate(SubSystems, .after = dplyr::last_col()) |>
      tidyr::unnest(cols = SubSystems, names_sep = '_')

    return(data)
  }
}

## indicators ------------------------------------------------------------------

#' List available indicators for a system level and time period
#'
#' @description
#' Retrieves basic details for all CVD indicators available for a given system level and reportion period from the CVDPREVENT API. Only indicators with data for the selected time period and system level are returned. This function is commonly used to populate indicator pickers or to discover what data is available for further queries.
#'
#' @section API Docuemtnation:
#' See the [CVDPREVENT API documentation: Indicator list](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Findicator%2Flist)
#'
#' @param time_period_id Integer (required). The reporting period (time period) for which to return indicators. Use [cvd_time_period_list()] to find valid IDs.
#' @param system_level_id Integer (required). The system level (e.g., National, Region, ICB, PCN, Practice) for which to return indicators. Use [cvd_area_system_level()] to find valid IDs for a given time period.
#'
#' @return
#' A tibble with one row per available indicator for the specified system level and time period. Typical columns include (but are not limited to) `IndicatorID`, `IndicatorCode`, `IndicatorShortName`, `IndicatorName`, `IndicatorStatus`, `IndicatorFormatID`, and others describing indicator properties.
#' If no indicators are found, returns a tibble describing the error.
#'
#' @details
#' Use this function to discover which indicators are available for a specific combination of system level and time period. The results can be joined with other outputs for further analysis, or used as the basis for more detailed indicator, metric, or data queries.
#'
#' @seealso
#' [cvd_indicator_metric_list()], [cvd_indicator()], [cvd_indicator_tags()], [cvd_indicator_details()], [cvd_indicator_sibling()], [cvd_indicator_child_data()], [cvd_indicator_data()], [cvd_indicator_metric_data()], [cvd_indicator_raw_data()], [cvd_indicator_nationalarea_metric_data()], [cvd_indicator_priority_groups()], [cvd_indicator_pathway_group()], [cvd_indicator_group()], [cvd_indicator_metric_timeseries()], [cvd_indicator_person_timeseries()], [cvd_indicator_metric_systemlevel_comparison()], [cvd_indicator_metric_area_breakdown()]
#'
#' @examples
#' # List four indicators for time period 17 and GP practice level (system level 5)
#' cvd_indicator_list(time_period_id = 17, system_level_id = 5) |>
#'   dplyr::select(IndicatorID, IndicatorCode, IndicatorShortName) |>
#'   dplyr::slice_head(n = 4)
#'
#' # Find valid time period and system level IDs, then list all available indicators
#' valid_periods <- cvd_time_period_list()
#' valid_levels <- cvd_area_system_level(time_period_id = 17)
#' cvd_indicator_list(time_period_id = 17, system_level_id = valid_levels$SystemLevelID[1])
#'
#' @export
cvd_indicator_list <- function(time_period_id = NULL, system_level_id = NULL) {
  # validate input
  validate_input_id(
    id = time_period_id,
    param_name = "time_period_id",
    required = TRUE,
    valid_ids = m_get_valid_time_period_ids()
  )
  validate_input_id(
    id = system_level_id,
    param_name = "system_level_id",
    required = TRUE,
    valid_ids = m_get_valid_system_level_id_for_time_period_id(
      time_period_id = time_period_id
    )
  )

  # compose the request
  req <-
    httr2::request(url_base) |>
    httr2::req_url_path_append('indicator/list') |>
    httr2::req_url_query(
      `timePeriodID` = time_period_id,
      `systemLevelID` = system_level_id
    )

  # perform the request
  resp <- req |>
    httr2::req_perform() |>
    httr2::resp_body_string()

  # wrangle to tibble for output
  data <- jsonlite::fromJSON(resp, flatten = T)$indicatorList

  if (length(data) == 0) {
    cli::cli_alert_danger('No indicators returned')
    return(dplyr::tibble(result = 'No indicators returned'))
  } else {
    data <- data |>
      purrr::compact() |>
      dplyr::as_tibble()

    return(data)
  }
}

#' List metrics for indicators
#'
#' Returns same data as cvd_indicator_list() but adds a 'MetricList' array for
#' each indicator, containing details of the relevant metrics. Only returns
#' indicators for which data exists in selected time period, and on selected
#' system level.
#'
#' CVD Prevent API documentation:
#' [Indicator metric list](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#*Proposed*%2Findicator%2FmetricList)
#'
#' @param time_period_id integer - time period to return data for (compulsory)
#' @param system_level_id integer - system level to return data for (compulsory)
#'
#' @return Tibble of details for indicators and associated metrics
#' @export
#' @seealso [cvd_indicator_list()], [cvd_indicator()],
#' [cvd_indicator_tags()], [cvd_indicator_details()], [cvd_indicator_sibling()],
#' [cvd_indicator_child_data()], [cvd_indicator_data()], [cvd_indicator_metric_data()],
#' [cvd_indicator_raw_data()], [cvd_indicator_nationalarea_metric_data()],
#' [cvd_indicator_priority_groups()], [cvd_indicator_pathway_group()],
#' [cvd_indicator_group()], [cvd_indicator_metric_timeseries()],
#' [cvd_indicator_person_timeseries()], [cvd_indicator_metric_systemlevel_comparison()],
#' [cvd_indicator_metric_area_breakdown()]
#'
#' @examples
#' # List metrics for the prevalence of atrial fibrillation (indicator ID 1),
#' # focussing on just those metrics for the 40-59 years age group:
#' cvd_indicator_metric_list(time_period_id = 17, system_level_id = 1) |>
#'   dplyr::filter(IndicatorID == 1, MetricCategoryName == '40-59') |>
#'   dplyr::count(IndicatorID, IndicatorShortName, MetricID, MetricCategoryName, CategoryAttribute) |>
#'   dplyr::select(-n)
cvd_indicator_metric_list <- function(time_period_id, system_level_id) {
  # validate_input
  validate_input_id(
    id = time_period_id,
    param_name = "time_period_id",
    required = TRUE,
    valid_ids = m_get_valid_time_period_ids()
  )
  validate_input_id(
    id = system_level_id,
    param_name = "system_level_id",
    required = TRUE,
    valid_ids = m_get_valid_system_level_id_for_time_period_id(
      time_period_id = time_period_id
    )
  )

  # compose the request
  req <-
    httr2::request(url_base) |>
    httr2::req_url_path_append('indicator/metricList') |>
    httr2::req_url_query(
      `timePeriodID` = time_period_id,
      `systemLevelID` = system_level_id
    )

  # perform the request
  resp <- req |>
    httr2::req_perform() |>
    httr2::resp_body_string()

  # wrangle to tibble for output
  data <- jsonlite::fromJSON(resp, flatten = T)[[2]]

  if (length(data) == 0) {
    cli::cli_alert_danger('No indicators returned')
    return(dplyr::tibble(result = 'No indicators returned'))
  } else {
    data <- data |>
      purrr::compact() |>
      dplyr::as_tibble()

    if ('MetricList' %in% names(data)) {
      data <-
        data |>
        dplyr::relocate(
          dplyr::any_of(c('MetricList')),
          .after = dplyr::last_col()
        ) |>
        tidyr::unnest(cols = dplyr::any_of(c('MetricList')))
    }

    return(data)
  }
}

#' Indicators
#'
#' Returns all indicators and data for a given time period and area. Also returns
#' time series data for all time periods available. If tags are specified, only
#' indicators which have one of the specified tags will be returned.
#'
#' CVD Prevent API documentation:
#' [Indicator](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Findicator)
#'
#' @param time_period_id integer - time period to return data for (compulsory)
#' @param area_id integer - area to return data for (compulsory)
#' @param tag_id numeric vector - allows filtering indicators by one or more tags (optional)
#'
#' @return List of named tibbles (indicators, categories, category_data, timeseries_data, all_data)
#' @export
#' @seealso [cvd_indicator_list()], [cvd_indicator_metric_list()],
#' [cvd_indicator_tags()], [cvd_indicator_details()], [cvd_indicator_sibling()],
#' [cvd_indicator_child_data()], [cvd_indicator_data()], [cvd_indicator_metric_data()],
#' [cvd_indicator_raw_data()], [cvd_indicator_nationalarea_metric_data()],
#' [cvd_indicator_priority_groups()], [cvd_indicator_pathway_group()], #
#' [cvd_indicator_group()], [cvd_indicator_metric_timeseries()],
#' [cvd_indicator_person_timeseries()], [cvd_indicator_metric_systemlevel_comparison()],
#' [cvd_indicator_metric_area_breakdown()]
#'
#' @examples
#' # Returns a list of named tibbles. To use we need to unpack the list:
#' return_list <- cvd_indicator(time_period_id = 17, area_id = 1103)
#'
#' # See what the list contains
#' return_list |> summary()
#'
#' # extract the indicators
#' indicators <- return_list$indicators
#' indicators |>
#'   dplyr::select(IndicatorID, IndicatorCode, IndicatorShortName) |>
#'   dplyr::arrange(IndicatorID) |>
#'   dplyr::slice_head(n = 4)
#'
#' # extract the metric categories
#' categories <- return_list$metric_categories
#' categories |>
#'   dplyr::filter(IndicatorID == 7, MetricCategoryID %in% c(7, 8)) |>
#'   dplyr::select(IndicatorID, MetricCategoryTypeName,
#'   CategoryAttribute, MetricCategoryName, MetricID)
#'
#' # extract metric data
#' metric_data <- return_list$metric_data
#' metric_data |>
#'   dplyr::filter(MetricID %in% c(126, 132)) |>
#'   dplyr::select(MetricID, Value, Numerator, Denominator)
#'
#' # extract the time series data
#' timeseries_data <- return_list$timeseries_data
#' timeseries_data |>
#'   dplyr::filter(MetricID %in% c(126, 132), !is.na(Value))
#'
#' # indicators are searcheable by one or more Tag.
#' return_list <-
#'   cvd_indicator(time_period_id = 17, area_id = 3, tag_id = c(3, 4))
cvd_indicator <- function(time_period_id, area_id, tag_id) {
  # validate input
  validate_input_id(
    id = time_period_id,
    param_name = "time_period_id",
    required = TRUE,
    valid_ids = m_get_valid_time_period_ids()
  )
  validate_input_id(
    id = area_id,
    param_name = "area_id",
    required = TRUE
  )
  validate_input_id_vector(
    ids = tag_id,
    param_name = "tag_id",
    required = FALSE,
    valid_ids = m_get_valid_tag_ids()
  )

  # compose the request
  if (base::missing(tag_id)) {
    req <- httr2::request(url_base) |>
      httr2::req_url_path_append('indicator') |>
      httr2::req_url_query(
        `timePeriodID` = time_period_id,
        `areaID` = area_id
      )
  } else {
    req <-
      httr2::request(url_base) |>
      httr2::req_url_path_append('indicator') |>
      httr2::req_url_query(
        `timePeriodID` = time_period_id,
        `areaID` = area_id,
        `tagID` = tag_id,
        .multi = 'explode'
      )
  }

  # perform the request
  resp <- req |>
    httr2::req_perform() |>
    httr2::resp_body_string()

  # wrangle for output
  data <- jsonlite::fromJSON(resp, flatten = T)$indicatorList

  if (length(data) == 0) {
    cli::cli_alert_danger('No indicators returned')
    return(dplyr::tibble(result = 'No indicators returned'))
  } else {
    # compose the return list
    return <- list()

    # prepare data as tibble
    data <- data |>
      purrr::compact() |>
      dplyr::as_tibble()

    # Indicator data
    indicators <- data |>
      dplyr::select(-dplyr::any_of('Categories')) |>
      dplyr::distinct()

    return <- return |>
      append(list('indicators' = indicators))

    # Metric categories
    if ('Categories' %in% names(data)) {
      # extract metric data
      metrics <- data |>
        dplyr::select(c(IndicatorID, dplyr::any_of('Categories'))) |>
        tidyr::unnest(cols = dplyr::any_of(c('Categories')))

      # extract metric categories
      metric_categories <- metrics |>
        dplyr::select(
          -c(dplyr::any_of(c('TimeSeries')), dplyr::starts_with('Data.'))
        ) |>
        dplyr::distinct()

      return <- return |>
        append(list('metric_categories' = metric_categories))

      # extract metric data
      metric_data <- metrics |>
        dplyr::select(c(MetricID, dplyr::starts_with('Data.'))) |>
        tidyr::unnest(cols = dplyr::any_of('Data')) |>
        dplyr::rename_with(
          .cols = dplyr::starts_with('Data.'),
          ~ base::gsub(pattern = 'Data.', replacement = '', x = .x, fixed = T)
        ) |>
        dplyr::distinct()

      return <- return |>
        append(list('metric_data' = metric_data))

      # extract timeseries data
      timeseries <- metrics |>
        dplyr::select(c(MetricID, dplyr::any_of('TimeSeries'))) |>
        tidyr::unnest(cols = dplyr::any_of('TimeSeries')) |>
        dplyr::distinct()

      return <- return |>
        append(list('timeseries_data' = timeseries))
    }

    return(return)
  }
}

#' Indicator tags
#'
#' Returns a list of all available tags, which can be used to filter indicators.
#'
#' CVD Prevent API documentation:
#' [Indicator tags](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Findicator%2Ftags)
#'
#' @return Tibble of details for indicator tags
#' @export
#' @seealso [cvd_indicator_list()], [cvd_indicator_metric_list()], [cvd_indicator()],
#' [cvd_indicator_details()], [cvd_indicator_sibling()],
#' [cvd_indicator_child_data()], [cvd_indicator_data()], [cvd_indicator_metric_data()],
#' [cvd_indicator_raw_data()], [cvd_indicator_nationalarea_metric_data()],
#' [cvd_indicator_priority_groups()], [cvd_indicator_pathway_group()], #
#' [cvd_indicator_group()], [cvd_indicator_metric_timeseries()],
#' [cvd_indicator_person_timeseries()], [cvd_indicator_metric_systemlevel_comparison()],
#' [cvd_indicator_metric_area_breakdown()]
#'
#' @examples
#' cvd_indicator_tags() |>
#'   dplyr::arrange(IndicatorTagID) |>
#'   dplyr::slice_head(n = 5)
cvd_indicator_tags <- function() {
  # compose the request
  req <-
    httr2::request(url_base) |>
    httr2::req_url_path_append('indicator/tags')

  # perform the request
  resp <- req |>
    httr2::req_perform() |>
    httr2::resp_body_string()

  # wrangle to tibble for output
  data <- jsonlite::fromJSON(resp, flatten = T)$indicatorTagList

  if (length(data) == 0) {
    cli::cli_alert_danger('No indicators tags returned')
    return(dplyr::tibble(result = 'No indicator tags returned'))
  } else {
    data <- data |>
      dplyr::as_tibble() |>
      dplyr::distinct() |>
      dplyr::arrange(IndicatorTagID)

    return(data)
  }
}

#' Indicator details
#'
#' Returns details of a single indicator
#'
#' CVD Prevent API documentation:
#' [Indicator details](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Findicator%2F%3Cindicator_ID%3E%2Fdetails)
#'
#' @param indicator_id integer - the ID for the indicator (compulsory)
#'
#' @return Tibble of details for the specified indicator
#' @export
#' @seealso [cvd_indicator_list()], [cvd_indicator_metric_list()], [cvd_indicator()],
#' [cvd_indicator_tags()], [cvd_indicator_sibling()],
#' [cvd_indicator_child_data()], [cvd_indicator_data()], [cvd_indicator_metric_data()],
#' [cvd_indicator_raw_data()], [cvd_indicator_nationalarea_metric_data()],
#' [cvd_indicator_priority_groups()], [cvd_indicator_pathway_group()], #
#' [cvd_indicator_group()], [cvd_indicator_metric_timeseries()],
#' [cvd_indicator_person_timeseries()], [cvd_indicator_metric_systemlevel_comparison()],
#' [cvd_indicator_metric_area_breakdown()]
#'
#' @examples
#' cvd_indicator_details(indicator_id = 7) |>
#'   dplyr::select(IndicatorID, MetaDataTitle, MetaData) |>
#'   dplyr::slice_head(n=5)
cvd_indicator_details <- function(indicator_id) {
  # validate input
  validate_input_id(
    id = indicator_id,
    param_name = "indicator_id",
    required = TRUE
  )

  # compose the request
  req <-
    httr2::request(url_base) |>
    httr2::req_url_path_append(glue::glue('indicator/{indicator_id}/details'))

  # catch errors caused by bad url - because indicator id is invalid
  tryCatch(
    {
      # perform the request
      resp <- req |>
        httr2::req_perform() |>
        httr2::resp_body_string()

      # wrangle for output
      data <- jsonlite::fromJSON(resp, flatten = T)$indicatorDetails

      if (length(data) == 0) {
        cli::cli_alert_danger('No indicator details returned')
        return(dplyr::tibble(result = 'No indicator details returned'))
      } else {
        data <- data |>
          purrr::compact() |>
          dplyr::as_tibble() |>
          dplyr::relocate(MetaData, .after = dplyr::last_col()) |>
          tidyr::unnest(col = MetaData)

        return(data)
      }
    },
    httr2_error = function(e) {
      internal_try_catch_html500(error = e, msg = 'Indicator ID is invalid')
    }
  )
}

#' Indicator sibling data
#'
#' Returns all sibling areas and their data for specified time period, area
#' and metric. This endpoint is intended to only return data for selected metric,
#' and not all metrics for a chosen indicator, hence the metric_id query parameter.
#'
#' CVD Prevent API documentation:
#' [Indicator sibling data](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Findicator%2FsiblingData)
#'
#' @param time_period_id integer - time period for which to return data (compulsory)
#' @param area_id integer - area for which all sibling data will be returned (compulsory)
#' @param metric_id integer - metric for which to return data (compulsory)
#'
#' @return Tibble of data for indicators for the area and its siblings in the time period
#' @export
#' @seealso [cvd_indicator_list()], [cvd_indicator_metric_list()], [cvd_indicator()],
#' [cvd_indicator_tags()], [cvd_indicator_details()],
#' [cvd_indicator_child_data()], [cvd_indicator_data()], [cvd_indicator_metric_data()],
#' [cvd_indicator_raw_data()], [cvd_indicator_nationalarea_metric_data()],
#' [cvd_indicator_priority_groups()], [cvd_indicator_pathway_group()], #
#' [cvd_indicator_group()], [cvd_indicator_metric_timeseries()],
#' [cvd_indicator_person_timeseries()], [cvd_indicator_metric_systemlevel_comparison()],
#' [cvd_indicator_metric_area_breakdown()]
#'
#' @examples
#' cvd_indicator_sibling(time_period_id = 17, area_id = 1103, metric_id = 126) |>
#'   dplyr::select(AreaID, AreaName, Value, LowerConfidenceLimit, UpperConfidenceLimit)
cvd_indicator_sibling <- function(
  time_period_id,
  area_id,
  metric_id
) {
  # validate input
  validate_input_id(
    id = time_period_id,
    param_name = "time_period_id",
    required = TRUE,
    valid_ids = m_get_valid_time_period_ids()
  )
  validate_input_id(
    id = area_id,
    param_name = "area_id",
    required = TRUE
  )
  validate_input_id(
    id = metric_id,
    param_name = "metric_id",
    required = TRUE
  )

  # compose the request
  req <-
    httr2::request(url_base) |>
    httr2::req_url_path_append('indicator/siblingData') |>
    httr2::req_url_query(
      `timePeriodID` = time_period_id,
      `areaID` = area_id,
      `metricID` = metric_id
    )

  # perform the request
  resp <- req |>
    httr2::req_perform() |>
    httr2::resp_body_string()

  # wrangle for output
  data <- jsonlite::fromJSON(resp, flatten = T)[[2]]

  if (length(data) == 0) {
    cli::cli_alert_danger('No sibling details returned')
    return(dplyr::tibble(result = 'No sibling details returned'))
  } else {
    data <- data |>
      purrr::compact() |>
      dplyr::as_tibble() |>
      dplyr::select(
        -dplyr::any_of(c(
          'NotificationCount',
          'HighestPriorityNotificationType'
        ))
      ) |> # prevents conflicts with column of same name in nested data
      dplyr::relocate(Data, .after = dplyr::last_col()) |>
      tidyr::unnest(col = Data)

    return(data)
  }
}

#' Indicator child data
#'
#' Returns all children areas and their data for specified time period, area
#' and metric. This endpoint is intended to only return data for selected
#' metric, and not all metrics for indicators, hence the metricID query
#' parameter.
#'
#' CVD Prevent API documentation:
#' [Indicator child data](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Findicator%2FchildData)
#'
#' @param time_period_id integer - time period for which to return data (compulsory)
#' @param area_id integer - area for which all children data will be returned (compulsory)
#' @param metric_id integer - metric for which to return data (compulsory)
#'
#' @return Tibble of details for the specified metric in the child areas of the specified area
#' @export
#' @seealso [cvd_indicator_list()], [cvd_indicator_metric_list()], [cvd_indicator()],
#' [cvd_indicator_tags()], [cvd_indicator_details()], [cvd_indicator_sibling()],
#' [cvd_indicator_data()], [cvd_indicator_metric_data()],
#' [cvd_indicator_raw_data()], [cvd_indicator_nationalarea_metric_data()],
#' [cvd_indicator_priority_groups()], [cvd_indicator_pathway_group()], #
#' [cvd_indicator_group()], [cvd_indicator_metric_timeseries()],
#' [cvd_indicator_person_timeseries()], [cvd_indicator_metric_systemlevel_comparison()],
#' [cvd_indicator_metric_area_breakdown()]
#'
#' @examples
#' cvd_indicator_child_data(time_period_id = 17, area_id = 74, metric_id = 126) |>
#'   dplyr::select(AreaID, AreaName, Value, LowerConfidenceLimit, UpperConfidenceLimit)
cvd_indicator_child_data <- function(
  time_period_id,
  area_id,
  metric_id
) {
  # validate input
  validate_input_id(
    id = time_period_id,
    param_name = "time_period_id",
    required = TRUE,
    valid_ids = m_get_valid_time_period_ids()
  )
  validate_input_id(
    id = area_id,
    param_name = "area_id",
    required = TRUE
  )
  validate_input_id(
    id = metric_id,
    param_name = "metric_id",
    required = TRUE
  )

  # compose the request
  req <-
    httr2::request(url_base) |>
    httr2::req_url_path_append('indicator/childData') |>
    httr2::req_url_query(
      `timePeriodID` = time_period_id,
      `areaID` = area_id,
      `metricID` = metric_id
    )

  # perform the request
  resp <- req |>
    httr2::req_perform() |>
    httr2::resp_body_string()

  # wrangle for output
  data <- jsonlite::fromJSON(resp, flatten = T)[[1]]

  if (length(data) == 0) {
    cli::cli_alert_danger('No child details returned')
    return(dplyr::tibble(result = 'No child details returned'))
  } else {
    data <- data |>
      purrr::compact() |>
      dplyr::as_tibble() |>
      dplyr::select(
        -dplyr::any_of(c(
          'NotificationCount',
          'HighestPriorityNotificationType'
        ))
      ) |> # prevents conflicts with column of same name in nested data
      dplyr::relocate(Data, .after = dplyr::last_col()) |>
      tidyr::unnest(col = Data)

    return(data)
  }
}

#' Indicator data
#'
#' Returns all metric data for a specified indicator. Data will include values
#' for both selected area, and organisation at National Level (usually England).
#'
#' CVD Prevent API documentation:
#' [Indicator data](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Findicator%2F%3Cindicator_id%3E%2Fdata)
#'
#' @param indicator_id integer - indicator for which to return data (compulsory)
#' @param time_period_id integer - time period for which to return data for (compulsory)
#' @param area_id integer - area for which to return data for (compulsory)
#'
#' @return Tibble of details for the indicator in the area and a national comparison
#' @export
#' @seealso [cvd_indicator_list()], [cvd_indicator_metric_list()], [cvd_indicator()],
#' [cvd_indicator_tags()], [cvd_indicator_details()], [cvd_indicator_sibling()],
#' [cvd_indicator_child_data()], [cvd_indicator_metric_data()],
#' [cvd_indicator_raw_data()], [cvd_indicator_nationalarea_metric_data()],
#' [cvd_indicator_priority_groups()], [cvd_indicator_pathway_group()], #
#' [cvd_indicator_group()], [cvd_indicator_metric_timeseries()],
#' [cvd_indicator_person_timeseries()], [cvd_indicator_metric_systemlevel_comparison()],
#' [cvd_indicator_metric_area_breakdown()]
#'
#' @examples
#' # Look at 'AF: treatment with anticoagulants' (indicator ID 7) in time
#' # period 17 for  'Leicester Central PCN' (area_id 701) focussed on metrics
#' # by gender:
#' cvd_indicator_data(time_period_id = 17, indicator_id = 7, area_id = 701) |>
#'   dplyr::filter(MetricCategoryTypeName == 'Sex') |>
#'   dplyr::select(MetricID, MetricCategoryName, AreaData.AreaName,
#'   AreaData.Value, NationalData.AreaName, NationalData.Value)
cvd_indicator_data <- function(
  indicator_id,
  time_period_id,
  area_id
) {
  # validate input
  validate_input_id(
    id = indicator_id,
    param_name = "indicator_id",
    required = TRUE
  )
  validate_input_id(
    id = time_period_id,
    param_name = "time_period_id",
    required = TRUE,
    valid_ids = m_get_valid_time_period_ids()
  )
  validate_input_id(
    id = area_id,
    param_name = "area_id",
    required = TRUE
  )

  # compose the request
  req <-
    httr2::request(url_base) |>
    httr2::req_url_path_append(glue::glue('indicator/{indicator_id}/data')) |>
    httr2::req_url_query(
      `timePeriodID` = time_period_id,
      `areaID` = area_id
    )

  # catch errors caused by bad url - because indicator id is invalid
  tryCatch(
    {
      # perform the request
      resp <- req |>
        httr2::req_perform() |>
        httr2::resp_body_string()

      # wrangle for output
      data <- jsonlite::fromJSON(resp, flatten = T)[[2]]

      if (length(data) == 0) {
        cli::cli_alert_danger('No indicator details returned')
        return(dplyr::tibble(result = 'No indicator details returned'))
      } else {
        data <- data |>
          purrr::compact() |>
          dplyr::as_tibble() |>
          dplyr::relocate(Categories, .after = dplyr::last_col()) |>
          tidyr::unnest(col = Categories)

        return(data)
      }
    },
    httr2_error = function(e) {
      internal_try_catch_html500(error = e, msg = 'Indicator ID is invalid')
    }
  )
}


#' Metric data
#'
#' Returns all metric data for a specified metric. Data will include values
#' for both selected area and organisation at National Level (usually England).
#'
#' CVD Prevent API documentation:
#' [Indicator metric data](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#*Proposed*%2Findicator%2Fmetric%2F%3Cmetric_id%3E%2Fdata)
#'
#' @param metric_id integer - metric for which to return data for (compulsory)
#' @param time_period_id integer - time period for which to return data for (compulsory)
#' @param area_id integer - area for which to return data for (compulsory)
#'
#' @return Tibble of details for metric performance in the area and time period
#' @export
#' @seealso [cvd_indicator_list()], [cvd_indicator_metric_list()], [cvd_indicator()],
#' [cvd_indicator_tags()], [cvd_indicator_details()], [cvd_indicator_sibling()],
#' [cvd_indicator_child_data()], [cvd_indicator_data()],
#' [cvd_indicator_raw_data()], [cvd_indicator_nationalarea_metric_data()],
#' [cvd_indicator_priority_groups()], [cvd_indicator_pathway_group()], #
#' [cvd_indicator_group()], [cvd_indicator_metric_timeseries()],
#' [cvd_indicator_person_timeseries()], [cvd_indicator_metric_systemlevel_comparison()],
#' [cvd_indicator_metric_area_breakdown()]
#'
#' @examples
#' # Retrieve metric data for AF: treatment with anticoagulants for 'Alliance
#' # PCN' (area ID 399) in time period 1 for metric 126 (breakdown by age group:
#' # males aged 40-59):
#' cvd_indicator_metric_data(metric_id = 126, time_period_id = 1, area_id = 399) |>
#'   dplyr::select(IndicatorShortName, CategoryAttribute, MetricCategoryName,
#'   AreaData.Value, NationalData.Value)
cvd_indicator_metric_data <- function(
  metric_id,
  time_period_id,
  area_id
) {
  # validate input
  validate_input_id(
    id = metric_id,
    param_name = "metric_id",
    required = TRUE
  )
  validate_input_id(
    id = time_period_id,
    param_name = "time_period_id",
    required = TRUE,
    valid_ids = m_get_valid_time_period_ids()
  )
  validate_input_id(
    id = area_id,
    param_name = "area_id",
    required = TRUE
  )

  # compose the request
  req <-
    httr2::request(url_base) |>
    httr2::req_url_path_append(glue::glue(
      'indicator/metric/{metric_id}/data'
    )) |>
    httr2::req_url_query(
      `timePeriodID` = time_period_id,
      `areaID` = area_id
    )

  # catch errors caused by bad url - because metric id is invalid
  tryCatch(
    {
      # perform the request
      resp <- req |>
        httr2::req_perform() |>
        httr2::resp_body_string()

      # wrangle for output
      data <- jsonlite::fromJSON(resp, flatten = T)[[2]]

      if (length(data) == 0) {
        cli::cli_alert_danger('No metric details returned')
        return(dplyr::tibble(result = 'No metric details returned'))
      } else {
        data <- data |>
          purrr::compact() |>
          dplyr::as_tibble() |>
          dplyr::relocate(Categories, .after = dplyr::last_col()) |>
          tidyr::unnest(col = Categories)

        return(data)
      }
    },
    httr2_error = function(e) {
      internal_try_catch_html500(error = e, msg = 'Metric ID is invalid')
    }
  )
}

#' Indicator raw data (JSON)
#'
#' Returns all metric data for a specified indicator, system level and time
#' period.
#'
#' CVD Prevent API documentation:
#' [Indicator raw data JSON](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Findicator%2F%3Cindicator_ID%3E%2FrawDataJSON)
#'
#' @param indicator_id integer - indicator for which to return data for (compulsory)
#' @param time_period_id integer - time period for which to return data for (compulsory)
#' @param system_level_id integer - system level for which to return data for (compulsory)
#'
#' @return Tibble of metric performance details for a specified indicator across the system level
#' @export
#' @seealso [cvd_indicator_list()], [cvd_indicator_metric_list()], [cvd_indicator()],
#' [cvd_indicator_tags()], [cvd_indicator_details()], [cvd_indicator_sibling()],
#' [cvd_indicator_child_data()], [cvd_indicator_data()], [cvd_indicator_metric_data()],
#' [cvd_indicator_nationalarea_metric_data()],
#' [cvd_indicator_priority_groups()], [cvd_indicator_pathway_group()], #
#' [cvd_indicator_group()], [cvd_indicator_metric_timeseries()],
#' [cvd_indicator_person_timeseries()], [cvd_indicator_metric_systemlevel_comparison()],
#' [cvd_indicator_metric_area_breakdown()]
#'
#' @examples
#' # return all metric data for indicator 'AF: treatment with anticoagulants'
#' # (indicator ID 7) in time period 17 at GP practice level (system level ID 5):
#' cvd_indicator_raw_data(indicator_id = 7, time_period_id = 17, system_level_id = 5) |>
#'   dplyr::slice_head(n = 5) |>
#'   dplyr::select(AreaCode, AreaName, Value)
cvd_indicator_raw_data <- function(
  indicator_id,
  time_period_id,
  system_level_id
) {
  # validate input
  validate_input_id(
    id = indicator_id,
    param_name = "indicator_id",
    required = TRUE
  )
  validate_input_id(
    id = time_period_id,
    param_name = "time_period_id",
    required = TRUE,
    valid_ids = m_get_valid_time_period_ids()
  )
  validate_input_id(
    id = system_level_id,
    param_name = "system_level_id",
    required = TRUE,
    valid_ids = m_get_valid_system_level_id_for_time_period_id(
      time_period_id = time_period_id
    )
  )

  # compose the request
  req <-
    httr2::request(url_base) |>
    httr2::req_url_path_append(glue::glue(
      'indicator/{indicator_id}/rawDataJSON'
    )) |>
    httr2::req_url_query(
      `timePeriodID` = time_period_id,
      `systemLevelID` = system_level_id
    )

  # catch errors caused by bad url - because indicator id is invalid
  tryCatch(
    {
      # perform the request
      resp <- req |>
        httr2::req_perform() |>
        httr2::resp_body_string()

      # wrangle for output
      data <- jsonlite::fromJSON(resp, flatten = T)[[2]]

      if (length(data) == 0) {
        cli::cli_alert_danger('No indicator details returned')
        return(dplyr::tibble(result = 'No indicator details returned'))
      } else {
        data <- data |>
          purrr::compact() |>
          dplyr::as_tibble()

        return(data)
      }
    },
    httr2_error = function(e) {
      internal_try_catch_html500(error = e, msg = 'Metric ID is invalid')
    }
  )
}

#' Indicator national vs area metric data
#'
#' Returns national and area data for the provided metric, area, and time period.
#'
#' The returned object is a list containing named tibbles. The two possible
#' tibbles are:
#' * `area`: contains metric data for the specified area in comparison with national metric data.
#'
#' * `target`: contains details on how to reach target values, including:
#'     * target value as a percentage (stored as a whole number up to 100)
#'     * target patients (the number of additional patients needed to reach the target percentage)
#'
#' Note that the `target` tibble is only provided if data is available for both
#' national and the chosen area.
#'
#' CVD Prevent API documentation:
#' [Indicator national vs area metric data](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Findicator%2FnationalVsAreaMetricData%2F%3Cmetric_ID%3E)
#'
#' @param metric_id integer - metric for which to return data (compulsory)
#' @param time_period_id integer - time period for which to return data (compulsory)
#' @param area_id integer - area for which to return data (compulsory)
#'
#' @return List of named tibbles (`area`, `target`) where `target` is only provided if data is available.
#' @export
#' @seealso [cvd_indicator_list()], [cvd_indicator_metric_list()], [cvd_indicator()],
#' [cvd_indicator_tags()], [cvd_indicator_details()], [cvd_indicator_sibling()],
#' [cvd_indicator_child_data()], [cvd_indicator_data()], [cvd_indicator_metric_data()],
#' [cvd_indicator_raw_data()],
#' [cvd_indicator_priority_groups()], [cvd_indicator_pathway_group()],
#' [cvd_indicator_group()], [cvd_indicator_metric_timeseries()],
#' [cvd_indicator_person_timeseries()], [cvd_indicator_metric_systemlevel_comparison()],
#' [cvd_indicator_metric_area_breakdown()]
#'
#' @examples
#' # Compare performance against metric 150  (AF: treatment with anticoagulants
#' # - all people) in 'Chester South PCN' (area ID 553) with national
#' # performance:
#' return_list <- cvd_indicator_nationalarea_metric_data(
#'     metric_id = 150,
#'     time_period_id = 17,
#'     area_id = 553
#' )
#'
#' # See what the list contains
#' return_list |> summary()
#'
#' # Extract the `area` details
#' area_data <- return_list$area
#' area_data |> gt::gt()
#'
#' # Extract `target` details
#' target_data <- return_list$target
#' target_data |> gt::gt()
cvd_indicator_nationalarea_metric_data <- function(
  metric_id = 1,
  time_period_id = 17,
  area_id = 739
) {
  # validate input
  validate_input_id(
    id = metric_id,
    param_name = "metric_id",
    required = TRUE
  )
  validate_input_id(
    id = time_period_id,
    param_name = "time_period_id",
    required = TRUE,
    valid_ids = m_get_valid_time_period_ids()
  )
  validate_input_id(
    id = area_id,
    param_name = "area_id",
    required = TRUE
  )

  # compose the request
  req <-
    httr2::request(url_base) |>
    httr2::req_url_path_append(glue::glue(
      'indicator/nationalVsAreaMetricData/{metric_id}'
    )) |>
    httr2::req_url_query(
      `timePeriodID` = time_period_id,
      `areaID` = area_id
    )

  # catch errors caused by bad url - because indicator id is invalid
  tryCatch(
    {
      # perform the request
      resp <- req |>
        httr2::req_perform() |>
        httr2::resp_body_string()

      # wrangle for output
      data <- jsonlite::fromJSON(resp, flatten = T)[[2]] |>
        purrr::compact()

      if (length(data) == 0) {
        cli::cli_alert_danger('No metric details returned')
        return(dplyr::tibble(result = 'No metric details returned'))
      } else {
        # compose the return list
        return <- list()

        # handle area data
        if ('AreaData' %in% names(data)) {
          # extract area data - the first element
          area_data <-
            data |>
            dplyr::nth(1) |>
            tibble::as_tibble()

          # add to return list
          return <-
            return |>
            append(list('area' = area_data))
        }

        # handle target data
        if ('TargetData' %in% names(data)) {
          # extract target data - the second element
          target_data <-
            data |>
            dplyr::nth(2)

          # only output if not NULL
          if (target_data |> dplyr::nth(1) |> is.null()) {
            # do nothing as the first element is null
          } else {
            # convert to tibble
            target_data <-
              target_data |>
              tibble::as_tibble()

            # add to return list
            return <-
              return |>
              append(list('target' = target_data))
          }
        }

        # return the result
        return(return)
      }
    },
    httr2_error = function(e) {
      internal_try_catch_html500(error = e, msg = 'Metric ID is invalid')
    }
  )
}

#' Indicator priority groups
#'
#' Returns the list of top-level groupings (Priority Groups) displayed in the
#' Regional & ICS Insights page. Returns a dictionary called 'PriorityGroups'
#' with each key being a Priority Group name, and each value being the array of
#' indicators contained in that group. The 'PriorityGroupDisplayOrder'
#' indicates the order in which it should be displayed for the given Priority
#' Group.
#'
#' CVD Prevent API documentation:
#' [Indicator priority groups](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Findicator%2FpriorityGroups)
#'
#' @return Tibble of indicators grouped by priority group
#' @export
#' @seealso [cvd_indicator_list()], [cvd_indicator_metric_list()], [cvd_indicator()],
#' [cvd_indicator_tags()], [cvd_indicator_details()], [cvd_indicator_sibling()],
#' [cvd_indicator_child_data()], [cvd_indicator_data()], [cvd_indicator_metric_data()],
#' [cvd_indicator_raw_data()], [cvd_indicator_nationalarea_metric_data()],
#' [cvd_indicator_pathway_group()],
#' [cvd_indicator_group()], [cvd_indicator_metric_timeseries()],
#' [cvd_indicator_person_timeseries()], [cvd_indicator_metric_systemlevel_comparison()],
#' [cvd_indicator_metric_area_breakdown()]
#'
#' @examples
#' # Return one indicator from each of the priority groups:
#' cvd_indicator_priority_groups() |>
#'   dplyr::select(PriorityGroup, PathwayGroupName, PathwayGroupID,
#'   IndicatorCode, IndicatorID, IndicatorName) |>
#'   dplyr::slice_head(by = PathwayGroupID)
cvd_indicator_priority_groups <- function() {
  # compose the request
  req <-
    httr2::request(url_base) |>
    httr2::req_url_path_append('indicator/priorityGroups')

  # perform the request
  resp <- req |>
    httr2::req_perform() |>
    httr2::resp_body_string()

  # wrangle for output
  data <- jsonlite::fromJSON(resp, flatten = T)[[1]]

  if (length(data) == 0) {
    cli::cli_alert_danger('No indicator priority groups returned')
    return(dplyr::tibble(result = 'No indicator priority groups returned'))
  } else {
    data <- data |>
      dplyr::tibble() |>
      dplyr::mutate(PriorityGroup = names(data)) |>
      purrr::compact() |>
      tidyr::unnest(cols = data)

    return(data)
  }
}


#' Pathway groups
#'
#' Pathway groups are sub-groupings of Priority Groups, visible in the Regional
#' & ICS Insights page. This endpoint returns a single pathway group for a given
#' group ID. An error will be returned if there is no pathway group associated
#' with the given ID. For a valid request, Pathway Group ID and named are
#' returned as key value pairs and the Indicators populate an array.
#'
#' CVD Prevent API documentation:
#' [Indicator pathway group](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Findicator%2FpathwayGroup%2F%3Cpathway_group_id%3E)
#'
#'
#' @param pathway_group_id integer - the pathway to return data for (compulsory)
#'
#' @return Tibble of indicators grouped by pathway groups
#' @export
#' @seealso [cvd_indicator_list()], [cvd_indicator_metric_list()], [cvd_indicator()],
#' [cvd_indicator_tags()], [cvd_indicator_details()], [cvd_indicator_sibling()],
#' [cvd_indicator_child_data()], [cvd_indicator_data()], [cvd_indicator_metric_data()],
#' [cvd_indicator_raw_data()], [cvd_indicator_nationalarea_metric_data()],
#' [cvd_indicator_priority_groups()],
#' [cvd_indicator_group()], [cvd_indicator_metric_timeseries()],
#' [cvd_indicator_person_timeseries()], [cvd_indicator_metric_systemlevel_comparison()],
#' [cvd_indicator_metric_area_breakdown()]
#'
#' @examples
#' # Return indicators for the 'Chronic Kidney Disease' Pathway Group (ID 9):
#' cvd_indicator_pathway_group(pathway_group_id = 9) |>
#'   dplyr::select(PathwayGroupName, PathwayGroupID, IndicatorCode, IndicatorID, IndicatorName)
cvd_indicator_pathway_group <- function(pathway_group_id) {
  # validate input
  validate_input_id(
    id = pathway_group_id,
    param_name = "pathway_group_id",
    required = TRUE
  )

  # compose the request
  req <-
    httr2::request(url_base) |>
    httr2::req_url_path_append(glue::glue(
      'indicator/pathwayGroup/{pathway_group_id}'
    ))

  # catch errors caused by bad url - because pathway group id is invalid
  tryCatch(
    {
      # perform the request
      # extract the response as string (this bit is most likely to fail)
      resp <- req |>
        httr2::req_perform() |>
        httr2::resp_body_string()

      # wrangle for output
      data <- jsonlite::fromJSON(resp, flatten = T)[[1]]

      if (length(data) == 0) {
        cli::cli_alert_danger('No indicator pathway groups returned')
        return(dplyr::tibble(result = 'No indicator pathway groups returned'))
      } else {
        data <- data |>
          purrr::compact() |>
          dplyr::as_tibble() |>
          dplyr::relocate(Indicators, .after = dplyr::last_col()) |>
          tidyr::unnest(cols = Indicators)

        return(data)
      }
    },
    httr2_error = function(e) {
      internal_try_catch_html500(error = e, msg = 'Pathway Group ID is invalid')
    }
  )
}

#' Indicator group
#'
#' Returns a single indicator group for a given group ID. An error will be
#' returned if there is no indicator group associated with the given ID.
#' `IndicatorGroup` is the primary key in the IndicatorGroup table, which also
#' contains `IndicatorGroupName` and `IndicatorGroupTypeID`. The group type ID
#' tells you what type of indicator group you're dealing with, e.g. a Priority
#' Group.
#' `IndicatorGroupTypeID` is the primary key of IndicatorGroupType and so
#' `IndicatorGroupTypeName` is the associated name for the given group type ID.
#' Finally, there is the array of indicators which are contained in this group,
#' including display orders for the given group.
#'
#' CVD Prevent API documentation:
#' [Indicator group](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Findicator%2FindicatorGroup%2F%3Cindicator_group_ID%3E)
#'
#'
#' @param indicator_group_id integer - the group to return data for (compulsory)
#'
#' @return Tibble of indicators grouped by indicator group
#' @export
#' @seealso [cvd_indicator_list()], [cvd_indicator_metric_list()], [cvd_indicator()],
#' [cvd_indicator_tags()], [cvd_indicator_details()], [cvd_indicator_sibling()],
#' [cvd_indicator_child_data()], [cvd_indicator_data()], [cvd_indicator_metric_data()],
#' [cvd_indicator_raw_data()], [cvd_indicator_nationalarea_metric_data()],
#' [cvd_indicator_priority_groups()], [cvd_indicator_pathway_group()], #
#' [cvd_indicator_metric_timeseries()],
#' [cvd_indicator_person_timeseries()], [cvd_indicator_metric_systemlevel_comparison()],
#' [cvd_indicator_metric_area_breakdown()]
#'
#' @examples
#' #  list the indicators under Indicator Group ID 13 (Monitoring) which lists
#' # 'Key Question' Indicator Group indicators:
#' cvd_indicator_group(indicator_group_id = 13) |>
#'   dplyr::select(IndicatorGroupID, IndicatorGroupName, IndicatorGroupTypeName,
#'   IndicatorID, IndicatorName)
cvd_indicator_group <- function(indicator_group_id) {
  # validate input
  validate_input_id(
    id = indicator_group_id,
    param_name = "indicator_group_id",
    required = TRUE
  )

  # compose the request
  req <-
    httr2::request(url_base) |>
    httr2::req_url_path_append(glue::glue(
      'indicator/indicatorGroup/{indicator_group_id}'
    ))

  # catch errors caused by bad url - because pathway group id is invalid
  tryCatch(
    {
      # perform the request
      # extract the response as string (this bit is most likely to fail)
      resp <- req |>
        httr2::req_perform() |>
        httr2::resp_body_string()

      # wrangle for output
      data <- jsonlite::fromJSON(resp, flatten = T)[[1]]
      data <- data |>
        purrr::compact() |>
        dplyr::as_tibble() |>
        dplyr::select(
          -dplyr::any_of(c(
            'NotificationCount',
            'HighestPriorityNotificationType'
          ))
        ) |> # prevents conflicts with column of same name in nested data
        dplyr::relocate(Indicators, .after = dplyr::last_col()) |>
        tidyr::unnest(cols = Indicators)

      return(data)
    },
    httr2_error = function(e) {
      internal_try_catch_html500(
        error = e,
        msg = 'Indicator Group ID is invalid'
      )
    }
  )
}

#' Indicator time series by metric
#'
#' Returns data for the time series chart for specified metric ID and area ID.
#' Contains an array of two areas in `Areas`, one of which is the National data
#' with the other corresponding to the provided area ID. `TargetValue` is also
#' returned in the `Data` dictionary.
#'
#' CVD Prevent API documentation:
#' [Indicator time series metrics](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Findicator%2FtimeSeriesByMetric%2F%3Cmetric_ID%3E)
#'
#' @param metric_id integer - the metric to return data for (compulsory)
#' @param area_id integer - the area to return data for (compulsory)
#'
#' @return Tibble of time-series data for the specified metric in the area
#' @export
#' @seealso [cvd_indicator_list()], [cvd_indicator_metric_list()], [cvd_indicator()],
#' [cvd_indicator_tags()], [cvd_indicator_details()], [cvd_indicator_sibling()],
#' [cvd_indicator_child_data()], [cvd_indicator_data()], [cvd_indicator_metric_data()],
#' [cvd_indicator_raw_data()], [cvd_indicator_nationalarea_metric_data()],
#' [cvd_indicator_priority_groups()], [cvd_indicator_pathway_group()], #
#' [cvd_indicator_group()],
#' [cvd_indicator_person_timeseries()], [cvd_indicator_metric_systemlevel_comparison()],
#' [cvd_indicator_metric_area_breakdown()]
#'
#' @examples
#' # List data for Salford South East PCN (area ID 705) for 'AF: treatment with
#' # anticoagulants' for women people aged 60-79 years (metric ID 130):
#' cvd_indicator_metric_timeseries(metric_id = 130, area_id = 705) |>
#'   dplyr::select(AreaName, TimePeriodName, TimePeriodID, Value) |>
#'   tidyr::pivot_wider(
#'     names_from = AreaName,
#'     values_from = Value
#'   )
cvd_indicator_metric_timeseries <- function(metric_id, area_id) {
  # validate input
  validate_input_id(
    id = metric_id,
    param_name = "metric_id",
    required = TRUE
  )
  validate_input_id(
    id = area_id,
    param_name = "area_id",
    required = TRUE
  )

  # compose the request
  req <-
    httr2::request(url_base) |>
    httr2::req_url_path_append(glue::glue(
      'indicator/timeSeriesByMetric/{metric_id}'
    )) |>
    httr2::req_url_query(
      `areaID` = area_id
    )

  # catch errors caused by bad url - because pathway group id is invalid
  tryCatch(
    {
      # perform the request
      resp <- req |>
        httr2::req_perform() |>
        httr2::resp_body_string()

      # wrangle for output
      data <- jsonlite::fromJSON(resp, flatten = T)[[1]]
      data <- data |>
        purrr::compact() |>
        dplyr::as_tibble() |>
        tidyr::unnest(cols = Areas) |>
        tidyr::unnest(cols = TimeSeriesData)

      return(data)
    },
    httr2_error = function(e) {
      internal_try_catch_html500(error = e, msg = 'Metric ID is invalid')
    }
  )
}

#' Indicator persons time series by indicator
#'
#' Returns data for the Inequalities Markers Time Series chart for the provided
#' indicator ID and area ID. `Data` contains information about the chosen
#' target value as well as an array `InequalityMarkers` which contains all the
#' time series data grouped into metric category types e.g. age group,
#' ethnicity, etc.
#'
#' CVD Prevent API documentation:
#' [Indicator person time series](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Findicator%2FpersonsTimeSeriesByIndicator%2F%3Cindicator_ID%3E)
#'
#' @param indicator_id integer - the indicator to return data for (compulsory)
#' @param area_id integer - the area to return data for (compulsory)
#'
#' @return Tibble of metric performance for the specified indicator in the area
#' @export
#' @seealso [cvd_indicator_list()], [cvd_indicator_metric_list()], [cvd_indicator()],
#' [cvd_indicator_tags()], [cvd_indicator_details()], [cvd_indicator_sibling()],
#' [cvd_indicator_child_data()], [cvd_indicator_data()], [cvd_indicator_metric_data()],
#' [cvd_indicator_raw_data()], [cvd_indicator_nationalarea_metric_data()],
#' [cvd_indicator_priority_groups()], [cvd_indicator_pathway_group()], #
#' [cvd_indicator_group()], [cvd_indicator_metric_timeseries()],
#' [cvd_indicator_metric_systemlevel_comparison()],
#' [cvd_indicator_metric_area_breakdown()]
#'
#' @examples
#' # View the details of the time-series performance for indicator 'AF:
#' # treatment with anticoagulants' (ID 7) in Salford South East PCN (area ID
#' # 705), focussed just on the age group inequalities metrics:
#' cvd_indicator_person_timeseries(indicator_id = 7, area_id = 705) |>
#'   dplyr::filter(
#'     MetricCategoryTypeName == 'Age group',
#'     !is.na(Value)
#'   ) |>
#'   dplyr::select(MetricCategoryName, TimePeriodName, TimePeriodID, Value) |>
#'   tidyr::pivot_wider(
#'     names_from = MetricCategoryName,
#'     values_from = Value
#'   )
cvd_indicator_person_timeseries <- function(indicator_id, area_id) {
  # validate input
  validate_input_id(
    id = indicator_id,
    param_name = "indicator_id",
    required = TRUE
  )
  validate_input_id(
    id = area_id,
    param_name = "area_id",
    required = TRUE
  )

  # compose the request
  req <-
    httr2::request(url_base) |>
    httr2::req_url_path_append(glue::glue(
      'indicator/personsTimeSeriesByIndicator/{indicator_id}'
    )) |>
    httr2::req_url_query(
      `areaID` = area_id
    )

  # catch errors caused by bad url - because pathway group id is invalid
  tryCatch(
    {
      # perform the request
      resp <- req |>
        httr2::req_perform() |>
        httr2::resp_body_string()

      # wrangle for output
      data <- jsonlite::fromJSON(resp, flatten = T)[[1]]
      data <- data |>
        purrr::compact() |>
        dplyr::as_tibble() |>
        tidyr::unnest(cols = InequalityMarkers) |>
        tidyr::unnest(cols = CategoryData)

      return(data)
    },
    httr2_error = function(e) {
      internal_try_catch_html500(
        error = e,
        msg = 'Either Metric ID or Area ID is invalid'
      )
    }
  )
}

#' Indicator metric system level comparison
#'
#' Returns data for the SystemLevel Comparison chart for provided metric, area
#' and time period. `Data` contains the target value as well as an array
#' `SystemLevels` which contains data grouped by system level.
#'
#' CVD Prevent API documentation:
#' [Indicator metric system level comparison](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Findicator%2FmetricSystemLevelComparison%2F%3Cmetric_ID%3E)
#'
#'
#' @param metric_id integer - the metric to return data for (compulsory)
#' @param time_period_id integer - the time period to return data for (compulsory)
#' @param area_id integer - the area to return data for (compulsory)
#'
#' @return Tibble of metric performance for the specified area and all other areas in the same system level in the time period
#' @export
#' @seealso [cvd_indicator_list()], [cvd_indicator_metric_list()], [cvd_indicator()],
#' [cvd_indicator_tags()], [cvd_indicator_details()], [cvd_indicator_sibling()],
#' [cvd_indicator_child_data()], [cvd_indicator_data()], [cvd_indicator_metric_data()],
#' [cvd_indicator_raw_data()], [cvd_indicator_nationalarea_metric_data()],
#' [cvd_indicator_priority_groups()], [cvd_indicator_pathway_group()], #
#' [cvd_indicator_group()], [cvd_indicator_metric_timeseries()],
#' [cvd_indicator_person_timeseries()],
#' [cvd_indicator_metric_area_breakdown()]
#'
#' @examples
#' # return performance for metric 'AF: DOAC & VitK' in people aged 40-59 years
#' # (metric ID 1270) in time period 17 for Salford South East PCN (area ID 705)
#' # and all other PCNs - truncated to a sample of four PCN performances:
#' cvd_indicator_metric_systemlevel_comparison(metric_id = 1270,
#' time_period_id = 17, area_id = 705) |>
#'   dplyr::filter(AreaID %in% c(705:709), !is.na(Value)) |>
#'   dplyr::select(SystemLevelName, AreaID, AreaName, Value)
cvd_indicator_metric_systemlevel_comparison <- function(
  metric_id,
  time_period_id,
  area_id
) {
  # validate input
  validate_input_id(
    id = metric_id,
    param_name = "metric_id",
    required = TRUE
  )
  validate_input_id(
    id = time_period_id,
    param_name = "time_period_id",
    required = TRUE,
    valid_ids = m_get_valid_time_period_ids()
  )
  validate_input_id(
    id = area_id,
    param_name = "area_id",
    required = TRUE
  )

  # compose the request
  req <-
    httr2::request(url_base) |>
    httr2::req_url_path_append(glue::glue(
      'indicator/metricSystemLevelComparison/{metric_id}'
    )) |>
    httr2::req_url_query(
      `timePeriodID` = time_period_id,
      `areaID` = area_id
    )

  # catch errors caused by bad url - because pathway group id is invalid
  tryCatch(
    {
      # perform the request
      resp <- req |>
        httr2::req_perform() |>
        httr2::resp_body_string()

      # wrangle for output
      data <- jsonlite::fromJSON(resp, flatten = T)[[1]]
      data <- data |>
        purrr::compact() |>
        dplyr::as_tibble() |>
        tidyr::unnest(cols = SystemLevels) |>
        dplyr::relocate(ComparisonData, .after = dplyr::last_col()) |>
        tidyr::unnest(cols = ComparisonData)

      return(data)
    },
    httr2_error = function(e) {
      internal_try_catch_html500(
        error = e,
        msg = 'Either Metric ID, Area ID or Time Period ID are invalid'
      )
    }
  )
}

#' Indicator metric area breakdown
#'
#' Returns data for the Area Breakdown chart for provided metric, area and time
#' period. `Data` contains the target value as well as an array `SystemLevels`
#' which contains data grouped by system level.
#'
#' CVD Prevent API documentation:
#' [Indicator metric area breakdown](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Findicator%2FmetricAreaBreakdown%2F%3Cmetric_ID%3E)
#'
#' @param metric_id integer - the metric to return data for (compulsory)
#' @param time_period_id integer - the time period to return data for (compulsory)
#' @param area_id integer - the area to return data for (compulsory)
#'
#' @return Tibble of metric performance for the specified area compared with National level
#' @export
#' @seealso [cvd_indicator_list()], [cvd_indicator_metric_list()], [cvd_indicator()],
#' [cvd_indicator_tags()], [cvd_indicator_details()], [cvd_indicator_sibling()],
#' [cvd_indicator_child_data()], [cvd_indicator_data()], [cvd_indicator_metric_data()],
#' [cvd_indicator_raw_data()], [cvd_indicator_nationalarea_metric_data()],
#' [cvd_indicator_priority_groups()], [cvd_indicator_pathway_group()], #
#' [cvd_indicator_group()], [cvd_indicator_metric_timeseries()],
#' [cvd_indicator_person_timeseries()], [cvd_indicator_metric_systemlevel_comparison()]
#'
#' @examples
#' # Return performance for metric 'AF: DOAC & VitK' in men aged 60-79 years
#' # (metric ID 128) in time period 17 for Salford South East PCN (area ID 705):
#' cvd_indicator_metric_area_breakdown(metric_id = 128, time_period_id = 17,
#'   area_id = 705) |>
#'   dplyr::select(SystemLevelName, AreaID, AreaName, Value)
cvd_indicator_metric_area_breakdown <- function(
  metric_id,
  time_period_id,
  area_id
) {
  # validate input
  validate_input_id(
    id = metric_id,
    param_name = "metric_id",
    required = TRUE
  )
  validate_input_id(
    id = time_period_id,
    param_name = "time_period_id",
    required = TRUE,
    valid_ids = m_get_valid_time_period_ids()
  )
  validate_input_id(
    id = area_id,
    param_name = "param_id",
    required = TRUE
  )

  # compose the request
  req <-
    httr2::request(url_base) |>
    httr2::req_url_path_append(glue::glue(
      'indicator/metricAreaBreakdown/{metric_id}'
    )) |>
    httr2::req_url_query(
      `timePeriodID` = time_period_id,
      `areaID` = area_id
    )

  # catch errors caused by bad url - because pathway group id is invalid
  tryCatch(
    {
      # perform the request
      resp <- req |>
        httr2::req_perform() |>
        httr2::resp_body_string()

      # wrangle for output
      data <- jsonlite::fromJSON(resp, flatten = T)[[1]]
      data <- data |>
        purrr::compact() |>
        dplyr::as_tibble() |>
        tidyr::unnest(cols = SystemLevels) |>
        dplyr::relocate(ComparisonData, .after = dplyr::last_col()) |>
        tidyr::unnest(cols = ComparisonData)

      return(data)
    },
    httr2_error = function(e) {
      internal_try_catch_html500(
        error = e,
        msg = 'Either Metric ID or Time Period ID are invalid'
      )
    }
  )
}

## external resource -----------------------------------------------------------
#' External resource
#'
#' Returns a list of all external resources
#'
#' CVD Prevent API documentation:
#' [External resources](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2FexternalResource)
#'
#' @return Tibble of details for external resources
#' @export
#' @seealso [cvd_data_availability()]
#'
#' @examples
#' # Here we show the first five external resources:
#' cvd_external_resource() |>
#'   dplyr::filter(ExternalResourceID < 10) |>
#'   dplyr::select(ExternalResourceCategory, ExternalResourceSource, ExternalResourceTitle) |>
#'   dplyr::group_by(ExternalResourceCategory)
cvd_external_resource <- function() {
  # compose the request
  req <-
    httr2::request(url_base) |>
    httr2::req_url_path_append('externalResource')

  # perform the request
  resp <- req |>
    httr2::req_perform() |>
    httr2::resp_body_string()

  # wrangle for output
  data <- jsonlite::fromJSON(resp, flatten = T)[[2]] |>
    purrr::compact() |>
    dplyr::as_tibble()
}

#' Data availability
#'
#' Returns the data availability.
#' Response:
#' `DataAvailabilityID` - ID of the resource as found in the database
#' `DataAvailabilityName` - explanation for the data availability
#' `IsAvailable` - `Y` for data is available, `N` for data is unavailable, and NULL for unknown data
#'
#' CVD Prevent API documentation:
#' [Data availability](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2FdataAvailability)
#'
#' @param time_period_id integer - the time period to return data for (compulsory)
#' @param system_level_id integer - the system level to return data for (compulsory)
#' @param indicator_id integer - the indicator to return data for (optional)
#' @param metric_category_type_id integer - the metric category to return data for (optional)
#'
#' @return Tibble of data availability
#' @export
#' @seealso [cvd_external_resource()]
#'
#' @examples
#' cvd_data_availability(time_period_id = 3, system_level_id = 5)
cvd_data_availability <- function(
  time_period_id,
  system_level_id,
  indicator_id, # optional
  metric_category_type_id # optional
) {
  # validate input
  validate_input_id(
    id = time_period_id,
    param_name = "time_period_id",
    required = TRUE,
    valid_ids = m_get_valid_time_period_ids()
  )
  validate_input_id(
    id = system_level_id,
    param_name = "system_level_id",
    required = TRUE,
    valid_ids = m_get_valid_system_level_id_for_time_period_id(
      time_period_id = time_period_id
    )
  )
  validate_input_id(
    id = indicator_id,
    param_name = "indicator_id",
    required = FALSE
  )
  validate_input_id(
    id = metric_category_type_id,
    param_name = "metric_category_type_id",
    required = FALSE
  )

  # compose the request
  req <-
    httr2::request(url_base) |>
    httr2::req_url_path_append('dataAvailability') |>
    httr2::req_url_query(
      `timePeriodID` = time_period_id,
      `systemLevelID` = system_level_id
    )

  if (!base::missing(indicator_id) & !base::missing(metric_category_type_id)) {
    req <-
      req |>
      httr2::req_url_query(
        `indicatorID` = indicator_id,
        `metricCategoryTypeID` = metric_category_type_id
      )
  } else if (!base::missing(indicator_id)) {
    req <-
      req |>
      httr2::req_url_query(
        `indicatorID` = indicator_id
      )
  } else if (!base::missing(metric_category_type_id)) {
    req <-
      req |>
      httr2::req_url_query(
        `metricCategoryTypeID` = metric_category_type_id
      )
  }

  # catch errors caused by bad url - because pathway group id is invalid
  tryCatch(
    {
      # perform the request
      resp <- req |>
        httr2::req_perform() |>
        httr2::resp_body_string()

      # wrangle for output
      data <- jsonlite::fromJSON(resp, flatten = T)[[1]]
      data <- data |>
        purrr::compact() |>
        dplyr::as_tibble()

      return(data)
    },
    httr2_error = function(e) {
      internal_try_catch_html500(
        error = e,
        msg = 'HTTPS error - please check either Time Period ID, Indicator ID (if supplied) and Metric Category Type ID (if supplied)'
      )
    }
  )
}
