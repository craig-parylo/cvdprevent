# setup ------------------------------------------------------------------------
#' Get the base URL for the CVD Prevent API
#' @return A character string with the base URL
#' @noRd
get_api_base_url <- function() {
  "https://api.cvdprevent.nhs.uk"
}

# keep for testing - remove before committing
# url_base <- "https://api.cvdprevent.nhs.uk"

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
cvd_time_period_list <- function(indicator_type_id = NULL) {
  # validate input
  v1 <- validate_input_id(
    id = indicator_type_id,
    param_name = "indicator_type_id",
    required = FALSE
  )
  if (!isTRUE(v1)) {
    return(v1)
  }

  # build request
  req <- httr2::request(get_api_base_url()) |>
    httr2::req_url_path_append("timePeriod")

  if (!is.null(indicator_type_id)) {
    req <- req |>
      httr2::req_url_query(indicatorTypeID = indicator_type_id)
  }

  # processor: extract and tidy the list
  process_time_periods <- function(parsed) {
    if (!"timePeriodList" %in% names(parsed)) {
      return(
        cvd_error_tibble(
          context = "cvd_time_period_list",
          error = "Response does not contain `timePeriodList`.",
          status = NA_integer_,
          url = httr2::req_get_url(req),
          params = NA_character_,
          resp = NA_character_
        )
      )
    }
    parsed$timePeriodList |>
      tibble::as_tibble()
  }

  # perform request safely with cache
  res <- memoised_safe_api_call(
    req = req,
    process_fn = process_time_periods,
    context = "cvd_time_period_list"
  )

  if (res$success) {
    res$result
  } else {
    res$tibble
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
#'   dplyr::slice_max(order_by = TimePeriodID) |>
#'   dplyr::select(TimePeriodID, TimePeriodName, SystemLevelID, SystemLevelName)
#'
#' @export
cvd_time_period_system_levels <- function() {
  # build request
  req <- httr2::request(get_api_base_url()) |>
    httr2::req_url_path_append("timePeriod/systemLevels")

  # processor: extract and tidy the nested SystemLevels
  process_system_levels <- function(parsed) {
    # defensive check
    if (length(parsed) < 2 || !is.list(parsed[[2]])) {
      return(
        cvd_error_tibble(
          context = "cvd_time_period_system_levels",
          error = "Response does not contain expected `SystemLevels` structure.",
          status = NA_integer_,
          url = httr2::req_get_url(req),
          params = NA_character_,
          resp = NA_character_
        )
      )
    }

    # continue with processing
    parsed[[2]] |>
      tibble::as_tibble() |>
      dplyr::select(
        -dplyr::any_of(c(
          "NotificationCount",
          "HighestPriorityNotificationType"
        ))
      ) |> # drop potentially conflicting columns
      dplyr::relocate(
        dplyr::any_of("SystemLevels"),
        .after = dplyr::last_col()
      ) |>
      tidyr::unnest(cols = dplyr::any_of("SystemLevels")) |>
      dplyr::arrange(
        dplyr::pick(dplyr::any_of(c("TimePeriodID", "SystemLevelID")))
      )
  }

  # safely perform the request
  res <- memoised_safe_api_call(
    req = req,
    process_fn = process_system_levels,
    context = "cvd_time_period_system_levels"
  )

  # if successful return the result, otherwise the error tibble
  if (res$success) {
    res$result
  } else {
    res$tibble
  }
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
  v <- validate_input_id(
    id = time_period_id,
    param_name = "time_period_id",
    required = TRUE,
    valid_ids = m_get_valid_time_period_ids()
  )
  if (!identical(v, TRUE)) {
    return(v)
  } # propagate error tibble

  # compose the request
  req <-
    httr2::request(get_api_base_url()) |>
    httr2::req_url_path_append('area/systemLevel') |>
    httr2::req_url_query(
      `timePeriodID` = time_period_id
    )

  # define a processor that works on parsed JSON
  process_area_system_level <- function(parsed) {
    # defensive check
    if (length(parsed) < 2 || !is.list(parsed[[2]])) {
      return(
        cvd_error_tibble(
          context = "cvd_area_system_level",
          error = "Response does not contain expected structure.",
          status = NA_integer_,
          url = httr2::req_get_url(req),
          params = NA_character_,
          resp = NA_character_
        )
      )
    }

    # continue with processing
    parsed[[2]] |> tibble::as_tibble()
  }

  # safely perform the request
  res <- memoised_safe_api_call(
    req = req,
    process_fn = process_area_system_level,
    context = "cvd_area_system_level"
  )

  # reutrn either processed tibble or error tibble
  if (isTRUE(res$success)) {
    return(res$result)
  } else {
    return(res$tibble)
  }
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
    httr2::request(get_api_base_url()) |>
    httr2::req_url_path_append('area/systemLevel/timePeriods')

  # define a function to process the data
  process_area_system_level_time_periods <- function(parsed) {
    # defensive check
    if (length(parsed) < 2 || !is.list(parsed[[2]])) {
      return(
        cvd_error_tibble(
          context = "cvd_area_system_level_time_periods",
          error = "Response does not contain expected `TimePeriods` structure.",
          status = NA_integer_,
          url = httr2::req_get_url(req),
          params = NA_character_,
          resp = NA_character_
        )
      )
    }

    # continue with processing
    parsed[[2]] |>
      tibble::as_tibble() |>
      tidyr::unnest(cols = dplyr::any_of(c("TimePeriods")))
  }

  # safely perform the request and memoise
  res <-
    memoised_safe_api_call(
      req = req,
      process_fn = process_area_system_level_time_periods,
      context = "cvd_area_system_level_time_periods"
    )

  # if successful return the result, otherwise the error tibble
  if (res$success) {
    res$result
  } else {
    res$tibble
  }
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
cvd_area_list <- function(
  time_period_id,
  parent_area_id = NULL,
  system_level_id = NULL
) {
  # validate input
  v1 <- validate_input_id(
    id = time_period_id,
    param_name = "time_period_id",
    required = TRUE,
    valid_ids = m_get_valid_time_period_ids(),
    suppress_cli = TRUE
  )
  if (!isTRUE(v1)) {
    return(v1)
  }

  v2 <- validate_input_id(
    id = system_level_id,
    param_name = "system_level_id",
    required = FALSE,
    valid_ids = m_get_valid_system_level_id_for_time_period_id(
      time_period_id = time_period_id
    ),
    suppress_cli = TRUE
  )
  if (!isTRUE(v2)) {
    return(v2)
  }

  v3 <- validate_input_id(
    id = parent_area_id,
    param_name = "parent_area_id",
    required = FALSE,
    # NB, I think a child area could have activity without the parent area - so no limit here
    # valid_ids = m_get_valid_area_ids_for_time_period_id(
    #   time_period_id = time_period_id
    # )
  )
  if (!isTRUE(v3)) {
    return(v3)
  }

  # compose the request
  req <-
    httr2::request(get_api_base_url()) |>
    httr2::req_url_path_append('area')

  if (base::missing(parent_area_id) && base::missing(system_level_id)) {
    # both optional arguments are missing - but we need at least one
    return(cvd_error_tibble(
      context = "cvd_area_list",
      error = "At least one of `parent_area_id` or `system_level_id` must be provided.",
      params = list(
        parent_area_id = parent_area_id,
        system_level_id = system_level_id
      )
    ))
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

  # processor function
  process_area_list <- function(parsed) {
    # defensive check
    if (!"areaList" %in% names(parsed) || length(parsed[["areaList"]]) < 2) {
      return(
        cvd_error_tibble(
          context = "cvd_area_list",
          error = "Response does not contain expected `areaList` structure",
          url = httr2::req_get_url(req)
        )
      )
    }

    # continue with processing
    parsed$areaList |>
      tibble::as_tibble() |>
      dplyr::relocate(
        dplyr::any_of(c("Parents")),
        .after = dplyr::last_col()
      ) |>
      tidyr::unnest(col = dplyr::any_of(c("Parents")))
  }

  # safely perform the request and memoise
  res <- memoised_safe_api_call(
    req = req,
    process_fn = process_area_list,
    context = "cvd_area_list"
  )

  # if successful return the result, otherwise the error tibble
  if (res$success) {
    res$result
  } else {
    res$tibble
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
#' # Retrieve details for 'Leicester, Leicestershire and Rutland ICB' (area_id = 8037)
#' # in time period 17
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
  v1 <- validate_input_id(
    id = time_period_id,
    param_name = "time_period_id",
    required = TRUE,
    valid_ids = m_get_valid_time_period_ids()
  )
  if (!isTRUE(v1)) {
    return(v1)
  }

  v2 <- validate_input_id(
    id = area_id,
    param_name = "area_id",
    required = TRUE,
    valid_ids = m_get_valid_area_ids_for_time_period_id(
      time_period_id = time_period_id
    )
  )
  if (!isTRUE(v2)) {
    return(v2)
  }

  # compose the request
  req <-
    httr2::request(get_api_base_url()) |>
    httr2::req_url_path_append(glue::glue('area/{area_id}/details')) |>
    httr2::req_url_query(
      `timePeriodID` = time_period_id
    )

  # processor function
  process_area_details <- function(parsed) {
    # defensive check
    if (
      !"areaDetails" %in% names(parsed) || length(parsed[["areaDetails"]]) < 2
    ) {
      return(
        cvd_error_tibble(
          context = "cvd_area_details",
          error = "Response does not contain expected `areaDetails` structure",
          url = httr2::req_get_url(req)
        )
      )
    }

    # continue with processing
    dat <-
      parsed$areaDetails |>
      purrr::compact() |>
      tibble::as_tibble()

    # prepare the return object
    return <- list()

    # select base fields, exclude any parent or child details
    area_details <-
      dat |>
      dplyr::select(-dplyr::any_of(c("ChildAreaList", "ParentAreaList"))) |>
      unique()

    return <-
      return |>
      append(list("area_details" = area_details))

    # extract any parent details
    if ('ParentAreaList' %in% names(dat)) {
      area_parent_details <- dat$ParentAreaList |>
        purrr::compact() |>
        dplyr::as_tibble() |>
        unique()

      return <-
        return |>
        append(list("area_parent_details" = area_parent_details))
    }

    # extract any child details
    if ('ChildAreaList' %in% names(dat)) {
      area_child_details <- dat$ChildAreaList |>
        purrr::compact() |>
        dplyr::as_tibble() |>
        unique()

      return <-
        return |>
        append(list("area_child_details" = area_child_details))
    }
    return(return)
  }

  # safely perform the request and memoise
  res <- memoised_safe_api_call(
    req = req,
    process_fn = process_area_details,
    context = "cvd_area_details"
  )

  # if successful return the result, otherwise the error tibble
  if (res$success) {
    res$result
  } else {
    res$tibble
  }
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
cvd_area_unassigned <- function(time_period_id, system_level_id = NULL) {
  # validate input
  v1 <- validate_input_id(
    id = time_period_id,
    param_name = "time_period_id",
    required = TRUE,
    valid_ids = m_get_valid_time_period_ids()
  )
  if (!isTRUE(v1)) {
    return(v1)
  }

  v2 <- validate_input_id(
    id = system_level_id,
    param_name = "system_level_id",
    required = FALSE,
    valid_ids = m_get_valid_system_level_id_for_time_period_id(
      time_period_id = time_period_id
    )
  )
  if (!isTRUE(v2)) {
    return(v2)
  }

  # compose the request
  req <-
    httr2::request(get_api_base_url()) |>
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

  # process function
  process_area_unassigned <- function(parsed) {
    # defensive check
    if (
      !"unassignedAreaList" %in% names(parsed) ||
        length(parsed[["unassignedAreaList"]]) < 2
    ) {
      return(
        cvd_error_tibble(
          context = "cvd_area_unassigned",
          error = "Response does not contain expected `unassignedAreaList` structure",
          url = httr2::req_get_url(req)
        )
      )
    }

    # continue with processing
    parsed$unassignedAreaList |>
      tibble::as_tibble()
  }

  # safely perform the request and memoise
  res <- memoised_safe_api_call(
    req = req,
    process_fn = process_area_unassigned,
    context = "cvd_area_unassigned"
  )

  # if successful return the result, otherwise the error tibble
  if (res$success) {
    res$result
  } else {
    res$tibble
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
  v1 <- validate_input_string(
    value = partial_area_name,
    param_name = "partial_area_name",
    required = TRUE
  )
  if (!isTRUE(v1)) {
    return(v1)
  }

  v2 <- validate_input_id(
    id = time_period_id,
    param_name = "time_period_id",
    required = TRUE,
    valid_ids = m_get_valid_time_period_ids()
  )
  if (!isTRUE(v2)) {
    return(v2)
  }

  # compose the request
  req <-
    httr2::request(get_api_base_url()) |>
    httr2::req_url_path_append('area/search') |>
    httr2::req_url_query(
      `partialAreaName` = partial_area_name,
      `timePeriodID` = time_period_id
    )

  # processing function
  process_area_search <- function(parsed) {
    # defensive check
    if (
      !"foundAreaList" %in% names(parsed) ||
        length(parsed[["foundAreaList"]]) < 2
    ) {
      return(
        cvd_error_tibble(
          context = "cvd_area_search",
          error = "Response does not contain expected `foundAreaList` structure",
          url = httr2::req_get_url(req)
        )
      )
    }

    # continue with processing
    parsed$foundAreaList |>
      purrr::compact() |>
      tibble::as_tibble()
  }

  # safely perform the request and memoise
  res <- memoised_safe_api_call(
    req = req,
    process_fn = process_area_search,
    context = "cvd_area_search"
  )

  # if successful return the result, otherwise the error tibble
  if (res$success) {
    res$result
  } else {
    res$tibble
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
  v1 <- validate_input_id(
    id = area_id,
    param_name = "area_id",
    required = TRUE
  )
  if (!isTRUE(v1)) {
    return(v1)
  }

  # compose the request
  req <-
    httr2::request(get_api_base_url()) |>
    httr2::req_url_path_append(glue::glue('area/{area_id}/nestedSubSystems'))

  # process function
  process_area_nested_subsystems <- function(parsed) {
    # defensive check
    if (length(parsed) < 1 || !is.list(parsed[[1]])) {
      return(
        cvd_error_tibble(
          context = "cvd_area_nested_subsystems",
          error = "Response does not contain expected structure.",
          status = NA_integer_,
          url = httr2::req_get_url(req),
          params = NA_character_,
          resp = NA_character_
        )
      )
    }

    # continue with processing
    dat <- parsed[[1]] |>
      purrr::compact() |>
      tibble::as_tibble()

    # set up the return object
    return <- list()

    # `dat` is a complex, multi-layered object that contains details about the
    # requested area and also potentially its children, nested in a variable
    # named 'Children'. Each of these children could potentially have nested
    # children data too.
    # To tackle this, we will implement a loop to extract details for each
    # layer before returning all extracted layers in a named list object.

    # set up the loop
    iter <- 0
    while (length(dat) > 0) {
      # count iterations
      iter <- iter + 1
      iter_name <- glue::glue('level_{iter}')

      # ensure data is a tibble
      dat <-
        dat |>
        purrr::compact() |>
        dplyr::as_tibble()

      # extract the details for the current level excluding children
      level <- dat |>
        dplyr::select(-dplyr::any_of('Children')) |>
        dplyr::distinct()

      # get the children details
      child <- dat |>
        dplyr::select(dplyr::any_of('Children')) |>
        tidyr::unnest(cols = dplyr::any_of('Children'))

      # add current level details to the return object if it contains data
      if (length(dat) > 0) {
        return <-
          return |>
          append(setNames(list(level), iter_name))
      }

      # set data to be children (for the loop)
      dat <- child
    }
    return(return)
  }

  # safely perform the request and memoise
  res <- memoised_safe_api_call(
    req = req,
    process_fn = process_area_nested_subsystems,
    context = "cvd_area_nested_subsystems"
  )

  # if successful return the result, otherwise the error tibble
  if (res$success) {
    res$result
  } else {
    res$tibble
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
  v1 <- validate_input_id(
    id = area_id,
    param_name = "area_id",
    required = TRUE
  )
  if (!isTRUE(v1)) {
    return(v1)
  }

  # compose the request
  req <-
    httr2::request(get_api_base_url()) |>
    httr2::req_url_path_append(glue::glue('area/{area_id}/flatSubSystems'))

  # process function
  process_area_flat_subsystems <- function(parsed) {
    # defensive check
    if (length(parsed[[1]]) < 2 || !is.list(parsed[[1]])) {
      return(
        cvd_error_tibble(
          context = "cvd_area_flat_subsystems",
          error = "Response does not contain expected structure.",
          status = NA_integer_,
          url = httr2::req_get_url(req),
          params = NA_character_,
          resp = NA_character_
        )
      )
    }

    # continue processing
    parsed[[1]] |>
      purrr::compact() |>
      tibble::as_tibble() |>
      dplyr::relocate(
        dplyr::any_of(c('SystemLevels')),
        .after = dplyr::last_col()
      ) |>
      tidyr::unnest(cols = dplyr::any_of(c("SubSystems")), names_sep = "_")
  }

  # safely perform the request and memoise
  res <- memoised_safe_api_call(
    req = req,
    process_fn = process_area_flat_subsystems,
    context = "cvd_area_flat_subsystems"
  )

  # if successful return the result, otherwise the error tibble
  if (res$success) {
    res$result
  } else {
    res$tibble
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
  v1 <- validate_input_id(
    id = time_period_id,
    param_name = "time_period_id",
    required = TRUE,
    valid_ids = m_get_valid_time_period_ids()
  )
  if (!isTRUE(v1)) {
    return(v1)
  }

  v2 <- validate_input_id(
    id = system_level_id,
    param_name = "system_level_id",
    required = TRUE,
    valid_ids = m_get_valid_system_level_id_for_time_period_id(
      time_period_id = time_period_id
    )
  )
  if (!isTRUE(v2)) {
    return(v2)
  }

  # compose the request
  req <-
    httr2::request(get_api_base_url()) |>
    httr2::req_url_path_append("indicator/list") |>
    httr2::req_url_query(
      `timePeriodID` = time_period_id,
      `systemLevelID` = system_level_id
    )

  # process function
  process_indicator_list <- function(parsed) {
    # defensive check
    if (
      !"indicatorList" %in% names(parsed) ||
        length(parsed[["indicatorList"]]) < 2
    ) {
      return(
        cvd_error_tibble(
          context = "cvd_area_search",
          error = "Response does not contain expected `indicatorList` structure",
          url = httr2::req_get_url(req)
        )
      )
    }

    # continue processing
    parsed$indicatorList |>
      purrr::compact() |>
      dplyr::as_tibble() |>
      dplyr::arrange(dplyr::pick(dplyr::any_of("IndicatorID")))
  }

  # safely perform the request and memoise
  res <- memoised_safe_api_call(
    req = req,
    process_fn = process_indicator_list,
    context = "cvd_area_search"
  )

  # if successful return the result, otherwise the error tibble
  if (res$success) {
    res$result
  } else {
    res$tibble
  }
}

#' List indicators and associated metrics for a system level and time period
#'
#' @description
#' Retrieves all CVD indicators available for a given reporting period and given system level from the CVDPREVENT API, with an expanded view that includes 'MetricList' array for each indicator. This allows you to see not only which indicators are available, but also the specific metrics (e.g., breakdowns by age, sex or other attributes) associated with each indicator in the selected context.
#'
#' Only indicators with available data for the specified time period and system level are returned. This function is useful for determining what granular metric breakdowns are provided for each indicator.
#'
#' @section API Documentation:
#' See the [CVDPREVENT API documentation: Indicator metric list](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#*Proposed*%2Findicator%2FmetricList)
#'
#' @param time_period_id Integer (required). The reporting period (time period) for which to return indicators and metrics. use [cvd_time_period_list()] to find valid IDs.
#' @param system_level_id Integer (required). The system level (e.g., National, Region, ICB, PCN, Practice) for which to return indicators and metrics. Use [cvd_area_system_level()] to find valid IDs for a given time period.
#'
#' @return
#' A tibble containing one row for each indicator-metric pair avialable for the specified system level and time period. Columns typically include `IndicatorID`, `IndicatorShortName`, `MetricID`, `MetricCategoryName`, `CategoryAttribute`, and other metric-related fields.
#' If no indicators or metrics are found returns a tibble describing the error.
#'
#' @details
#' Use this function to explore the detailed metric breakdowns available for each indicator before performing data extraction or analysis. The `MetricList` column is unnested for convenience, so each row represents a single metric linked to an indicator.
#'
#' @examples
#' # List metrics for the prevalence of atrial fibrillation (indicator ID 1),
#' # focusing on metrics for the 40-59 years age group at the national level:
#' cvd_indicator_metric_list(time_period_id = 17, system_level_id = 1) |>
#'   dplyr::filter(IndicatorID == 1, MetricCategoryName == "40-59") |>
#'   dplyr::count(IndicatorID, IndicatorShortName, MetricID, MetricCategoryName, CategoryAttribute) |>
#'   dplyr::select(-n)
#'
#' # Get all indicator-metric pairs for GP practice level (system level 5) in a given period
#' cvd_indicator_metric_list(time_period_id = 17, system_level_id = 5)
#'
#' @export
cvd_indicator_metric_list <- function(time_period_id, system_level_id) {
  # validate_input
  v1 <- validate_input_id(
    id = time_period_id,
    param_name = "time_period_id",
    required = TRUE,
    valid_ids = m_get_valid_time_period_ids()
  )
  if (!isTRUE(v1)) {
    return(v1)
  }

  v2 <- validate_input_id(
    id = system_level_id,
    param_name = "system_level_id",
    required = TRUE,
    valid_ids = m_get_valid_system_level_id_for_time_period_id(
      time_period_id = time_period_id
    )
  )
  if (!isTRUE(v2)) {
    return(v2)
  }

  # compose the request
  req <-
    httr2::request(get_api_base_url()) |>
    httr2::req_url_path_append('indicator/metricList') |>
    httr2::req_url_query(
      `timePeriodID` = time_period_id,
      `systemLevelID` = system_level_id
    )

  # process function
  process_indicator_metric_list <- function(parsed) {
    # defensive check
    if (length(parsed[[2]]) < 2 || !is.list(parsed[[2]])) {
      return(
        cvd_error_tibble(
          context = "cvd_indicator_metric_list",
          error = "Response does not contain expected structure.",
          status = NA_integer_,
          url = httr2::req_get_url(req),
          params = NA_character_,
          resp = NA_character_
        )
      )
    }

    # continue process
    dat <-
      parsed[[2]] |>
      purrr::compact() |>
      tibble::as_tibble()

    # move metrics to the last column and expand
    if ("MetricList" %in% names(dat)) {
      dat <-
        dat |>
        dplyr::relocate(
          dplyr::any_of(c("MetricList")),
          .after = dplyr::last_col()
        ) |>
        tidyr::unnest(cols = dplyr::any_of(c("MetricList")))
    }
    return(dat)
  }

  # safely perform the request and memoise
  res <- memoised_safe_api_call(
    req = req,
    process_fn = process_indicator_metric_list,
    context = "cvd_indicator_metric_list"
  )

  # if successful return the result, otherwise the error tibble
  if (res$success) {
    res$result
  } else {
    res$tibble
  }
}

#' Retrieve all indicators and their data for a given time period and area
#'
#' @description
#' Returns all CVD indicators and related data for a specified reporting period (`time_period_id`) and NHS area (`area_id`) from the CVDPREVENT API. Also retrieves time series data for all available periods. Optionally, you can filter results by one or more indicator tags.
#'
#' The returned object is a nemd list of tibbles, including details about indicators, metric categories, metric data and time series, making this function ideal for comprehensive data extraction and downstream analysis.
#'
#' @section API Documentation:
#' See the [CVDPREVENT API documentation: Indicator](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Findicator)
#'
#' @param time_period_id Integer (required). The reporting period (time period) for which to return indicator data. Use the [cvd_time_period_list()] to find valid IDs.
#' @param area_id Integer (required). The AreaID for which to return indicator data. Use [cvd_area_list()] or [cvd_area_search()] to find valid IDs.
#' @param tag_id Numeric vector (optional). One or more tag IDs to filter indicators by tag. Use [cvd_indicator_tags()] to find valid IDs.
#'
#' @return
#' A named list containing up to four tibbles:
#' \describe{
#'   \item{indicators}{Tibble of indicators for the area and time period.}
#'   \item{metric_categories}{Tibble of metric categories related to the indicators.}
#'   \item{metric_data}{Tibble of metric values for the area and indicators.}
#'   \item{timeseries_data}{Tibble of time series data for metrics and indicators across time periods.}
#' }
#' If no indicators are found, returns a tibble describing the error.
#'
#' @details
#' This function is useful for extracting all indicator data for a given area and period, including breakdowns by category and time series. The list output allows easy access to different data tables for further analysis or visualisation. Filtering by tag enables targeted queries for specific subsets of indicators.
#'
#' @seealso
#' [cvd_indicator_list()], [cvd_indicator_metric_list()], [cvd_indicator_tags()], [cvd_indicator_details()], [cvd_indicator_sibling()], [cvd_indicator_child_data()], [cvd_indicator_data()], [cvd_indicator_metric_data()], [cvd_indicator_raw_data()], [cvd_indicator_nationalarea_metric_data()], [cvd_indicator_priority_groups()], [cvd_indicator_pathway_group()], [cvd_indicator_group()], [cvd_indicator_metric_timeseries()], [cvd_indicator_person_timeseries()], [cvd_indicator_metric_systemlevel_comparison()], [cvd_indicator_metric_area_breakdown()]
#'
#' @examples
#' # Get all indicator data for area_id = 1103 in time period 17
#' return_list <- cvd_indicator(time_period_id = 17, area_id = 1103)
#'
#' # See what data tables are available
#' summary(return_list)
#'
#' # Extract and examine indicators
#' indicators <- return_list$indicators
#' indicators |>
#'   dplyr::select(IndicatorID, IndicatorCode, IndicatorShortName) |>
#'   dplyr::arrange(IndicatorID) |>
#'   dplyr::slice_head(n = 4)
#'
#' # Extract metric categories for a specific indicator and categories
#' categories <- return_list$metric_categories
#' categories |>
#'   dplyr::filter(IndicatorID == 7, MetricCategoryID %in% c(7, 8)) |>
#'   dplyr::select(
#'     IndicatorID,
#'     MetricCategoryTypeName,
#'     CategoryAttribute,
#'     MetricCategoryName,
#'     MetricID
#'   )
#'
#' # Extract metric data for specific metrics
#' metric_data <- return_list$metric_data
#' metric_data |>
#'   dplyr::filter(MetricID %in% c(126, 132)) |>
#'   dplyr::select(MetricID, Value, Numerator, Denominator)
#'
#' # Extract time series data for selected metrics
#' timeseries_data <- return_list$timeseries_data
#' timeseries_data |>
#'   dplyr::filter(MetricID %in% c(126, 132), !is.na(Value))
#'
#' # Filter by tags: get indicators tagged with either tag 3 or 4 in area 3, time period 17
#' return_list <- cvd_indicator(time_period_id = 17, area_id = 3, tag_id = c(3, 4))
#'
#' @export
cvd_indicator <- function(time_period_id, area_id, tag_id = NULL) {
  # validate input
  v1 <- validate_input_id(
    id = time_period_id,
    param_name = "time_period_id",
    required = TRUE,
    valid_ids = m_get_valid_time_period_ids()
  )
  if (!isTRUE(v1)) {
    return(v1)
  }

  v2 <- validate_input_id(
    id = area_id,
    param_name = "area_id",
    required = TRUE
    # valid_ids = m_get_valid_area_ids_for_time_period_id(
    #   time_period_id = time_period_id
    # )
  )
  if (!isTRUE(v1)) {
    return(v2)
  }

  v3 <- validate_input_id_vector(
    ids = tag_id,
    param_name = "tag_id",
    required = FALSE,
    valid_ids = m_get_valid_tag_ids()
  )
  if (!isTRUE(v3)) {
    return(v3)
  }

  # compose the request
  if (base::missing(tag_id)) {
    req <- httr2::request(get_api_base_url()) |>
      httr2::req_url_path_append('indicator') |>
      httr2::req_url_query(
        `timePeriodID` = time_period_id,
        `areaID` = area_id
      )
  } else {
    req <-
      httr2::request(get_api_base_url()) |>
      httr2::req_url_path_append('indicator') |>
      httr2::req_url_query(
        `timePeriodID` = time_period_id,
        `areaID` = area_id,
        `tagID` = tag_id,
        .multi = 'explode'
      )
  }

  # process function
  process_indicator <- function(parsed) {
    # defensive check
    if (
      !"indicatorList" %in% names(parsed) ||
        length(parsed[["indicatorList"]]) < 2
    ) {
      return(
        cvd_error_tibble(
          context = "cvd_area_search",
          error = "Response does not contain expected `indicatorList` structure",
          url = httr2::req_get_url(req)
        )
      )
    }

    # continue process
    dat <-
      parsed$indicatorList |>
      purrr::compact() |>
      tibble::as_tibble()

    # `dat` is a complex, multi-layered object that contains details about the
    # requested indicator and also potentially related metric categories and data
    # (nested in a variable named 'Categories') and time series data, (nested in a
    # variable named 'TimeSeries').
    # Separate tibbles for indicator, metric categories, metric data and time series
    # data will be extracted and returned as part of a named list.

    # compose the return list
    return <- list()

    # get indicator data
    indicators <-
      dat |>
      dplyr::select(-dplyr::any_of(c("Categories"))) |>
      dplyr::distinct()

    # handle where no data is returned
    if (tibble::is_tibble(indicators) && nrow(indicators) == 0) {
      cli::cli_alert_danger(
        "{.fn cvdprevent::cvd_indicator} returned no data"
      )
      return(
        tibble::tibble(
          context = "cvd_indicator",
          result = "No data",
          timestamp = Sys.time()
        )
      )
    } else {
      return <-
        return |>
        append(list("indicators" = indicators))
    }

    # get metric categories
    if ("Categories" %in% names(dat)) {
      # extract metric data
      metrics <-
        dat |>
        dplyr::select(dplyr::any_of(c("IndicatorID", "Categories"))) |>
        tidyr::unnest(cols = dplyr::any_of(c("Categories")))

      # extract metric categories
      metric_categories <-
        metrics |>
        dplyr::select(
          -dplyr::any_of(c("TimeSeries")),
          -dplyr::starts_with("Data.")
        ) |>
        dplyr::distinct()

      return <-
        return |>
        append(list("metric_categories" = metric_categories))

      # extract metric data
      if ("Data" %in% names(metrics)) {
        metric_data <-
          metrics |>
          dplyr::select(c(
            dplyr::any_of(c("MetricID", "Data"))
            # dplyr::starts_with("Data")
          )) |>
          tidyr::unnest(cols = dplyr::any_of("Data")) |>
          dplyr::rename_with(
            .cols = dplyr::starts_with("Data."),
            ~ base::gsub(
              pattern = "Data.",
              replacement = "",
              x = .x,
              fixed = TRUE
            )
          ) |>
          dplyr::distinct()

        return <-
          return |>
          append(list("metric_data" = metric_data))
      }

      # extract timeseries data
      if ("TimeSeries" %in% names(metrics)) {
        timeseries <-
          metrics |>
          dplyr::select(dplyr::any_of(c("MetricID", "TimeSeries"))) |>
          tidyr::unnest(cols = dplyr::any_of("TimeSeries")) |>
          dplyr::distinct()

        return <-
          return |>
          append(list("timeseries_data" = timeseries))
      }
    }

    return(return)
  }

  # safely perform the request and memoise
  res <- memoised_safe_api_call(
    req = req,
    process_fn = process_indicator,
    context = "cvd_indicator"
  )

  # if successful return the result, otherwise the error tibble
  if (res$success) {
    res$result
  } else {
    res$tibble
  }
}

#' List all available indicator tags
#'
#' @description
#' Retrieves a list of all tags from the CVDPREVENT API that can be used to filter indicators. Tags provide a way to categorise and search for indicators by clinical or reporting groupings (such as "Priority Group", "Pathway Group" or other clinical categories).
#'
#' Use this function to obtain valid tag IDs for use in functions that support filtering by tag, such as [cvd_indicator()].
#'
#' @section API Documentation:
#' See the [CVDPREVENT API documentation: Indicator tags](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Findicator%2Ftags)
#'
#' @return
#' A tibble with one row per available indicator tag. Typical columns include `IndicatorTagID`, `IndicatorTagName` and other descriptive fields.
#' If no tags are found, returns a tibble describing the error.
#'
#' @details
#' Tags are useful for grouping or filtering indicators in dashboards, reports or scripted analyses. Tag IDs returned by this function can be supplied to functions like [cvd_indicator()] via the `tag_id` argument for targeted queries.
#'
#' @seealso
#' [cvd_indicator_list()], [cvd_indicator_metric_list()], [cvd_indicator()], [cvd_indicator_details()], [cvd_indicator_sibling()], [cvd_indicator_child_data()], [cvd_indicator_data()], [cvd_indicator_metric_data()], [cvd_indicator_raw_data()], [cvd_indicator_nationalarea_metric_data()], [cvd_indicator_priority_groups()], [cvd_indicator_pathway_group()], [cvd_indicator_group()], [cvd_indicator_metric_timeseries()], [cvd_indicator_person_timeseries()], [cvd_indicator_metric_systemlevel_comparison()], [cvd_indicator_metric_area_breakdown()]
#'
#' @examples
#' # List the first five indicator tags
#' cvd_indicator_tags() |>
#'   dplyr::arrange(IndicatorTagID) |>
#'   dplyr::slice_head(n = 5)
#'
#' # Use a tag ID to filter indicators in another query
#' tags <- cvd_indicator_tags()
#' tag_id <- tags$IndicatorTagID[1]
#' cvd_indicator(time_period_id = 17, area_id = 3, tag_id = tag_id)
#'
#' @export
cvd_indicator_tags <- function() {
  # compose the request
  req <-
    httr2::request(get_api_base_url()) |>
    httr2::req_url_path_append('indicator/tags')

  # process function
  process_indicator_tags <- function(parsed) {
    # defensive check
    if (
      !"indicatorTagList" %in% names(parsed) ||
        length(parsed[["indicatorTagList"]]) < 2
    ) {
      return(
        cvd_error_tibble(
          context = "cvd_indicator_tags",
          error = "Response does not contain expected `indicatorTagList` structure",
          url = httr2::req_get_url(req)
        )
      )
    }

    # continue process
    parsed$indicatorTagList |>
      purrr::compact() |>
      tibble::as_tibble() |>
      dplyr::arrange(dplyr::pick(dplyr::any_of(c("IndicatorTagID"))))
  }

  # safely perform the request and memoise
  res <- memoised_safe_api_call(
    req = req,
    process_fn = process_indicator_tags,
    context = "cvd_indicator_tags"
  )

  # if successful return the result, otherwise the error tibble
  if (res$success) {
    res$result
  } else {
    res$tibble
  }
}

#' Retrieve details for a specific indicator
#'
#' @description
#' Returns metadata and descriptive information for a single CVD indicator, identified by its IndicatorID, from the CVDPREVENT API. This function allows you to programmatically access the definitions, titles and metadata fields associated with specific indicators for use in reporting, dashboards or documentation.
#'
#' @section API Documentation:
#' See the [CVDPREVENT API documentation: Indicator details](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Findicator%2F%3Cindicator_ID%3E%2Fdetails)
#'
#' @param indicator_id Integer (required). The IndicatorID for which to return details. Used [cvd_indicator_list()] or [cvd_indicator_metric_list()] to find valid IDs.
#'
#' @return
#' A tibble containing metadata and details for the specified indicator. Typical columns include `IndicatorID`, `MetaDataTitle`, `MetaData`, and other descriptive fields.
#' If no indicator details are found, returns a tibble describing the error.
#'
#' @details
#' Use this function to retrieve indicator definitions, full names, and metadata fields for use in custom reports or to provide documentation / tooltips in analytical applications. Metadata fields are unnested for convenience.
#'
#' @seealso
#' [cvd_indicator_list()], [cvd_indicator_metric_list()], [cvd_indicator()], [cvd_indicator_tags()], [cvd_indicator_sibling()], [cvd_indicator_child_data()], [cvd_indicator_data()], [cvd_indicator_metric_data()], [cvd_indicator_raw_data()], [cvd_indicator_nationalarea_metric_data()], [cvd_indicator_priority_groups()], [cvd_indicator_pathway_group()], [cvd_indicator_group()], [cvd_indicator_metric_timeseries()], [cvd_indicator_person_timeseries()], [cvd_indicator_metric_systemlevel_comparison()], [cvd_indicator_metric_area_breakdown()]
#'
#' @examples
#' # Retrieve details for indicator with ID 7
#' cvd_indicator_details(indicator_id = 7) |>
#'   dplyr::select(IndicatorID, MetaDataTitle, MetaData) |>
#'   dplyr::slice_head(n = 5)
#'
#' # Find a valid indicator ID, then get its details
#' indicators <- cvd_indicator_list(time_period_id = 17, system_level_id = 5)
#' cvd_indicator_details(indicator_id = indicators$IndicatorID[1])
#'
#' @export
cvd_indicator_details <- function(indicator_id) {
  # validate input
  v1 <- validate_input_id(
    id = indicator_id,
    param_name = "indicator_id",
    required = TRUE
  )
  if (!isTRUE(v1)) {
    return(v1)
  }

  # compose the request
  req <-
    httr2::request(get_api_base_url()) |>
    httr2::req_url_path_append(glue::glue('indicator/{indicator_id}/details'))

  # process function
  process_indicator_details <- function(parsed) {
    # defensive check
    if (
      !"indicatorDetails" %in% names(parsed) ||
        length(parsed[["indicatorDetails"]]) < 2
    ) {
      return(
        cvd_error_tibble(
          context = "cvd_indicator_details",
          error = "Response does not contain expected `indicatorDetails` structure",
          url = httr2::req_get_url(req)
        )
      )
    }

    # continue process
    parsed$indicatorDetails |>
      purrr::compact() |>
      tibble::as_tibble() |>
      dplyr::relocate(
        dplyr::any_of("MetaData"),
        .after = dplyr::last_col()
      ) |>
      tidyr::unnest(cols = dplyr::any_of("MetaData"))
  }

  # safely perform the request and memoise
  res <- memoised_safe_api_call(
    req = req,
    process_fn = process_indicator_details,
    context = "cvd_indicator_details"
  )

  # if successful return the result, otherwise the error tibble
  if (res$success) {
    res$result
  } else {
    res$tibble
  }
}

#' Indicator sibling data
#'
#' @description
#' Returns data for all sibling areas (i.e., areas sharing the same parent) and the specified area itself, for a given metric and reporting period from the CVDPREVENT API. This endpoint is intended to provide a direct comparison of a single metric across related areas (e.g., all PCNs within an ICB, or all practices within an PCN).
#'
#' Only the selected metric is returned for each sibling area.
#'
#' @section API Documentation:
#' See the [CVDPREVENT API documentation: Indicator sibling data](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Findicator%2FsiblingData)
#'
#' @param time_period_id Integer (required). The reporting period (time period) for which to return sibling data. Use [cvd_time_period_list()] to obtain valid IDs.
#' @param area_id Integer (required). The AreaID for which sibling data will be determined. Use [cvd_area_list()] or [cvd_area_search()] to find valid IDs.
#' @param metric_id Integer (required). The MetricID for which to return data. Use [cvd_indicator_metric_list()] or [cvd_indicator_data()] to find valid MetricIDs.
#'
#' @return
#' A tibble containing the data for the specified metric in the selected area and all its siblings for the given reporting period. Columns typically include `AreaID`, `AreaName`, `Value`, `LowerConfidenceLimit`, `UpperConfidenceLimit`, and other relevant metric information.
#' If no sibling data is found, returns a tibble describing the error.
#'
#' @details
#' Use this function to compare a metric across all areas at the same heirarchical level (e.g., compare all practices in a PCN or all PCNs in an ICB) for benchmarking and visualisation purposes.
#'
#' @seealso
#' [cvd_indicator_list()], [cvd_indicator_metric_list()], [cvd_indicator()], [cvd_indicator_tags()], [cvd_indicator_details()], [cvd_indicator_child_data()], [cvd_indicator_data()], [cvd_indicator_metric_data()], [cvd_indicator_raw_data()], [cvd_indicator_nationalarea_metric_data()], [cvd_indicator_priority_groups()], [cvd_indicator_pathway_group()], [cvd_indicator_group()], [cvd_indicator_metric_timeseries()], [cvd_indicator_person_timeseries()], [cvd_indicator_metric_systemlevel_comparison()], [cvd_indicator_metric_area_breakdown()]
#'
#' @examples
#' # Compare the value of metric 126 for area 1103 and all its siblings in time period 17
#' cvd_indicator_sibling(time_period_id = 17, area_id = 1103, metric_id = 126) |>
#'   dplyr::select(AreaID, AreaName, Value, LowerConfidenceLimit, UpperConfidenceLimit)
#'
#' # Find a valid metric ID for an indicator, then get sibling data
#' metrics <- cvd_indicator_metric_list(time_period_id = 17, system_level_id = 5)
#' metric_id <- metrics$MetricID[1]
#' cvd_indicator_sibling(time_period_id = 17, area_id = 1103, metric_id = metric_id)
#'
#' @export
cvd_indicator_sibling <- function(
  time_period_id,
  area_id,
  metric_id
) {
  # validate input
  v1 <- validate_input_id(
    id = time_period_id,
    param_name = "time_period_id",
    required = TRUE,
    valid_ids = m_get_valid_time_period_ids()
  )
  if (!isTRUE(v1)) {
    return(v1)
  }

  v2 <- validate_input_id(
    id = area_id,
    param_name = "area_id",
    required = TRUE
    # valid_ids = m_get_valid_area_ids_for_time_period_id(
    #   time_period_id = time_period_id
    # )
  )
  if (!isTRUE(v2)) {
    return(v2)
  }

  v3 <- validate_input_id(
    id = metric_id,
    param_name = "metric_id",
    required = TRUE
    # valid_ids = m_get_valid_metric_ids_for_time_period_id_and_area_id(
    #   time_period_id = time_period_id,
    #   area_id = area_id
    # )
  )
  if (!isTRUE(v3)) {
    return(v3)
  }

  # compose the request
  req <-
    httr2::request(get_api_base_url()) |>
    httr2::req_url_path_append('indicator/siblingData') |>
    httr2::req_url_query(
      `timePeriodID` = time_period_id,
      `areaID` = area_id,
      `metricID` = metric_id
    )

  # process function
  process_indicator_sibling <- function(parsed) {
    # defensive check
    if (length(parsed) < 2 || !is.list(parsed[[2]])) {
      return(
        cvd_error_tibble(
          context = "cvd_indicator_sibling",
          error = "Response does not contain expected structure.",
          status = NA_integer_,
          url = httr2::req_get_url(req),
          params = NA_character_,
          resp = NA_character_
        )
      )
    }

    # continue process
    parsed[[2]] |>
      purrr::compact() |>
      tibble::as_tibble() |>
      dplyr::select(
        -dplyr::any_of(c(
          "NotificationCount",
          "HighestPriorityNotificationType"
        ))
      ) |>
      dplyr::relocate(dplyr::any_of("Data"), .after = dplyr::last_col()) |>
      tidyr::unnest(col = dplyr::any_of("Data"))
  }

  # safely perform the request and memoise
  res <- memoised_safe_api_call(
    req = req,
    process_fn = process_indicator_sibling,
    context = "cvd_indicator_sibling"
  )

  # if successful return the result, otherwise the error tibble
  if (res$success) {
    res$result
  } else {
    res$tibble
  }
}

#' Retrieve child area data for a specific metric, time period and area
#'
#' @description
#' Returns the value of a single metric for all child areas of a specified NHS area (and the specified area itself) for a chosen reporting period, using the CVDPREVENT API. This function enables direct comparison of a specific metric across all subordinate areas (e.g., all GP practices within a PCN, or all PCNs within an ICB).
#'
#' Only the selected metric is returned for each child area.
#'
#' @section API Documentation:
#' See the [CVDPREVENT API documentation: Indicator child data](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Findicator%2FchildData)
#'
#' @param time_period_id Integer (required). The reporting period (time period) for which to return child data. Use [cvd_time_period_list()] to find valid IDs.
#' @param area_id Integer (required). The AreaID for which to find child areas. Use [cvd_area_list()] or [cvd_area_search()] to find valid IDs.
#' @param metric_id Integer (required). The MetricID for which to retrieve values. Use [cvd_indicator_metric_list()] or [cvd_indicator_data()] to find valid MetricIDs.
#'
#' @return
#' A tibble with the value of the specified metric for the given area and all its child areas, for the specified time period. Typical columns include `AreaID`, `AreaName`, `Value`, `LowerConfidenceLimit`, `UpperConfidenceLimit` and other relevant metric information.
#' If no child data is found, returns a tibble describing the error.
#'
#' @details
#' Use this function to compare a metric across all immediate child areas under a parent (for example, to benchmark all GP practices within a PCN for a specific indicator and reporting period).
#'
#' @seealso
#' [cvd_indicator_list()], [cvd_indicator_metric_list()], [cvd_indicator()], [cvd_indicator_tags()], [cvd_indicator_details()], [cvd_indicator_sibling()], [cvd_indicator_data()], [cvd_indicator_metric_data()], [cvd_indicator_raw_data()], [cvd_indicator_nationalarea_metric_data()], [cvd_indicator_priority_groups()], [cvd_indicator_pathway_group()], [cvd_indicator_group()], [cvd_indicator_metric_timeseries()], [cvd_indicator_person_timeseries()], [cvd_indicator_metric_systemlevel_comparison()], [cvd_indicator_metric_area_breakdown()]
#'
#' @examples
#' # Compare the value of metric 126 for area 74 and all its child areas in time period 22
#' cvd_indicator_child_data(time_period_id = 22, area_id = 74, metric_id = 126) |>
#'   dplyr::select(AreaID, AreaName, Value, LowerConfidenceLimit, UpperConfidenceLimit)
#'
#' # Find a valid metric ID for an indicator, then get child area data
#' metrics <- cvd_indicator_metric_list(time_period_id = 17, system_level_id = 5)
#' metric_id <- metrics$MetricID[1]
#' cvd_indicator_child_data(time_period_id = 17, area_id = 1103, metric_id = metric_id)
#'
#' @export
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
    httr2::request(get_api_base_url()) |>
    httr2::req_url_path_append('indicator/childData') |>
    httr2::req_url_query(
      `timePeriodID` = time_period_id,
      `areaID` = area_id,
      `metricID` = metric_id
    )

  process_indicator_child_data <- function(parsed) {
    # defensive check
    if (length(parsed[[1]]) < 2 || !is.list(parsed[[1]])) {
      return(
        cvd_error_tibble(
          context = "cvd_indicator_child_data",
          error = "Response does not contain expected structure.",
          status = NA_integer_,
          url = httr2::req_get_url(req),
          params = NA_character_,
          resp = NA_character_
        )
      )
    }

    # continue process
    dat <-
      parsed[[1]] |>
      purrr::compact() |>
      tibble::as_tibble()

    # only proceed if `dat` is a tibble and contains at least one column
    if (tibble::is_tibble(dat) && ncol(dat) > 0) {
      dat <-
        dat |>
        # prevent conflicts with columns of the same name in nested data
        dplyr::select(
          -dplyr::any_of(c(
            "NotificationCount",
            "HighestPriorityNotificationType"
          ))
        ) |>
        # move 'Data' to the end and unpack
        dplyr::relocate(
          dplyr::any_of(c("Data")),
          .after = dplyr::last_col()
        ) |>
        tidyr::unnest(col = dplyr::any_of("Data"))
    }
    return(dat)
  }

  # safely perform the request and memoise
  res <- memoised_safe_api_call(
    req = req,
    process_fn = process_indicator_child_data,
    context = "cvd_indicator_child_data"
  )

  # if successful return the result, otherwise the error tibble
  if (res$success) {
    res$result
  } else {
    res$tibble
  }
}

#' Retrieve CVD indicator data for a specific area and time period
#'
#' @description
#' Fetches all available metric breakdowns for a single cardiovascular disease (CVD) indicator from the CVDPREVENT API, scoped to a specified NHS area and reporting period. This includes subgroup data such as age, sex, ethnicity, deprivation quintile, and more.
#'
#' @section API Documentation:
#' See the [CVDPREVENT API documentation: Indicator data](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Findicator%2Fdata)
#'
#' @param time_period_id Integer (required). The reporting period (time period) to retrieve data for. Use [cvd_time_period_list()] to find valid IDs.
#' @param area_id Integer (required). The AreaID for which to retrieve indicator data. use [cvd_area_list()] or [cvd_area_search()] to find valid IDs.
#' @param indicator_id Integer (required). The IndicatorID for which to retrieve data. use [cvd_indicator_list()] or [cvd_indicator_metric_list()] to find valid IDs.
#'
#' @return
#' A named list containing three tibbles:
#' \describe{
#'   \item{indicator_metrics}{Tibble. Metadata and definitions for the selected indicator and its associated metrics.}
#'   \item{area_data}{Tibble. Metric values for the specified NHS area (`area_id`) across all available breakdowns.}
#'   \item{national_data}{Tibble. Metric values for England, used for benchmarking and comparison.}
#' }
#' If no indicator data is found, returns a tibble describing the error.
#'
#' \strong{indicator_metrics} contains the following items:
#' \describe{
#'   \item{AxisCharacter}{Character. Symbol used to represent the metric axis (e.g., "%").}
#'   \item{FormatDisplayName}{Character. Display format for the metric (e.g., "Proportion %").}
#'   \item{IndicatorCode}{Character. Unique code for the indicator (e.g., "CVDP002AF").}
#'   \item{IndicatorFormatID}{Integer. Internal ID for the indicator's format type.}
#'   \item{IndicatorID}{Integer. Unique identifier for the indicator.}
#'   \item{IndicatorName}{Character. Full descriptive name of the indicator.}
#'   \item{IndicatorOrder}{Integer. Display order for the indicator in dashboards or reports.}
#'   \item{IndicatorShortName}{Character. Abbreviated name of the indicator for display.}
#'   \item{NotificationCount}{Integer. Count of notifications associated with the indicator. Often zero.}
#'   \item{TimePeriodID}{Integer. ID of the reporting time period.}
#'   \item{TimePeriodName}{Character. Label for the reporting time period (e.g., "To March 2024").}
#'   \item{CategoryAttribute}{Character. Subgroup label (e.g., "Male", "Female", "Persons").}
#'   \item{MetricCategoryID}{Integer. Unique ID for the metric category.}
#'   \item{MetricCategoryName}{Character. Name of the subgroup or category (e.g., "18–39", "Sex").}
#'   \item{MetricCategoryOrder}{Integer. Display order for the metric category.}
#'   \item{MetricCategoryTypeName}{Character. Type of subgroup (e.g., "Age group", "Sex").}
#'   \item{MetricID}{Integer. Unique ID for the specific metric being measured.}
#' }
#'
#' \strong{area_data} and \strong{national_data} contain the following items:
#' \describe{
#'   \item{MetricID}{Integer. Unique identifier for the metric being measured.}
#'   \item{MetricCategoryTypeName}{Character. Type of subgroup (e.g., "Sex", "Age group").}
#'   \item{MetricCategoryName}{Character. Name of the subgroup (e.g., "Female", "18–39").}
#'   \item{CategoryAttribute}{Character. Label used to group individuals (e.g., "Male", "Persons").}
#'   \item{AreaCode}{Character. ONS code for the NHS area (e.g., "U60176").}
#'   \item{AreaID}{Integer. Internal ID for the NHS area.}
#'   \item{AreaName}{Character. Name of the NHS area (e.g., "3 Centres PCN").}
#'   \item{Count}{Integer. Total number of individuals in the subgroup.}
#'   \item{DataID}{Integer. Unique identifier for the data record.}
#'   \item{Denominator}{Numeric. Population or count used as the denominator in metric calculation.}
#'   \item{Factor}{Numeric. Scaling factor applied to the metric, if applicable. Often NA.}
#'   \item{HighestPriorityNotificationType}{Character. Notification priority level, if available. Often NA.}
#'   \item{LowerConfidenceLimit}{Numeric. Lower bound of the confidence interval for the metric value.}
#'   \item{Max}{Numeric. Maximum observed value for the metric across comparable areas.}
#'   \item{Median}{Numeric. Median value for the metric across comparable areas.}
#'   \item{Min}{Numeric. Minimum observed value for the metric across comparable areas.}
#'   \item{NotificationCount}{Integer. Count of notifications associated with the indicator. Often zero.}
#'   \item{Numerator}{Numeric. Count used as the numerator in metric calculation.}
#'   \item{Q20}{Numeric. 20th percentile value across comparable areas.}
#'   \item{Q40}{Numeric. 40th percentile value across comparable areas.}
#'   \item{Q60}{Numeric. 60th percentile value across comparable areas.}
#'   \item{Q80}{Numeric. 80th percentile value across comparable areas.}
#'   \item{TimePeriodID}{Integer. ID of the reporting time period.}
#'   \item{TimePeriodName}{Character. Label for the reporting time period (e.g., "To March 2024").}
#'   \item{UpperConfidenceLimit}{Numeric. Upper bound of the confidence interval for the metric value.}
#'   \item{Value}{Numeric. Calculated metric value (e.g., percentage of patients treated).}
#'   \item{ValueNote}{Character. Additional notes or flags about the value. Often NA.}
#' }
#'
#' @details
#' Use this function to obtain all metric values for a single indicator in a particular area and time period, such as for a local dashboard or a focussed report. For broader queries across multiple indicators, see [cvd_indicator()] or [cvd_indicator_metric_list()].
#'
#' @seealso
#' [cvd_indicator_list()], [cvd_indicator_metric_list()], [cvd_indicator()], [cvd_indicator_tags()], [cvd_indicator_details()], [cvd_indicator_sibling()], [cvd_indicator_child_data()], [cvd_indicator_metric_data()], [cvd_indicator_raw_data()], [cvd_indicator_nationalarea_metric_data()], [cvd_indicator_priority_groups()], [cvd_indicator_pathway_group()], [cvd_indicator_group()], [cvd_indicator_metric_timeseries()], [cvd_indicator_person_timeseries()], [cvd_indicator_metric_systemlevel_comparison()], [cvd_indicator_metric_area_breakdown()]
#'
#' @examples
#' # Retrieve all metric breakdowns for indicator 7 in area 1103 for time period 17
#' returned_list <- cvd_indicator_data(time_period_id = 17, area_id = 1103, indicator_id = 7)
#'
#' # See the structure of this data
#' returned_list |> dplyr::glimpse()
#'
#' # See the definition for one metric
#' returned_list$indicator_metrics |>
#'   dplyr::slice_head(n = 1) |>
#'   dplyr::glimpse()
#'
#' # Compare performance in the specified area (AreaID = 1103) with national results for
#' # women aged 40-59 years (MetricID = 132)
#' dplyr::bind_rows(
#'   returned_list$area_data,
#'   returned_list$national_data
#' ) |>
#' dplyr::filter(MetricID == 132)
#'
#' @export
cvd_indicator_data <- function(
  time_period_id,
  area_id,
  indicator_id
) {
  # validate input
  v1 <- validate_input_id(
    id = time_period_id,
    param_name = "time_period_id",
    required = TRUE,
    valid_ids = m_get_valid_time_period_ids()
  )
  if (!isTRUE(v1)) {
    return(v1)
  }

  v2 <- validate_input_id(
    id = area_id,
    param_name = "area_id",
    required = TRUE,
    valid_ids = m_get_valid_area_ids_for_time_period_id(
      time_period_id = time_period_id
    )
  )
  if (!isTRUE(v2)) {
    return(v2)
  }

  v3 <- validate_input_id(
    id = indicator_id,
    param_name = "indicator_id",
    required = TRUE,
    valid_ids = m_get_valid_indicator_ids_for_time_period_id_and_area_id(
      time_period_id = time_period_id,
      area_id = area_id
    )
  )
  if (!isTRUE(v3)) {
    return(v3)
  }

  # compose the request
  req <-
    httr2::request(get_api_base_url()) |>
    httr2::req_url_path_append(glue::glue('indicator/{indicator_id}/data')) |>
    httr2::req_url_query(
      `timePeriodID` = time_period_id,
      `areaID` = area_id
    )

  # process function
  process_indicator_data <- function(parsed) {
    # defensive check
    if (length(parsed) < 2 || !is.list(parsed[[2]])) {
      return(
        cvd_error_tibble(
          context = "cvd_indicator_data",
          error = "Response does not contain expected structure.",
          status = NA_integer_,
          url = httr2::req_get_url(req),
          params = NA_character_,
          resp = NA_character_
        )
      )
    }

    # continue process
    dat <-
      parsed[[2]] |>
      purrr::compact() |>
      tibble::as_tibble() |>
      dplyr::relocate(
        dplyr::any_of(c("Categories")),
        .after = dplyr::last_col()
      ) |>
      tidyr::unnest(col = dplyr::any_of(c("Categories")))

    # set up the return object
    # return <- list("raw" = dat)
    return <- list()

    # append the indicator_metrics data
    indicator_metrics <-
      dat |>
      dplyr::select(-dplyr::any_of(c("AreaData", "NationalData")))

    return <-
      return |>
      append(list("indicator_metrics" = indicator_metrics))

    # extract area data if available
    if ("AreaData" %in% names(dat)) {
      area_data <-
        dat |>
        dplyr::select(dplyr::any_of(c(
          "MetricID",
          "MetricCategoryTypeName",
          "MetricCategoryName",
          "CategoryAttribute",
          "AreaData"
        ))) |>
        tidyr::unnest(cols = dplyr::any_of(c("AreaData")))

      return <-
        return |>
        append(list("area_data" = area_data))
    }

    # extract national data if available
    if ("NationalData" %in% names(dat)) {
      national_data <-
        dat |>
        dplyr::select(dplyr::any_of(c(
          "MetricID",
          "MetricCategoryTypeName",
          "MetricCategoryName",
          "CategoryAttribute",
          "NationalData"
        ))) |>
        tidyr::unnest(cols = dplyr::any_of(c("NationalData")))

      return <-
        return |>
        append(list("national_data" = national_data))
    }
    return(return)
  }

  # safely perform the request and memoise
  res <- memoised_safe_api_call(
    req = req,
    process_fn = process_indicator_data,
    context = "cvd_indicator_data"
  )

  # if successful return the result, otherwise the error tibble
  if (res$success) {
    res$result
  } else {
    res$tibble
  }
}


#' Retrieve metric data for a specific metric, time period and area
#'
#' @description
#' Returns detailed area data for a single CVD metric (`metric_id`) for a specified NHS area (`area_id`) and reporting period (`time_period_id`) from the CVDPREVENT API. This function provides all values and breakdowns available for the selected metric within the chosen context, allowing analysis and visualisation of precise, granular results (e.g., age groups, sexes, ethnicities).
#'
#' @section API Documentation:
#' See the [CVDPREVENT API documentation: Indicator metric data](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Findicator%2FmetricData)
#'
#' @param time_period_id Integer (required). The reporting period (time period) for which to return metric data. Use [cvd_time_period_list()] to find valid IDs.
#' @param area_id Integer (required). The AreaID for which to return metric data. Use [cvd_area_list()] or [cvd_area_search()] to find valid IDs.
#' @param metric_id Integer (required). The MetricID for which to retrieve values. Use [cvd_indicator_metric_list()] or [cvd_indicator_data()] to find valid MetricIDs.
#'
#' @return
#' A named list containing three tibbles:
#' \describe{
#'   \item{metrics}{Tibble. Metadata and definitions for the selected metric.}
#'   \item{area_data}{Tibble. Metric values for the specified NHS area (`area_id`).}
#'   \item{national_data}{Tibble. Metric values for England, used for benchmarking and comparison.}
#' }
#' If no indicator data is found, returns a tibble describing the error.
#'
#' \strong{indicator_metrics} contains the following items:
#' \describe{
#'   \item{AxisCharacter}{Character. Symbol used to represent the metric axis (e.g., "%").}
#'   \item{FormatDisplayName}{Character. Display format for the metric (e.g., "Proportion %").}
#'   \item{IndicatorCode}{Character. Unique code for the indicator (e.g., "CVDP002AF").}
#'   \item{IndicatorFormatID}{Integer. Internal ID for the indicator's format type.}
#'   \item{IndicatorID}{Integer. Unique identifier for the indicator.}
#'   \item{IndicatorName}{Character. Full descriptive name of the indicator.}
#'   \item{IndicatorOrder}{Integer. Display order for the indicator in dashboards or reports.}
#'   \item{IndicatorShortName}{Character. Abbreviated name of the indicator for display.}
#'   \item{NotificationCount}{Integer. Count of notifications associated with the indicator. Often zero.}
#'   \item{TimePeriodID}{Integer. ID of the reporting time period.}
#'   \item{TimePeriodName}{Character. Label for the reporting time period (e.g., "To March 2024").}
#'   \item{CategoryAttribute}{Character. Subgroup label (e.g., "Male", "Female", "Persons").}
#'   \item{MetricCategoryID}{Integer. Unique ID for the metric category.}
#'   \item{MetricCategoryName}{Character. Name of the subgroup or category (e.g., "18–39", "Sex").}
#'   \item{MetricCategoryOrder}{Integer. Display order for the metric category.}
#'   \item{MetricCategoryTypeName}{Character. Type of subgroup (e.g., "Age group", "Sex").}
#'   \item{MetricID}{Integer. Unique ID for the specific metric being measured.}
#' }
#'
#' \strong{area_data} and \strong{national_data} contain the following items:
#' \describe{
#'   \item{MetricID}{Integer. Unique identifier for the metric being measured.}
#'   \item{MetricCategoryTypeName}{Character. Type of subgroup (e.g., "Sex", "Age group").}
#'   \item{MetricCategoryName}{Character. Name of the subgroup (e.g., "Female", "18–39").}
#'   \item{CategoryAttribute}{Character. Label used to group individuals (e.g., "Male", "Persons").}
#'   \item{AreaCode}{Character. ONS code for the NHS area (e.g., "U60176").}
#'   \item{AreaID}{Integer. Internal ID for the NHS area.}
#'   \item{AreaName}{Character. Name of the NHS area (e.g., "3 Centres PCN").}
#'   \item{Count}{Integer. Total number of individuals in the subgroup.}
#'   \item{DataID}{Integer. Unique identifier for the data record.}
#'   \item{Denominator}{Numeric. Population or count used as the denominator in metric calculation.}
#'   \item{Factor}{Numeric. Scaling factor applied to the metric, if applicable. Often NA.}
#'   \item{HighestPriorityNotificationType}{Character. Notification priority level, if available. Often NA.}
#'   \item{LowerConfidenceLimit}{Numeric. Lower bound of the confidence interval for the metric value.}
#'   \item{Max}{Numeric. Maximum observed value for the metric across comparable areas.}
#'   \item{Median}{Numeric. Median value for the metric across comparable areas.}
#'   \item{Min}{Numeric. Minimum observed value for the metric across comparable areas.}
#'   \item{NotificationCount}{Integer. Count of notifications associated with the indicator. Often zero.}
#'   \item{Numerator}{Numeric. Count used as the numerator in metric calculation.}
#'   \item{Q20}{Numeric. 20th percentile value across comparable areas.}
#'   \item{Q40}{Numeric. 40th percentile value across comparable areas.}
#'   \item{Q60}{Numeric. 60th percentile value across comparable areas.}
#'   \item{Q80}{Numeric. 80th percentile value across comparable areas.}
#'   \item{TimePeriodID}{Integer. ID of the reporting time period.}
#'   \item{TimePeriodName}{Character. Label for the reporting time period (e.g., "To March 2024").}
#'   \item{UpperConfidenceLimit}{Numeric. Upper bound of the confidence interval for the metric value.}
#'   \item{Value}{Numeric. Calculated metric value (e.g., percentage of patients treated).}
#'   \item{ValueNote}{Character. Additional notes or flags about the value. Often NA.}
#' }
#'
#' @details
#' Use this function to retrieve all available breakdowns for a metric in a specific area and period, such as for in-depth local reporting, dashboard figures, or subgroup analysis. It is best used when you know the exact metric required for your query.
#'
#' @seealso
#' [cvd_indicator_list()], [cvd_indicator_metric_list()], [cvd_indicator()], [cvd_indicator_tags()], [cvd_indicator_details()], [cvd_indicator_sibling()], [cvd_indicator_child_data()], [cvd_indicator_data()], [cvd_indicator_raw_data()], [cvd_indicator_nationalarea_metric_data()], [cvd_indicator_priority_groups()], [cvd_indicator_pathway_group()], [cvd_indicator_group()], [cvd_indicator_metric_timeseries()], [cvd_indicator_person_timeseries()], [cvd_indicator_metric_systemlevel_comparison()], [cvd_indicator_metric_area_breakdown()]
#'
#' @examples
#' # Retrieve  a single metric breakdown showing how men aged 40-59 years
#' # are treated with anticoagulants (MetricID = 126) for 3 Centres PCN
#' # (AreaID = 1103) to March 2020 (TimePeriodID = 1)
#' returned_list <- cvd_indicator_metric_data(
#'   time_period_id = 1,
#'   area_id = 1103,
#'   metric_id = 126
#' )
#'
#' # See the structure of this data
#' returned_list |> dplyr::glimpse()
#'
#' # See the definition for this metric
#' returned_list$metrics |> dplyr::glimpse()
#'
#' # Compare performance between our area and the national average
#' dplyr::bind_rows(
#'   returned_list$area_data,
#'   returned_list$national_data
#' ) |>
#' dplyr::glimpse()
#'
#' @export
cvd_indicator_metric_data <- function(
  metric_id,
  time_period_id,
  area_id
) {
  # validate input
  v1 <- validate_input_id(
    id = time_period_id,
    param_name = "time_period_id",
    required = TRUE,
    valid_ids = m_get_valid_time_period_ids()
  )
  if (!isTRUE(v1)) {
    return(v1)
  }

  v2 <- validate_input_id(
    id = area_id,
    param_name = "area_id",
    required = TRUE
    # valid_ids = m_get_valid_area_ids_for_time_period_id(
    #   time_period_id = time_period_id
    # )
  )
  if (!isTRUE(v2)) {
    return(v2)
  }

  v3 <- validate_input_id(
    id = metric_id,
    param_name = "metric_id",
    required = TRUE
    # valid_ids = m_get_valid_metric_ids_for_time_period_id_and_area_id(
    #   time_period_id = time_period_id,
    #   area_id = area_id
    # )
  )
  if (!isTRUE(v3)) {
    return(v3)
  }

  # compose the request
  req <-
    httr2::request(get_api_base_url()) |>
    httr2::req_url_path_append(glue::glue(
      'indicator/metric/{metric_id}/data'
    )) |>
    httr2::req_url_query(
      `timePeriodID` = time_period_id,
      `areaID` = area_id
    )

  # process function
  process_indicator_metric_data <- function(parsed) {
    # defensive check
    if (length(parsed) < 2 || !is.list(parsed[[2]])) {
      return(
        cvd_error_tibble(
          context = "cvd_indicator_metric_data",
          error = "Response does not contain expected structure.",
          status = NA_integer_,
          url = httr2::req_get_url(req),
          params = NA_character_,
          resp = NA_character_
        )
      )
    }

    # continue process
    dat <-
      parsed[[2]] |>
      purrr::compact() |>
      tibble::as_tibble() |>
      dplyr::relocate(
        dplyr::any_of(c("Categories")),
        .after = dplyr::last_col()
      ) |>
      tidyr::unnest(col = dplyr::any_of(c("Categories")))

    # set up the return object
    # return <- list("raw" = dat)
    return <- list()

    # append the indicator_metrics data
    metrics <-
      dat |>
      dplyr::select(-dplyr::any_of(c("AreaData", "NationalData")))

    return <-
      return |>
      append(list("metrics" = metrics))

    # extract area data if available
    if ("AreaData" %in% names(dat)) {
      area_data <-
        dat |>
        dplyr::select(dplyr::any_of(c(
          "MetricID",
          "MetricCategoryTypeName",
          "MetricCategoryName",
          "CategoryAttribute",
          "AreaData"
        ))) |>
        tidyr::unnest(cols = dplyr::any_of(c("AreaData")))

      return <-
        return |>
        append(list("area_data" = area_data))
    }

    # extract national data if available
    if ("NationalData" %in% names(dat)) {
      national_data <-
        dat |>
        dplyr::select(dplyr::any_of(c(
          "MetricID",
          "MetricCategoryTypeName",
          "MetricCategoryName",
          "CategoryAttribute",
          "NationalData"
        ))) |>
        tidyr::unnest(cols = dplyr::any_of(c("NationalData")))

      return <-
        return |>
        append(list("national_data" = national_data))
    }
    return(return)
  }

  # safely perform the request and memoise
  res <- memoised_safe_api_call(
    req = req,
    process_fn = process_indicator_metric_data,
    context = "cvd_indicator_metric_data"
  )

  # if successful return the result, otherwise the error tibble
  if (res$success) {
    res$result
  } else {
    res$tibble
  }
}

#' Retrieve raw metric values for multiple metrics, a specified area and time period
#'
#' @description
#' Returns raw values for multiple metrics within an indicator (specified as `indicator_id`) for a single NHS system level and reporting period using the CVDPREVENT API. This function fetches unfiltered raw data at the metric level, allowing comprehensive extraction for all selected metrics and their available breakdowns (such as by age, sex or other category) within the chosen context.
#'
#' @section API Documentation:
#' See the [CVDPREVENT API documentation: Indicator raw data](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Findicator%2F%3Cindicator_ID%3E%2FrawDataJSON)
#'
#' @param time_period_id Integer (required). The reporting period (time period) for which to retrieve data. Use [cvd_time_period_system_levels()] to find valid IDs.
#' @param system_level_id Integer (required). The SystemLevelID for which to retrieve data. Use [cvd_time_period_system_levels()] to find valid IDs.
#' @param indicator_id Integer vector (required). One or more IndicatorIDs specifying which indicator and its associated metrics to return. Use [cvd_indicator_list()] to find valid IndicatorIDs.
#'
#' @return
#' A tibble with one row per metric breakdown for all requested metrics. The tibble has the following columns:
#' \describe{
#'   \item{AreaCode}{Character. ONS geographic code for the area (e.g., "E92000001" for England).}
#'   \item{AreaName}{Character. Name of the geographic area.}
#'   \item{CategoryAttribute}{Character. Subgroup label (e.g., "Male", "Female", "Persons").}
#'   \item{Denominator}{Numeric. Population or count used as the denominator in metric calculation.}
#'   \item{Factor}{Numeric. Scaling factor applied to the metric, if applicable. May be NA.}
#'   \item{HighestPriorityNotificationType}{Character. Notification priority level, if available. Often NA.}
#'   \item{IndicatorCode}{Character. Unique code for the indicator (e.g., "CVDP002AF").}
#'   \item{IndicatorName}{Character. Full descriptive name of the indicator.}
#'   \item{IndicatorShortName}{Character. Abbreviated name of the indicator.}
#'   \item{LowerConfidenceLimit}{Numeric. Lower bound of the confidence interval for the metric value.}
#'   \item{MetricCategoryName}{Character. Name of the subgroup or category (e.g., "40–59", "Female").}
#'   \item{MetricCategoryTypeName}{Character. Type of subgroup (e.g., "Age group", "Sex", "Ethnicity").}
#'   \item{NotificationCount}{Integer. Count of notifications associated with the indicator. Often zero.}
#'   \item{Numerator}{Numeric. Count used as the numerator in metric calculation.}
#'   \item{TimePeriodName}{Character. Label for the time period (e.g., "To December 2024").}
#'   \item{UpperConfidenceLimit}{Numeric. Upper bound of the confidence interval for the metric value.}
#'   \item{Value}{Numeric. Calculated metric value (e.g., percentage of patients treated).}
#'   \item{ValueNote}{Character. Additional notes or flags about the value. Often NA.}
#' }
#'
#' @details
#' Use this function to retrieve a wide set of metric breakdowns for a given indicator in a single area and time period - useful for broad data extractions, dashboards or advanced analytics.
#'
#' @examples
#' # Retrieve metric data for 'CVD: All-cause mortality' (IndicatorID = 35) across
#' # NHS Regions (SystemLevelID = 6) in the period April 2024 to
#' # March 2025 (TimePeriodID = 27) and view a sample of 4 rows:
#' cvd_indicator_raw_data(
#'   time_period_id = 27,
#'   system_level_id = 6,
#'   indicator_id = 35
#' ) |>
#'   dplyr::slice_sample(n = 4)
#'
#' # Find a valid indicator IDs for a specified time period and system level,
#' # then retrieve raw data for one of these
#' indicators <- cvd_indicator_list(time_period_id = 22, system_level_id = 4)
#' cvd_indicator_raw_data(
#'   time_period_id = 22,
#'   system_level_id = 4,
#'   indicator_id = indicators$IndicatorID[1]
#' )
#'
#' @export
cvd_indicator_raw_data <- function(
  time_period_id,
  system_level_id,
  indicator_id
) {
  # validate input
  v1 <- validate_input_id(
    id = time_period_id,
    param_name = "time_period_id",
    required = TRUE,
    valid_ids = m_get_valid_time_period_ids()
  )
  if (!isTRUE(v1)) {
    return(v1)
  }

  v2 <- validate_input_id(
    id = system_level_id,
    param_name = "system_level_id",
    required = TRUE,
    valid_ids = m_get_valid_system_level_id_for_time_period_id(
      time_period_id = time_period_id
    )
  )
  if (!isTRUE(v2)) {
    return(v2)
  }

  v3 <- validate_input_id(
    id = indicator_id,
    param_name = "indicator_id",
    required = TRUE,
    valid_ids = m_get_valid_indicator_ids_for_time_period_id_and_system_level_id(
      time_period_id = time_period_id,
      system_level_id = system_level_id
    )
  )
  if (!isTRUE(v3)) {
    return(v3)
  }

  # compose the request
  req <-
    httr2::request(get_api_base_url()) |>
    httr2::req_url_path_append(glue::glue(
      'indicator/{indicator_id}/rawDataJSON'
    )) |>
    httr2::req_url_query(
      `timePeriodID` = time_period_id,
      `systemLevelID` = system_level_id
    )

  # process function
  process_indicator_raw_data <- function(parsed) {
    # defensive check
    if (length(parsed) < 2 || !is.list(parsed[[2]])) {
      return(
        cvd_error_tibble(
          context = "cvd_indicator_raw_data",
          error = "Response does not contain expected structure.",
          status = NA_integer_,
          url = httr2::req_get_url(req),
          params = NA_character_,
          resp = NA_character_
        )
      )
    }

    # continue process
    parsed[[2]] |>
      purrr::compact() |>
      tibble::as_tibble()
  }

  # safely perform the request and memoise
  res <- memoised_safe_api_call(
    req = req,
    process_fn = process_indicator_raw_data,
    context = "cvd_indicator_raw_data"
  )

  # if successful return the result, otherwise the error tibble
  if (res$success) {
    res$result
  } else {
    res$tibble
  }
}

#' Retrieve metric data for a specific area and for national (England) comparison
#'
#' @description
#' Returns a named list of tibbles containing: (1) metric data for the specified NHS area and the national (England, AreaID = 1) aggregate, and (2) details achieving the target value (if defined), including the target percentage and the additional number of patients needed to reach the target. This function supports benchmarking local performance vs. the national average, and helps quantify gaps to clinical targets.
#'
#' If there is no data for either national or the chosen area for the given parameters, an error tibble is returned.
#'
#' @section API Documentation:
#' See the [CVDPREVENT API documentation: Indicator national area metric data](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Findicator%2FnationalAreaMetricData)
#'
#' @param time_period_id Integer (required). The reporting period (time period) for which to retrieve metric data. Use [cvd_time_period_list()] to find valid IDs.
#' @param area_id Integer (required). The AreaID for which to retrieve data in addition to the national aggregate. use [cvd_area_list()] or [cvd_area_search()] to find valid IDs.
#' @param metric_id Integer (required). The MetricID for which to retrieve values. Use [cvd_indicator_metric_list()] or [cvd_indicator_data()] to find valid MetricIDs.
#'
#' @return
#' A named list with up to two tibbles:
#' \describe{
#'   \item{area}{Tibble with one or more rows, summarising the metric for the specified area and the England aggregate (AreaID = 1). Typical columns include `AreaID`, `AreaName`, `MetricID`, `Value`, `Numerator`, `Denominator`, `LowerConfidenceLimit`, `UpperConfidenceLimit`, `MetricCategoryName`, `CategoryAttribute`, etc.}
#'   \item{target}{Tibble (if available) with target-setting details for the area, including columns such as: `TargetValue` (target as a percentage up to 100, integer), and `TargetPatients` (number of additional patients required to reach the target).}
#' }
#' If no data exists for both the area and the national aggregate for the given parameters, returns a tibble describing the error.
#'
#' @details
#' Use this function to benchmark a local area's metric value against the national figure and to understand the actual gap to a clinically meaningful target.
#'
#' @seealso
#' [cvd_indicator_list()], [cvd_indicator_metric_list()], [cvd_indicator_metric_data()], [cvd_indicator_data()], [cvd_indicator_raw_data()], [cvd_area_list()]
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
#' area_data
#'
#' # Extract `target` details
#' target_data <- return_list$target
#' target_data
#'
#' @export
cvd_indicator_nationalarea_metric_data <- function(
  time_period_id,
  area_id,
  metric_id
) {
  # validate input
  v1 <- validate_input_id(
    id = time_period_id,
    param_name = "time_period_id",
    required = TRUE,
    valid_ids = m_get_valid_time_period_ids()
  )
  if (!isTRUE(v1)) {
    return(v1)
  }

  v2 <- validate_input_id(
    id = area_id,
    param_name = "area_id",
    required = TRUE,
    valid_ids = m_get_valid_area_ids_for_time_period_id(
      time_period_id = time_period_id
    )
  )
  if (!isTRUE(v2)) {
    return(v2)
  }

  v3 <- validate_input_id(
    id = metric_id,
    param_name = "metric_id",
    required = TRUE,
    valid_ids = m_get_valid_metric_ids_for_time_period_id_and_area_id(
      time_period_id = time_period_id,
      area_id = area_id
    )
  )
  if (!isTRUE(v3)) {
    return(v3)
  }

  # compose the request
  req <-
    httr2::request(get_api_base_url()) |>
    httr2::req_url_path_append(glue::glue(
      'indicator/nationalVsAreaMetricData/{metric_id}'
    )) |>
    httr2::req_url_query(
      `timePeriodID` = time_period_id,
      `areaID` = area_id
    )

  # process function
  process_indicator_nationalarea_metric_data <- function(parsed) {
    # defensive check
    if (length(parsed) < 2 || !is.list(parsed[[2]])) {
      return(
        cvd_error_tibble(
          context = "cvd_indicator_nationalarea_metric_data",
          error = "Response does not contain expected structure.",
          status = NA_integer_,
          url = httr2::req_get_url(req),
          params = NA_character_,
          resp = NA_character_
        )
      )
    }

    # continue process
    dat <-
      parsed[[2]] |>
      purrr::compact()

    # set up the return object
    return <- list()

    # handle area data
    if ("AreaData" %in% names(dat)) {
      # extract area data - the first element
      area_data <-
        dat |>
        dplyr::nth(1) |>
        tibble::as_tibble()

      return <-
        return |>
        append(list("area" = area_data))
    }

    # handle target data
    if ("TargetData" %in% names(dat)) {
      # extract target data - the second element
      target_data <-
        dat |>
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
          append(list("target" = target_data))
      }
    }

    return(return)
  }

  # safely perform the request and memoise
  res <- memoised_safe_api_call(
    req = req,
    process_fn = process_indicator_nationalarea_metric_data,
    context = "cvd_indicator_nationalarea_metric_data"
  )

  # if successful return the result, otherwise the error tibble
  if (res$success) {
    res$result
  } else {
    res$tibble
  }
}

#' List all indicator priority groups
#'
#' @description
#' Retrieves a tibble of indicator priority groups from the CVDPREVENT API. Priority groups reflect high-level clinical, operational or policy themes (such as "Inequalities" or "NHS Long Term Plan") and provide a way to cluster or filter multiple indicators for reporting and analytics.
#'
#' @section API Documentation:
#' See the [CVDPREVENT API documentation: Indicator priority groups](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Findicator%2FpriorityGroup)
#'
#' @return
#' A tibble with one row per indicator / priority group. Key columns typically include `PriorityGroupID`, `PriorityGroup` (the display name), `PriorityGroupDisplayOrder`, and `IndicatorName`. Other columns may be present to support specialised reporting or grouping.
#' If no priority groups are found, returns a tibble describing the error.
#'
#' @details
#' Use this function to provide grouping / filtering options for dashboards or reports, or to explore which indicator themes are tracked in CVDPREVENT. Typically, you will select the priority group's name and ID for grouping or filtering tasks.
#'
#' @seealso
#' [cvd_indicator_list()], [cvd_indicator_group()], [cvd_area_list()], [cvd_time_period_list()]
#'
#' @examples
#' # List all available priority group display names and their IDs
#' cvd_indicator_priority_groups() |>
#'   dplyr::select(PriorityGroupID, PriorityGroup)
#'
#' # Preview group names for a sidebar filter in a dashboard
#' groups <- cvd_indicator_priority_groups()
#' unique(groups$PriorityGroup)
#'
#' @export
cvd_indicator_priority_groups <- function() {
  # compose the request
  req <-
    httr2::request(get_api_base_url()) |>
    httr2::req_url_path_append('indicator/priorityGroups')

  # process function
  process_indicator_priority_groups <- function(parsed) {
    # defensive check
    if (length(parsed[[1]]) < 2 || !is.list(parsed[[1]])) {
      return(
        cvd_error_tibble(
          context = "cvd_indicator_priority_groups",
          error = "Response does not contain expected structure.",
          status = NA_integer_,
          url = httr2::req_get_url(req),
          params = NA_character_,
          resp = NA_character_
        )
      )
    }

    # continue process
    parsed[[1]] |>
      purrr::compact() |>
      # this is a named list of tibbles, one for each of the priority groups
      # the next step combines these tibbles together into a single tibble
      # adding the name of each tibble as a new variable called 'PriorityGroup'
      dplyr::bind_rows(.id = "PriorityGroup")
  }

  # safely perform the request and memoise
  res <- memoised_safe_api_call(
    req = req,
    process_fn = process_indicator_priority_groups,
    context = "cvd_indicator_priority_groups"
  )

  # if successful return the result, otherwise the error tibble
  if (res$success) {
    res$result
  } else {
    res$tibble
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
  v1 <- validate_input_id(
    id = pathway_group_id,
    param_name = "pathway_group_id",
    required = TRUE,
    valid_ids = m_get_valid_pathway_group_ids()
  )
  if (!isTRUE(v1)) {
    return(v1)
  }

  # compose the request
  req <-
    httr2::request(get_api_base_url()) |>
    httr2::req_url_path_append(glue::glue(
      'indicator/pathwayGroup/{pathway_group_id}'
    ))

  # process function
  process_indicator_pathway_group <- function(parsed) {
    # defensive check
    if (length(parsed[[1]]) < 2 || !is.list(parsed[[1]])) {
      return(
        cvd_error_tibble(
          context = "cvd_indicator_pathway_group",
          error = "Response does not contain expected structure.",
          status = NA_integer_,
          url = httr2::req_get_url(req),
          params = NA_character_,
          resp = NA_character_
        )
      )
    }

    # continue process
    parsed[[1]] |>
      purrr::compact() |>
      tibble::as_tibble() |>
      dplyr::relocate(
        dplyr::any_of(c("Indicators")),
        .after = dplyr::last_col()
      ) |>
      tidyr::unnest(cols = dplyr::any_of(c("Indicators")))
  }

  # safely perform the request and memoise
  res <- memoised_safe_api_call(
    req = req,
    process_fn = process_indicator_pathway_group,
    context = "cvd_indicator_pathway_group"
  )

  # if successful return the result, otherwise the error tibble
  if (res$success) {
    res$result
  } else {
    res$tibble
  }
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
  v1 <- validate_input_id(
    id = indicator_group_id,
    param_name = "indicator_group_id",
    required = TRUE
  )
  if (!isTRUE(v1)) {
    return(v1)
  }

  # compose the request
  req <-
    httr2::request(get_api_base_url()) |>
    httr2::req_url_path_append(glue::glue(
      'indicator/indicatorGroup/{indicator_group_id}'
    ))

  # process function
  process_indicator_group <- function(parsed) {
    # defensive check
    if (length(parsed[[1]]) < 2 || !is.list(parsed[[1]])) {
      return(
        cvd_error_tibble(
          context = "cvd_indicator_group",
          error = "Response does not contain expected structure.",
          status = NA_integer_,
          url = httr2::req_get_url(req),
          params = NA_character_,
          resp = NA_character_
        )
      )
    }

    # continue process
    parsed[[1]] |>
      purrr::compact() |>
      tibble::as_tibble() |>
      # prevent conflicts with columns of the same name in nested data
      dplyr::select(
        -dplyr::any_of(c(
          "NotificationCount",
          "HighestPriorityNotificationType"
        ))
      ) |>
      dplyr::relocate(
        dplyr::any_of(c("Indicators")),
        .after = dplyr::last_col()
      ) |>
      tidyr::unnest(cols = dplyr::any_of(c("Indicators")))
  }

  # safely perform the request and memoise
  res <- memoised_safe_api_call(
    req = req,
    process_fn = process_indicator_group,
    context = "cvd_indicator_group"
  )

  # if successful return the result, otherwise the error tibble
  if (res$success) {
    res$result
  } else {
    res$tibble
  }
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
  v1 <- validate_input_id(
    id = metric_id,
    param_name = "metric_id",
    required = TRUE
  )
  if (!isTRUE(v1)) {
    return(v1)
  }

  v2 <- validate_input_id(
    id = area_id,
    param_name = "area_id",
    required = TRUE
  )
  if (!isTRUE(v2)) {
    return(v2)
  }

  # compose the request
  req <-
    httr2::request(get_api_base_url()) |>
    httr2::req_url_path_append(glue::glue(
      'indicator/timeSeriesByMetric/{metric_id}'
    )) |>
    httr2::req_url_query(
      `areaID` = area_id
    )

  # process function
  process_indicator_metric_timeseries <- function(parsed) {
    # defensive check
    if (length(parsed[[1]]) < 2 || !is.list(parsed[[1]])) {
      return(
        cvd_error_tibble(
          context = "cvd_indicator_metric_timeseries",
          error = "Response does not contain expected structure.",
          status = NA_integer_,
          url = httr2::req_get_url(req),
          params = NA_character_,
          resp = NA_character_
        )
      )
    }

    # continue process
    parsed[[1]] |>
      purrr::compact() |>
      tibble::as_tibble() |>
      tidyr::unnest(cols = dplyr::any_of(c("Areas"))) |>
      tidyr::unnest(cols = dplyr::any_of(c("TimeSeriesData")))
  }

  # safely perform the request and memoise
  res <- memoised_safe_api_call(
    req = req,
    process_fn = process_indicator_metric_timeseries,
    context = "cvd_indicator_metric_timeseries"
  )

  # if successful return the result, otherwise the error tibble
  if (res$success) {
    res$result
  } else {
    res$tibble
  }
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
  v1 <- validate_input_id(
    id = indicator_id,
    param_name = "indicator_id",
    required = TRUE
  )
  if (!isTRUE(v1)) {
    return(v1)
  }

  v2 <- validate_input_id(
    id = area_id,
    param_name = "area_id",
    required = TRUE
  )
  if (!isTRUE(v2)) {
    return(v2)
  }

  # compose the request
  req <-
    httr2::request(get_api_base_url()) |>
    httr2::req_url_path_append(glue::glue(
      'indicator/personsTimeSeriesByIndicator/{indicator_id}'
    )) |>
    httr2::req_url_query(
      `areaID` = area_id
    )

  # process function
  process_indicator_person_timeseries <- function(parsed) {
    # defensive check
    if (length(parsed[[1]]) < 2 || !is.list(parsed[[1]])) {
      return(
        cvd_error_tibble(
          context = "cvd_indicator_person_timeseries",
          error = "Response does not contain expected structure.",
          status = NA_integer_,
          url = httr2::req_get_url(req),
          params = NA_character_,
          resp = NA_character_
        )
      )
    }

    # continue process
    parsed[[1]] |>
      purrr::compact() |>
      tibble::as_tibble() |>
      tidyr::unnest(cols = dplyr::any_of(c("InequalityMarkers"))) |>
      tidyr::unnest(cols = dplyr::any_of(c("CategoryData")))
  }

  # safely perform the request and memoise
  res <- memoised_safe_api_call(
    req = req,
    process_fn = process_indicator_person_timeseries,
    context = "cvd_indicator_person_timeseries"
  )

  # if successful return the result, otherwise the error tibble
  if (res$success) {
    res$result
  } else {
    res$tibble
  }
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
  v1 <- validate_input_id(
    id = time_period_id,
    param_name = "time_period_id",
    required = TRUE,
    valid_ids = m_get_valid_time_period_ids()
  )
  if (!isTRUE(v1)) {
    return(v1)
  }

  v2 <- validate_input_id(
    id = area_id,
    param_name = "area_id",
    required = TRUE,
    valid_ids = m_get_valid_area_ids_for_time_period_id(
      time_period_id = time_period_id
    )
  )
  if (!isTRUE(v2)) {
    return(v2)
  }

  v3 <- validate_input_id(
    id = metric_id,
    param_name = "metric_id",
    required = TRUE,
    valid_ids = m_get_valid_metric_ids_for_time_period_id_and_area_id(
      time_period_id = time_period_id,
      area_id = area_id
    )
  )
  if (!isTRUE(v3)) {
    return(v3)
  }

  # compose the request
  req <-
    httr2::request(get_api_base_url()) |>
    httr2::req_url_path_append(glue::glue(
      'indicator/metricSystemLevelComparison/{metric_id}'
    )) |>
    httr2::req_url_query(
      `timePeriodID` = time_period_id,
      `areaID` = area_id
    )

  # process function
  process_indicator_metric_systemlevel_comparison <- function(parsed) {
    # defensive check
    if (length(parsed[[1]]) < 2 || !is.list(parsed[[1]])) {
      return(
        cvd_error_tibble(
          context = "cvd_indicator_metric_systemlevel_comparison",
          error = "Response does not contain expected structure.",
          status = NA_integer_,
          url = httr2::req_get_url(req),
          params = NA_character_,
          resp = NA_character_
        )
      )
    }

    # continue process
    parsed[[1]] |>
      purrr::compact() |>
      tibble::as_tibble() |>
      tidyr::unnest(cols = dplyr::any_of(c("SystemLevels"))) |>
      dplyr::relocate(
        dplyr::any_of(c("ComparisonData")),
        .after = dplyr::last_col()
      ) |>
      tidyr::unnest(cols = dplyr::any_of(c("ComparisonData")))
  }

  # safely perform the request and memoise
  res <- memoised_safe_api_call(
    req = req,
    process_fn = process_indicator_metric_systemlevel_comparison,
    context = "cvd_indicator_metric_systemlevel_comparison"
  )

  # if successful return the result, otherwise the error tibble
  if (res$success) {
    res$result
  } else {
    res$tibble
  }
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
  time_period_id,
  area_id,
  metric_id
) {
  # validate input
  v1 <- validate_input_id(
    id = time_period_id,
    param_name = "time_period_id",
    required = TRUE,
    valid_ids = m_get_valid_time_period_ids()
  )
  if (!isTRUE(v1)) {
    return(v1)
  }

  v2 <- validate_input_id(
    id = area_id,
    param_name = "area_id",
    required = TRUE,
    valid_ids = m_get_valid_area_ids_for_time_period_id(
      time_period_id = time_period_id
    )
  )
  if (!isTRUE(v2)) {
    return(v2)
  }

  v3 <- validate_input_id(
    id = metric_id,
    param_name = "metric_id",
    required = TRUE,
    valid_ids = m_get_valid_metric_ids_for_time_period_id_and_area_id(
      time_period_id = time_period_id,
      area_id = area_id
    )
  )
  if (!isTRUE(v3)) {
    return(v3)
  }

  # compose the request
  req <-
    httr2::request(get_api_base_url()) |>
    httr2::req_url_path_append(glue::glue(
      'indicator/metricAreaBreakdown/{metric_id}'
    )) |>
    httr2::req_url_query(
      `timePeriodID` = time_period_id,
      `areaID` = area_id
    )

  # process function
  process_indicator_metric_area_breakdown <- function(parsed) {
    # defensive check
    if (length(parsed[[1]]) < 2 || !is.list(parsed[[1]])) {
      return(
        cvd_error_tibble(
          context = "cvd_indicator_metric_area_breakdown",
          error = "Response does not contain expected structure.",
          status = NA_integer_,
          url = httr2::req_get_url(req),
          params = NA_character_,
          resp = NA_character_
        )
      )
    }

    # continue process
    parsed[[1]] |>
      purrr::compact() |>
      tibble::as_tibble() |>
      tidyr::unnest(cols = dplyr::any_of(c("SystemLevels"))) |>
      dplyr::relocate(
        dplyr::any_of(c("ComparisonData")),
        .after = dplyr::last_col()
      ) |>
      tidyr::unnest(cols = dplyr::any_of(c("ComparisonData")))
  }

  # safely perform the request and memoise
  res <- memoised_safe_api_call(
    req = req,
    process_fn = process_indicator_metric_area_breakdown,
    context = "cvd_indicator_metric_area_breakdown"
  )

  # if successful return the result, otherwise the error tibble
  if (res$success) {
    res$result
  } else {
    res$tibble
  }
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
    httr2::request(get_api_base_url()) |>
    httr2::req_url_path_append('externalResource')

  # process function
  process_external_resource <- function(parsed) {
    # defensive check
    if (length(parsed) < 2 || !is.list(parsed[[2]])) {
      return(
        cvd_error_tibble(
          context = "cvd_external_resource",
          error = "Response does not contain expected structure.",
          status = NA_integer_,
          url = httr2::req_get_url(req),
          params = NA_character_,
          resp = NA_character_
        )
      )
    }

    # continue process
    parsed[[2]] |>
      purrr::compact() |>
      tibble::as_tibble()
  }

  # safely perform the request and memoise
  res <- memoised_safe_api_call(
    req = req,
    process_fn = process_external_resource,
    context = "cvd_external_resource"
  )

  # if successful return the result, otherwise the error tibble
  if (res$success) {
    res$result
  } else {
    res$tibble
  }
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
  indicator_id = NULL, # optional
  metric_category_type_id = NULL # optional
) {
  # validate input
  v1 <- validate_input_id(
    id = time_period_id,
    param_name = "time_period_id",
    required = TRUE,
    valid_ids = m_get_valid_time_period_ids()
  )
  if (!isTRUE(v1)) {
    return(v1)
  }

  v2 <- validate_input_id(
    id = system_level_id,
    param_name = "system_level_id",
    required = TRUE,
    valid_ids = m_get_valid_system_level_id_for_time_period_id(
      time_period_id = time_period_id
    )
  )
  if (!isTRUE(v2)) {
    return(v2)
  }

  v3 <- validate_input_id(
    id = indicator_id,
    param_name = "indicator_id",
    required = FALSE
  )
  if (!isTRUE(v3)) {
    return(v3)
  }

  v4 <- validate_input_id(
    id = metric_category_type_id,
    param_name = "metric_category_type_id",
    required = FALSE
  )
  if (!isTRUE(v4)) {
    return(v4)
  }

  # compose the request
  req <-
    httr2::request(get_api_base_url()) |>
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

  # process function
  process_data_availability <- function(parsed) {
    # defensive check
    if (length(parsed[[1]]) < 2 || !is.list(parsed[[1]])) {
      return(
        cvd_error_tibble(
          context = "cvd_data_availability",
          error = "Response does not contain expected structure.",
          status = NA_integer_,
          url = httr2::req_get_url(req),
          params = NA_character_,
          resp = NA_character_
        )
      )
    }

    # continue process
    parsed[[1]] |>
      purrr::compact() |>
      dplyr::as_tibble()
  }

  # safely perform the request and memoise
  res <- memoised_safe_api_call(
    req = req,
    process_fn = process_data_availability,
    context = "cvd_data_availability"
  )

  # if successful return the result, otherwise the error tibble
  if (res$success) {
    res$result
  } else {
    res$tibble
  }
}
