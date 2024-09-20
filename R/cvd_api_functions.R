# setup ------------------------------------------------------------------------
url_base <- 'https://api.cvdprevent.nhs.uk'

globalVariables(
  c(
    'AreaData', 'Areas', 'Categories', 'Categories_MetricID', 'Categories_TimeSeries',
    'CategoryData', 'Children', 'Children_Children', 'Children_Children_Children',
    'ComparisonData', 'Data', 'IndicatorID', 'Indicators', 'InequalityMarkers', 'MetaData',
    'MetricList', 'SubSystems', 'SystemLevels', 'TimeSeriesData', 'IndicatorTypeID',
    'IndicatorTypeName', 'TimePeriodID', 'SystemLevelID'
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
#' # List available indicator types
#' cvd_indicator_types()
cvd_indicator_types <- function() {

  # get the data from the function - no parameters to return all types
  data <- cvd_time_period_list() |>
    dplyr::select(IndicatorTypeID, IndicatorTypeName) |>
    dplyr::distinct()
}

#' List time periods
#'
#' Returns all available time periods
#'
#' CVD Prevent API documentation:
#' [Time period](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2FtimePeriod)
#'
#' @param indicator_type_id integer - Indicator type ID, e.g. standard or outcome indicator type. If passed will show time periods containing data of the given type (optional)
#'
#' @return Tibble of time period details
#' @export
#' @seealso [cvd_indicator_types()], [cvd_time_period_system_levels()]
#'
#' @examples
#' # get a tibble of all periods
#' cvd_time_periods <- cvd_time_period_list()
#'
#' # filter for the latest four periods
#' cvd_time_period_list() |>
#'   dplyr::filter(IndicatorTypeName == 'Standard') |>
#'   dplyr::slice_max(order_by = TimePeriodID, n = 4) |>
#'   dplyr::select(TimePeriodID, TimePeriodName)
cvd_time_period_list <- function(indicator_type_id) {

  # compose the request
  req <-
    httr2::request(url_base) |>
    httr2::req_url_path_append('timePeriod')

  if(!missing(indicator_type_id)) {
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

#' Time periods and system levels
#'
#' Returns all available time periods along with the systems levels included
#' in each time period.
#'
#' CVD Prevent API documentation:
#' [Time period system levels](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#*Proposed*-%2FtimePeriod%2FsystemLevels)
#'
#' @return tibble of time periods and associated system levels
#' @export
#' @seealso [cvd_time_period_list()]
#'
#' @examples
#' # get a tibble of all periods and levels
#' periods_levels <- cvd_time_period_system_levels()
#'
#' # see which levels are available for the latest period
#' periods_levels |>
#'   dplyr::filter(TimePeriodID == max(TimePeriodID)) |>
#'   dplyr::select(TimePeriodID, TimePeriodName, SystemLevelID, SystemLevelName)
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
    dplyr::relocate(dplyr::any_of(c('SystemLevels')), .after = dplyr::last_col()) |>
    tidyr::unnest(cols = dplyr::any_of(c('SystemLevels'))) |>
    dplyr::arrange(TimePeriodID, SystemLevelID)

}

## area ------------------------------------------------------------------------

#' List system levels per time period
#'
#' Returns all available system levels for a specified time period.
#'
#' CVD Prevent API documentation:
#' [System levels per time period](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Farea%2FsystemLevel)
#'
#' @param time_period_id integer - the time period to return data for (compulsory)
#'
#' @return tibble of system levels available for the time period
#' @export
#' @seealso [cvd_area_details()], [cvd_area_unassigned()], [cvd_area_search()], [cvd_area_nested_subsystems()], [cvd_area_flat_subsystems()]
#'
#' @examples
#' # list system levels for time period 4
#' cvd_area_system_level(time_period_id = 4) |>
#'   dplyr::select(SystemLevelID, SystemLevelName)
cvd_area_system_level <- function(time_period_id = 1) {

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

#' List all system levels and available time periods
#'
#' Returns all available system levels along with the time periods where the
#' system levels occur.
#'
#' Note: this is the inverse of `cvd_time_period_system_levels()`.
#'
#' CVD Prevent API documentation:
#' [All system levels and time periods](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#*Proposed*%2Farea%2FsystemLevel%2FtimePeriods)
#'
#' @return tibble of system levels and reporting periods
#' @export
#' @seealso [cvd_time_period_system_levels()], [cvd_area_details()], [cvd_area_unassigned()], [cvd_area_search()], [cvd_area_nested_subsystems()], [cvd_area_flat_subsystems()]
#'
#' @examples
#' # list the latest four reporting periods at GP practice level
#' cvd_area_system_level_time_periods() |>
#'   dplyr::filter(SystemLevelName == 'Practice') |>
#'   dplyr::slice_max(order_by = TimePeriodID, n = 4) |>
#'   dplyr::select(SystemLevelName, TimePeriodID, TimePeriodName)
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


#' List areas
#'
#' Returns all areas for a given time period and parent area or system level.
#' Only areas which have data for the specified time period will be returned.
#'
#' Either parent area or system level must be specified:
#' If parent area is specified, all children areas of that parent will be returned.
#' If system level is specified, all areas within that system level will be returned.
#'
#' Parent area takes precedence over system level - if parent area is specified, system level is ignored.
#'
#' CVD Prevent API documentation:
#' [Area lists](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Farea)
#'
#' @param time_period_id integer - specifies time period for which to return areas (compulsory)
#' @param parent_area_id integer - specifies the area of which children will be returned (optional)
#' @param system_level_id integer - specifies which system levels to return areas for (optional)
#'
#' @return Tibble of area details
#' @export
#' @seealso [cvd_area_details()], [cvd_area_unassigned()], [cvd_area_search()], [cvd_area_nested_subsystems()], [cvd_area_flat_subsystems()]
#'
#' @examples
#' # list four PCNs with data available at time period 17
#' cvd_area_list(time_period_id = 17, system_level_id = 4) |>
#'   dplyr::select(SystemLevelName, AreaID, AreaCode, AreaName) |>
#'   dplyr::slice_head(n = 4)
cvd_area_list <- function(time_period_id = 1, parent_area_id, system_level_id) {

  # compose the request
  req <-
    httr2::request(url_base) |>
    httr2::req_url_path_append('area')

  if(base::missing(parent_area_id) & base::missing(system_level_id)) {
    # both optional arguments are missing
    req <- req |>
      httr2::req_url_query(
        `timePeriodID` = time_period_id
      )
  } else if(base::missing(parent_area_id)) {
    # system level id provided
    req <- req |>
      httr2::req_url_query(
        `timePeriodID` = time_period_id,
        `systemLevelID` = system_level_id
      )
  } else if(base::missing(system_level_id)) {
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
      dplyr::relocate(dplyr::any_of(c('Parents')), .after = dplyr::last_col()) |>
      tidyr::unnest(col = dplyr::any_of(c('Parents')))
  }

}
#test <- cvd_area_list(time_period_id = 17, system_level_id = 4)

#' Area details
#'
#' Returns details of a specific area at a given time period, including details about any parent and child areas.
#'
#' CVD Prevent API documentation:
#' [Area details](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Farea%2F%3Carea_id%3E%2Fdetails)
#'
#' @param time_period_id integer - specified time period for which to return details for, i.e. population and participation rate (compulsory)
#' @param area_id integer - specified area id for which to return details for.
#'
#' @return Named list of tibbles containing area 'area_details', child 'area_child_details' (where appropriate) and parent 'area_parent_details' (where appropriate)
#' @export
#' @seealso [cvd_area_list()], [cvd_area_unassigned()], [cvd_area_search()], [cvd_area_nested_subsystems()], [cvd_area_flat_subsystems()]
#'
#' @examples
#' # to see details for '3 Centres PCN' (area_id = 1103) use the following:
#' # get the list of tibbles from the function
#' returned_list <- cvd_area_details(time_period_id = 17, area_id = 1103)
#'
#' # view area details
#' returned_list$area_details |>
#'   dplyr::select(AreaCode, AreaName)
#'
#' # view details for the parent of this area
#' returned_list$area_parent_details |>
#'   dplyr::select(AreaID, AreaName, SystemLevelID)
#'
#' # view details for the children of this area
#' returned_list$area_child_details |>
#'   dplyr::select(AreaID, AreaName, SystemLevelID)
cvd_area_details <- function(time_period_id = 1, area_id = 1) {

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
  if('ParentAreaList' %in% names(data)) {

    area_parent_details <- data$ParentAreaList |>
      purrr::compact() |>
      dplyr::as_tibble() |>
      unique()

    return <- return |>
      append(list('area_parent_details' = area_parent_details))
  }

  # extract any child details
  if('ChildAreaList' %in% names(data)) {

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

#' Unassigned areas
#'
#' Returns a list of all areas which have data in the selected time period,
#' but do not have any parent areas assigned, and therefore are unreachable.
#'
#' CVD Prevent API documentation:
#' [Areas unassigned](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Farea%2Funassigned)
#'
#' @param time_period_id integer - time period for which Area must have data for (compulsory)
#' @param system_level_id integer - system level of areas in the unassigned list (optional)
#'
#' @return Tibble of details for areas without parent details
#' @export
#' @seealso [cvd_area_list()], [cvd_area_details()], [cvd_area_search()], [cvd_area_nested_subsystems()], [cvd_area_flat_subsystems()]
#'
#' @examples
#' # Report four GP practices (ID = 5) without parent PCN details:
#' cvd_area_unassigned(time_period_id = 17, system_level_id = 5) |>
#'   dplyr::slice_head(n = 4) |>
#'   dplyr::select(SystemLevelName, AreaID, AreaName)
#'
#' # England, as the highest system_level (ID = 1) does not have parent details
#' cvd_area_unassigned(time_period_id = 17, system_level_id = 1) |>
#'   dplyr::select(SystemLevelName, AreaID, AreaName)
cvd_area_unassigned <- function(time_period_id = 1, system_level_id) {

  # compose the request
  req <-
    httr2::request(url_base) |>
    httr2::req_url_path_append('area/unassigned')

  if(base::missing(system_level_id)) {
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

  if(length(data) == 0) {
    cli::cli_alert_danger('No unassigned areas returned')
    return(dplyr::tibble(result = 'No unassigned areas returned'))

  } else {
    data <- data |>
      purrr::compact() |>
      dplyr::as_tibble()

    return(data)
  }
}

#' Search for matching areas
#'
#' Returns a list of Areas that match a partial name for a given time period.
#' Uses simple LIKE '%<partial_area_name>%' comparison.
#'
#' CVD Prevent API documentation:
#' [Area search](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Farea%2Fsearch)
#'
#' @param partial_area_name string - string to use to search for an Area (compulsory)
#' @param time_period_id integer - limits the search to Areas which have data in specified time period (compulsory)
#'
#' @return Tibble of details for areas matching the search term
#' @export
#' @seealso [cvd_area_list()], [cvd_area_details()], [cvd_area_unassigned()], [cvd_area_nested_subsystems()], [cvd_area_flat_subsystems()]
#'
#' @examples
#' # search for areas matching the term 'practice'
#' cvd_area_search(partial_area_name = 'practice', time_period_id = 17) |>
#'   dplyr::select(AreaID, AreaName, AreaCode)
#'
#' # search for areas matching the term 'PCN'
#' cvd_area_search(partial_area_name = 'PCN', time_period_id = 17) |>
#'   dplyr::select(AreaID, AreaName, AreaCode)
cvd_area_search <- function(partial_area_name = 'Surgery', time_period_id = 1) {

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

  if(length(data) == 0) {
    cli::cli_alert_danger('No found areas returned')
    return(dplyr::tibble(result = 'No found areas returned'))

  } else {
    data <- data |>
      purrr::compact() |>
      dplyr::as_tibble()

    return(data)
  }
}

#' Area nested sub systems
#'
#' Returns given area and children areas in a nested structure
#'
#' CVD Prevent API documentation:
#' [Area nested subsystems](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Farea%2F%3Carea_id%3E%2FnestedSubSystems)
#'
#' @param area_id integer - the area to return data for (compulsory)
#'
#' @return List of named tibbles containing details for the area and each sub-level areas
#' @export
#' @seealso [cvd_area_list()], [cvd_area_details()], [cvd_area_unassigned()], [cvd_area_search()], [cvd_area_flat_subsystems()]
#'
#' @examples
#' # View details for for Somerset STP
#' returned_list <- cvd_area_nested_subsystems(area_id = 5)
#' returned_list |> summary()
#'
#' # see details for five of the immediate children of Somerset STP
#' returned_list$level_2 |>
#'   dplyr::slice_head(n = 5)
#'
#' # View details for Leicester Central PCN
#' returned_list <- cvd_area_nested_subsystems(area_id = 701)
#' returned_list |> summary()
#'
#' # see details for the GP practice children of the PCN
#' returned_list$level_2
cvd_area_nested_subsystems <- function(area_id = 5) {

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

  if(length(data) == 0) {
    cli::cli_alert_danger('No area returned')
    return(dplyr::tibble(result = 'No area returned'))

  } else {

    # set up the return
    return <- list()

    # extract any children in a loop and add them to the return as named object
    iter <- 0
    while(length(data) > 0) {

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

#' Area flat subsystems
#'
#' Similar to `cvd_area_nested_subsystems()` but the sub-areas are grouped
#' based on their system level.
#'
#' CVD Prevent API documentation:
#' [Area flat subsystems](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Farea%2F%3Carea_id%3E%2FflatSubSystems)
#'
#' @param area_id integer - the area to return data for (compulsory)
#'
#' @return Tibble of details for the area and its child areas (where applicable)
#' @export
#' @seealso [cvd_area_list()], [cvd_area_details()], [cvd_area_unassigned()], [cvd_area_search()], [cvd_area_nested_subsystems()]
#'
#' @examples
#' # View details for for Somerset STP
#' cvd_area_flat_subsystems(area_id = 5) |>
#'   dplyr::glimpse()
#'
#' # View details for Leicester Central PCN
#' cvd_area_flat_subsystems(area_id = 701) |>
#'   dplyr::glimpse()
cvd_area_flat_subsystems <- function(area_id = 5) {

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

  if(length(data) == 0) {
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

#' List indicators
#'
#' Returns basic details of all indicators for a given system level and time period.
#' Only returns indicators for which data exists in selected time period, and on
#' selected system level. Used to populate available indicator list in Data Explorer.
#'
#' CVD Prevent API documentation:
#' [Indicator list](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Findicator%2Flist)
#'
#' @param time_period_id integer - time period to reutrn data for (compulsory)
#' @param system_level_id integer - system level to return data for (compulsory)
#'
#' @return Tibble of details for indicators for the time period and system level
#' @export
#' @seealso [cvd_indicator_metric_list()], [cvd_indicator()],
#' [cvd_indicator_tags()], [cvd_indicator_details()], [cvd_indicator_sibling()],
#' [cvd_indicator_child_data()], [cvd_indicator_data()], [cvd_indicator_metric_data()],
#' [cvd_indicator_raw_data()], [cvd_indicator_nationalarea_metric_data()],
#' [cvd_indicator_priority_groups()], [cvd_indicator_pathway_group()], #
#' [cvd_indicator_group()], [cvd_indicator_metric_timeseries()],
#' [cvd_indicator_person_timeseries()], [cvd_indicator_metric_systemlevel_comparison()],
#' [cvd_indicator_metric_area_breakdown()]
#'
#' @examples
#' # List four indicators for time point 17 and GP practice level (system level 5)
#' cvd_indicator_list(time_period_id = 17, system_level_id = 5) |>
#'   dplyr::select(IndicatorID, IndicatorCode, IndicatorShortName) |>
#'   dplyr::slice_head(n = 4)
cvd_indicator_list <- function(time_period_id = 1, system_level_id = 2) {

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

  if(length(data) == 0) {
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
cvd_indicator_metric_list <- function(time_period_id = 1, system_level_id = 1) {
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

  if(length(data) == 0) {
    cli::cli_alert_danger('No indicators returned')
    return(dplyr::tibble(result = 'No indicators returned'))

  } else {
    data <- data |>
      purrr::compact() |>
      dplyr::as_tibble()

    if('MetricList' %in% names(data)) {
      data <-
        data |>
        dplyr::relocate(dplyr::any_of(c('MetricList')), .after = dplyr::last_col()) |>
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
#' @param tag_id numeric vector - allows filtering indicators by one or more tags (optional, array)
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
#' # extract category data
#' category_data <- return_list$category_data
#' category_data |>
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
cvd_indicator <- function(time_period_id = 1, area_id = 1, tag_id) {

  # compose the request
  if(base::missing(tag_id)) {
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


  if(length(data) == 0) {
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
    if('Categories' %in% names(data)) {
      # extract metric data
      metrics <- data |>
        dplyr::select(c(IndicatorID, dplyr::any_of('Categories'))) |>
        tidyr::unnest(cols = dplyr::any_of(c('Categories')))

      # extract metric categories
      metric_categories <- metrics |>
        dplyr::select(-c(dplyr::any_of(c('TimeSeries')), dplyr::starts_with('Data.'))) |>
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

  if(length(data) == 0) {
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
cvd_indicator_details <- function(indicator_id = 1) {

  # compose the request
  req <-
    httr2::request(url_base) |>
    httr2::req_url_path_append(glue::glue('indicator/{indicator_id}/details'))

  # check the indicator is valid
  if (as.numeric(indicator_id) > max(internal_list_indicator_ids())) {
    cli::cli_alert_danger('Indicator ID is not valid')
    return(dplyr::tibble(result = 'Indicator ID is not valid'))
  }

  # perform the request
  resp <- req |>
    httr2::req_perform() |>
    httr2::resp_body_string()

  # wrangle for output
  data <- jsonlite::fromJSON(resp, flatten = T)$indicatorDetails

  if(length(data) == 0) {
    cli::cli_alert_danger('No indicators details returned')
    return(dplyr::tibble(result = 'No indicator details returned'))

  } else {
    data <- data |>
      purrr::compact() |>
      dplyr::as_tibble() |>
      dplyr::relocate(MetaData, .after = dplyr::last_col()) |>
      tidyr::unnest(col = MetaData)

    return(data)
  }
}

#' INTERNAL FUNCTION - List available indicator IDs
#'
#' Returns a list of indicator IDs for the latest time period at GP practice level.
#' To be used as part of a check of valid indicator IDs
#'
#' @return Vector of indicator IDs
internal_list_indicator_ids <- function() {
  # get latest time period
  latest_time_id <- cvd_time_period_list() |>
    dplyr::slice_max(order_by = TimePeriodID) |>
    dplyr::pull(TimePeriodID)

  # list indicators for the latest time period
  indicator_list <-
    cvd_indicator_list(time_period_id = latest_time_id, system_level_id = 5) |>
    dplyr::arrange(IndicatorID) |>
    dplyr::pull(IndicatorID) |>
    base::unique()

  return(indicator_list)
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
cvd_indicator_sibling <- function(time_period_id = 17, area_id = 30, metric_id = 1) {

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
  data <- jsonlite::fromJSON(resp, flatten = T)[[2]] |>
    purrr::compact() |>
    dplyr::as_tibble() |>
    dplyr::relocate(Data, .after = dplyr::last_col()) |>
    tidyr::unnest(col = Data)

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
cvd_indicator_child_data <- function(time_period_id = 17, area_id = 74, metric_id = 1) {

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
  data <- jsonlite::fromJSON(resp, flatten = T)[[1]] |>
    purrr::compact() |>
    dplyr::as_tibble() |>
    dplyr::relocate(Data, .after = dplyr::last_col()) |>
    tidyr::unnest(col = Data)

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
cvd_indicator_data <- function(indicator_id = 2, time_period_id = 1, area_id = 2) {

  # compose the request
  req <-
    httr2::request(url_base) |>
    httr2::req_url_path_append(glue::glue('indicator/{indicator_id}/data')) |>
    httr2::req_url_query(
      `timePeriodID` = time_period_id,
      `areaID` = area_id
    )

  # perform the request
  resp <- req |>
    httr2::req_perform() |>
    httr2::resp_body_string()

  # wrangle for output
  data <- jsonlite::fromJSON(resp, flatten = T)[[2]] |>
    purrr::compact() |>
    dplyr::as_tibble() |>
    dplyr::relocate(Categories, .after = dplyr::last_col()) |>
    tidyr::unnest(col = Categories)
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
cvd_indicator_metric_data <- function(metric_id = 7, time_period_id = 1, area_id = 2) {

  # compose the request
  req <-
    httr2::request(url_base) |>
    httr2::req_url_path_append(glue::glue('indicator/metric/{metric_id}/data')) |>
    httr2::req_url_query(
      `timePeriodID` = time_period_id,
      `areaID` = area_id
    )

  # perform the request
  resp <- req |>
    httr2::req_perform() |>
    httr2::resp_body_string()

  # wrangle for output
  data <- jsonlite::fromJSON(resp, flatten = T)[[2]] |>
    purrr::compact() |>
    dplyr::as_tibble() |>
    dplyr::relocate(Categories, .after = dplyr::last_col()) |>
    tidyr::unnest(col = Categories)

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
cvd_indicator_raw_data <- function(indicator_id = 1, time_period_id = 1, system_level_id = 1) {

  # compose the request
  req <-
    httr2::request(url_base) |>
    httr2::req_url_path_append(glue::glue('indicator/{indicator_id}/rawDataJSON')) |>
    httr2::req_url_query(
      `timePeriodID` = time_period_id,
      `systemLevelID` = system_level_id
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

#' Indicator national vs area metric data
#'
#' Returns national and area data for provided metric, area and time period.
#' Target data contains the target value as a percentage stored as whole number
#' up to 100; target patients is the number of patients more needed to reach
#' the target percentage. If there is not data for both national and chosen
#' area an error will be returned.
#'
#' CVD Prevent API documentation:
#' [Indicator national vs area metric data](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Findicator%2FnationalVsAreaMetricData%2F%3Cmetric_ID%3E)
#'
#' @param metric_id integer - metric for which to return data (compulsory)
#' @param time_period_id integer - time period for which to return data (compulsory)
#' @param area_id integer - area for which to return data (compulsory)
#'
#' @return Tibble of performance against the specified metric in the area as compared with national level
#' @export
#' @seealso [cvd_indicator_list()], [cvd_indicator_metric_list()], [cvd_indicator()],
#' [cvd_indicator_tags()], [cvd_indicator_details()], [cvd_indicator_sibling()],
#' [cvd_indicator_child_data()], [cvd_indicator_data()], [cvd_indicator_metric_data()],
#' [cvd_indicator_raw_data()],
#' [cvd_indicator_priority_groups()], [cvd_indicator_pathway_group()], #
#' [cvd_indicator_group()], [cvd_indicator_metric_timeseries()],
#' [cvd_indicator_person_timeseries()], [cvd_indicator_metric_systemlevel_comparison()],
#' [cvd_indicator_metric_area_breakdown()]
#'
#' @examples
#' # Compare performance against metric 150  (AF: treatment with anticoagulants
#' # - all people) in 'Chester South PCN' (area ID 553) with national
#' # performance:
#' cvd_indicator_nationalarea_metric_data(metric_id = 150, time_period_id = 17, area_id = 553) |>
#' dplyr::slice_head(n=5)
cvd_indicator_nationalarea_metric_data <- function(metric_id = 1, time_period_id = 17, area_id = 739) {

  # compose the request
  req <-
    httr2::request(url_base) |>
    httr2::req_url_path_append(glue::glue('indicator/nationalVsAreaMetricData/{metric_id}')) |>
    httr2::req_url_query(
      `timePeriodID` = time_period_id,
      `areaID` = area_id
    )

  # perform the request
  resp <- req |>
    httr2::req_perform() |>
    httr2::resp_body_string()

  # wrangle for output
  data <- jsonlite::fromJSON(resp, flatten = T)[[2]] |>
    purrr::compact() |>
    dplyr::as_tibble() |>
    tidyr::unnest(cols = AreaData)

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
    httr2::req_url_path_append(glue::glue('indicator/priorityGroups'))

  # perform the request
  resp <- req |>
    httr2::req_perform() |>
    httr2::resp_body_string()

  # wrangle for output
  data <- jsonlite::fromJSON(resp, flatten = T)[[1]]
  data <- data |>
    dplyr::tibble() |>
    dplyr::mutate(PriorityGroup = names(data)) |>
    purrr::compact() |>
    tidyr::unnest(cols = data)
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
cvd_indicator_pathway_group <- function(pathway_group_id = 10) {

  # compose the request
  req <-
    httr2::request(url_base) |>
    httr2::req_url_path_append(glue::glue('indicator/pathwayGroup/{pathway_group_id}'))

  # perform the request
  resp <- req |>
    httr2::req_perform() |>
    httr2::resp_body_string()

  # wrangle for output
  data <- jsonlite::fromJSON(resp, flatten = T)[[1]] |>
    purrr::compact() |>
    dplyr::as_tibble() |>
    dplyr::relocate(Indicators, .after = dplyr::last_col()) |>
    tidyr::unnest(cols = Indicators)
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
cvd_indicator_group <- function(indicator_group_id = 15) {

  # compose the request
  req <-
    httr2::request(url_base) |>
    httr2::req_url_path_append(glue::glue('indicator/indicatorGroup/{indicator_group_id}'))

  # perform the request
  resp <- req |>
    httr2::req_perform() |>
    httr2::resp_body_string()

  # wrangle for output
  data <- jsonlite::fromJSON(resp, flatten = T)[[1]] |>
    purrr::compact() |>
    dplyr::as_tibble() |>
    dplyr::relocate(Indicators, .after = dplyr::last_col()) |>
    tidyr::unnest(cols = Indicators)
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
cvd_indicator_metric_timeseries <- function(metric_id = 1, area_id = 50) {

  # compose the request
  req <-
    httr2::request(url_base) |>
    httr2::req_url_path_append(glue::glue('indicator/timeSeriesByMetric/{metric_id}')) |>
    httr2::req_url_query(
      `areaID` = area_id
    )

  # perform the request
  resp <- req |>
    httr2::req_perform() |>
    httr2::resp_body_string()

  # wrangle for output
  data <- jsonlite::fromJSON(resp, flatten = T)[[1]] |>
    purrr::compact() |>
    dplyr::as_tibble() |>
    tidyr::unnest(cols = Areas) |>
    tidyr::unnest(cols = TimeSeriesData)

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
cvd_indicator_person_timeseries <- function(indicator_id = 1, area_id = 1) {

  # compose the request
  req <-
    httr2::request(url_base) |>
    httr2::req_url_path_append(glue::glue('indicator/personsTimeSeriesByIndicator/{indicator_id}')) |>
    httr2::req_url_query(
      `areaID` = area_id
    )

  # perform the request
  resp <- req |>
    httr2::req_perform() |>
    httr2::resp_body_string()

  # wrangle for output
  data <- jsonlite::fromJSON(resp, flatten = T)[[1]] |>
    purrr::compact() |>
    dplyr::as_tibble() |>
    tidyr::unnest(cols = InequalityMarkers) |>
    tidyr::unnest(cols = CategoryData)

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
cvd_indicator_metric_systemlevel_comparison <- function(metric_id = 1, time_period_id = 1, area_id = 50) {

  # compose the request
  req <-
    httr2::request(url_base) |>
    httr2::req_url_path_append(glue::glue('indicator/metricSystemLevelComparison/{metric_id}')) |>
    httr2::req_url_query(
      `timePeriodID` = time_period_id,
      `areaID` = area_id
    )

  # perform the request
  resp <- req |>
    httr2::req_perform() |>
    httr2::resp_body_string()

  # wrangle for output
  data <- jsonlite::fromJSON(resp, flatten = T)[[1]] |>
    purrr::compact() |>
    dplyr::as_tibble() |>
    tidyr::unnest(cols = SystemLevels) |>
    dplyr::relocate(ComparisonData, .after = dplyr::last_col()) |>
    tidyr::unnest(cols = ComparisonData)
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
cvd_indicator_metric_area_breakdown <- function(metric_id = 1, time_period_id = 1, area_id = 1) {

  # compose the request
  req <-
    httr2::request(url_base) |>
    httr2::req_url_path_append(glue::glue('indicator/metricAreaBreakdown/{metric_id}')) |>
    httr2::req_url_query(
      `timePeriodID` = time_period_id,
      `areaID` = area_id
    )

  # perform the request
  resp <- req |>
    httr2::req_perform() |>
    httr2::resp_body_string()

  # wrangle for output
  data <- jsonlite::fromJSON(resp, flatten = T)[[1]] |>
    purrr::compact() |>
    dplyr::as_tibble() |>
    tidyr::unnest(cols = SystemLevels) |>
    dplyr::relocate(ComparisonData, .after = dplyr::last_col()) |>
    tidyr::unnest(cols = ComparisonData)

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
    time_period_id = 1,
    system_level_id = 1,
    indicator_id, # optional
    metric_category_type_id # optional
) {

  # compose the request
  req <-
    httr2::request(url_base) |>
    httr2::req_url_path_append('dataAvailability') |>
    httr2::req_url_query(
      `timePeriodID` = time_period_id,
      `systemLevelID` = system_level_id
    )

  if(!base::missing(indicator_id) & !base::missing(metric_category_type_id)) {
    req <-
      req |>
      httr2::req_url_query(
        `indicatorID` = indicator_id,
        `metricCategoryTypeID` = metric_category_type_id
      )
  } else if(!base::missing(indicator_id)) {
    req <-
      req |>
      httr2::req_url_query(
        `indicatorID` = indicator_id
      )
  } else if(!base::missing(metric_category_type_id)) {
    req <-
      req |>
      httr2::req_url_query(
        `metricCategoryTypeID` = metric_category_type_id
      )
  }

  # perform the request
  resp <- req |>
    httr2::req_perform() |>
    httr2::resp_body_string()

  # wrangle for output
  data <- jsonlite::fromJSON(resp, flatten = T)[[1]] |>
    purrr::compact() |>
    dplyr::as_tibble()
}
