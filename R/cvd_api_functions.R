# setup ------------------------------------------------------------------------
url_base <- 'https://api.cvdprevent.nhs.uk'

globalVariables(
  c(
    'AreaData', 'Areas', 'Categories', 'Categories_MetricID', 'Categories_TimeSeries',
    'CategoryData', 'Children', 'Children_Children', 'Children_Children_Children',
    'ComparisonData', 'Data', 'IndicatorID', 'Indicators', 'InequalityMarkers', 'MetaData',
    'MetricList', 'SubSystems', 'SystemLevels', 'TimeSeriesData'
  )
)

## time period -----------------------------------------------------------------
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
#' @seealso [cvd_time_period_system_levels()]
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
    httr2::resp_body_json()

  # wrangle to tibble for output
  return <- resp$timePeriodList |>
    purrr::map_dfr(
      .f = \(.period_item) {
        .period_item |>
          purrr::compact() |>
          dplyr::as_tibble()
      }
    )
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
    dplyr::relocate(SystemLevels, .after = dplyr::last_col()) |>
    tidyr::unnest(cols = SystemLevels)

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
    tidyr::unnest(TimePeriods)
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
    httr2::resp_body_json()

  # wrangle to tibble for output
  return <- resp$areaList |>
    purrr::map_dfr(
      .f = \(.area_item) {
        .area_item[!names(.area_item) %in% c('Parents')] |>
          purrr::compact() |>
          dplyr::as_tibble()
      }
    )
}

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
    httr2::resp_body_json()

  # wrangle to tibble for output
  data <- resp$areaDetails |>
    purrr::compact() |>
    dplyr::as_tibble()

  # select the base fields, exclude any parent or child details
  area_details <- data |>
    dplyr::select(-dplyr::any_of(c('ChildAreaList', 'ParentAreaList'))) |>
    unique()

  return <- list(
    'area_details' = area_details
  )

  # extract any parent details
  if('ParentAreaList' %in% names(data)) {

    area_parent_details <- data$ParentAreaList |>
      purrr::map_dfr(
        .f = \(.parent) {
          .parent |>
            purrr::compact() |>
            dplyr::as_tibble() |>
            unique()
        }
      ) |>
      unique()
    return <- return |> append(list('area_parent_details' = area_parent_details))
  }

  # extract any child details
  if('ChildAreaList' %in% names(data)) {

    area_child_details <- data$ChildAreaList |>
      purrr::map_dfr(
        .f = \(.child) {
          .child |>
            purrr::compact() |>
            dplyr::as_tibble() |>
            unique()
        }
      ) |>
      unique()
    return <- return |> append(list('area_child_details' = area_child_details))
  }
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
#' @return tibble
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
    httr2::resp_body_json()

  # wrangle to tibble for output
  data <- resp$unassignedAreaList |>
    purrr::map_dfr(
      .f = \(.area) {
        .area |>
          purrr::compact() |>
          dplyr::as_tibble() |>
          unique()
      }
    )
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
#' @return Tibble of matching areas
#' @export
#' @seealso [cvd_area_list()], [cvd_area_details()], [cvd_area_unassigned()], [cvd_area_nested_subsystems()], [cvd_area_flat_subsystems()]
#'
#' @examples
#' test <- cvd_area_search(partial_area_name = 'Station', time_period_id = 17)
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
    httr2::resp_body_json()

  # wrangle to tibble for output
  return <- resp$foundAreaList |>
    purrr::map_dfr(
      .f = \(.area_item) {
        .area_item |>
          purrr::compact() |>
          dplyr::as_tibble()
      }
    )
}

#' Area neested sub systems
#'
#' Returns given area and children areas in a nested structure
#'
#' CVD Prevent API documentation:
#' [Area nested subsystems](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Farea%2F%3Carea_id%3E%2FnestedSubSystems)
#'
#' @param area_id integer - the area to return data for (compulsory)
#'
#' @return tibble of data
#' @export
#' @seealso [cvd_area_list()], [cvd_area_details()], [cvd_area_unassigned()], [cvd_area_search()], [cvd_area_flat_subsystems()]
#'
#' @examples
#' test <- cvd_area_nested_subsystems(area_id = 5)
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
    dplyr::as_tibble() |>
    dplyr::relocate(Children, .after = dplyr::last_col()) |>
    tidyr::unnest(cols = Children, names_sep = '_') |>
    dplyr::relocate(Children_Children, .after = dplyr::last_col()) |>
    tidyr::unnest(cols = Children_Children, names_sep = '_') |>
    dplyr::relocate(Children_Children_Children, .after = dplyr::last_col()) |>
    tidyr::unnest(cols = Children_Children_Children, names_sep = '_')

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
#' @return tibble of data
#' @export
#' @seealso [cvd_area_list()], [cvd_area_details()], [cvd_area_unassigned()], [cvd_area_search()], [cvd_area_nested_subsystems()]
#'
#' @examples
#' test <- cvd_area_flat_subsystems(area_id = 5)
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
  data <- jsonlite::fromJSON(resp, flatten = T)[[1]] |>
    purrr::compact() |>
    dplyr::as_tibble() |>
    dplyr::relocate(SubSystems, .after = dplyr::last_col()) |>
    tidyr::unnest(cols = SubSystems, names_sep = '_')
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
#' @return tibble of time period details
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
#' cvd_indicators <- cvd_indicator_list()
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
    httr2::resp_body_json()

  # wrangle to tibble for output
  return <- resp$indicatorList |>
    purrr::map_dfr(
      .f = \(.indicator_item) {
        .indicator_item |>
          purrr::compact() |>
          dplyr::as_tibble()
      }
    )
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
#' @return tibble of indicators left-joined with metrics
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
#' test <- cvd_indicator_metric_list(time_period_id = 1, system_level_id = 1)
#' test <- cvd_indicator_metric_list(time_period_id = 17, system_level_id = 2)
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

  # wrangle for output
  data <- jsonlite::fromJSON(resp, flatten = T)[[2]] |>
    purrr::compact() |>
    dplyr::as_tibble()

  if('MetricList' %in% names(data)) {
    data <-
      data |>
      dplyr::relocate(dplyr::any_of(c('MetricList')), .after = dplyr::last_col()) |>
      tidyr::unnest(cols = dplyr::any_of(c('MetricList')))
  }

  if(length(data) == 0) {
    cli::cli_alert(
      text = 'No results returned'
    )
  }

  return(data)
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
#' @return list of tibbles
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
#' test <- cvd_indicator()
#' test <- cvd_indicator(time_period_id = 17, area_id = 55, tag_id = c(12, 13))
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
  # 1. get the data from the response
  df <- jsonlite::fromJSON(resp, flatten = T)
  df <- df[['indicatorList']] |>
    dplyr::as_tibble()

  # 2. unnest the nested list data in 'Categories' and 'TimeSeries'
  df <- df |>
    # expand categories (and data)
    dplyr::relocate(Categories, .after = dplyr::last_col()) |>
    tidyr::unnest(col = Categories, names_sep = '_') |>
    # expand the timeseries
    dplyr::relocate(Categories_TimeSeries, .after = dplyr::last_col()) |>
    tidyr::unnest(col = Categories_TimeSeries, names_sep = '_')

  # 1. get indicator data
  # find out when the 'Categories_' columns begin
  index_categories <- grep(pattern = 'Categories_', colnames(df)) |> min()

  # select columns
  indicators <- df |>
    dplyr::select(1:min(index_categories) - 1) |>
    unique()

  # 2. get categories
  categories <- df |>
    # select indicator ID and all category columns except Timeseries ones
    dplyr::select(
      c(
        IndicatorID,
        dplyr::starts_with('Categories_'),
        -dplyr::contains('_TimeSeries'),
        -dplyr::starts_with('.Data')
      )
    ) |>
    # remove 'Categories_' prefix
    dplyr::rename_with(
      .fn = \(.col_name) {stringr::str_remove(.col_name, pattern = 'Categories_')},
      .cols = dplyr::starts_with('Categories')
    ) |>
    unique()

  # 3. get categories_data
  category_data <- df |>
    dplyr::select(
      c(
        IndicatorID,
        MetrricID = Categories_MetricID,
        dplyr::starts_with('Categories_Data.')
      )
    ) |>
    # remove the 'Categories_Data.' prefix
    dplyr::rename_with(
      .f = \(.col_name) {stringr::str_remove(.col_name, pattern = 'Categories_Data.')},
      .cols = dplyr::starts_with('Categories_Data.')
    ) |>
    unique()

  # 4. get timeseries
  timeseries_data <- df |>
    dplyr::select(
      c(
        IndicatorID,
        MetrricID = Categories_MetricID,
        dplyr::starts_with('Categories_TimeSeries')
      )
    ) |>
    # remove the 'Categories_TimeSeries_' prefix
    dplyr::rename_with(
      .f = \(.col_name) {stringr::str_remove(.col_name, pattern = 'Categories_TimeSeries_')},
      .cols = dplyr::starts_with('Categories_TimeSeries_')
    ) |>
    unique()

  # output as a named list for ease of checking
  return <- list(
    'indicators' = indicators,
    'categories' = categories,
    'category_data' = category_data,
    'timeseries_data' = timeseries_data
  )
}

#' Indicator tags
#'
#' Returns a list of all available tags, which can be used to filter indicators.
#'
#' CVD Prevent API documentation:
#' [Indicator tags](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Findicator%2Ftags)
#'
#' @return tibble of tags
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
#' test <- cvd_indicator_tags()
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
  data <- jsonlite::fromJSON(resp, flatten = T)$indicatorTagList |>
    dplyr::as_tibble()
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
#' @return tibble
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
#' test <- cvd_indicator_details()
#' test <- cvd_indicator_details(indicator_id = 7)
cvd_indicator_details <- function(indicator_id = 1) {

  # compose the request
  req <-
    httr2::request(url_base) |>
    httr2::req_url_path_append(glue::glue('indicator/{indicator_id}/details'))

  # perform the request
  resp <- req |>
    httr2::req_perform() |>
    httr2::resp_body_string()

  # wrangle for output
  data <- jsonlite::fromJSON(resp, flatten = T)$indicatorDetails |>
    purrr::compact() |>
    dplyr::as_tibble() |>
    dplyr::relocate(MetaData, .after = dplyr::last_col()) |>
    tidyr::unnest(col = MetaData)

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
#' @return tibble of data
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
#' est <- cvd_indicator_sibling()
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
#' @return tibble of child indicator data
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
#' test <- cvd_indicator_child_data()
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
#' @return tibble of indicator data
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
#' test <- cvd_indicator_data()
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
#' @return tibble of metric data
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
#' test <- cvd_indicator_metric_data()
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
#' @return tibble of indicator data
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
#' test <- cvd_indicator_raw_data()
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
#' @return tibble of data
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
#' test <- cvd_indicator_nationalarea_metric_data()
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
#' indicates the order in whcih it should be displayed for the given Priority
#' Group.
#'
#' CVD Prevent API documentation:
#' [Indicator priority groups](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Findicator%2FpriorityGroups)
#'
#' @return tibble of data
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
#' test <- cvd_indicator_priority_groups()
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
  data <- jsonlite::fromJSON(resp, flatten = T)[[1]] |>
    purrr::map_dfr(
      .f = \(.group_item) {
        .group_item |>
          purrr::compact() |>
          dplyr::as_tibble()
      }
    )

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
#' @return tibble of data
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
#' test <- cvd_indicator_pathway_group()
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
#' @return tibble of data
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
#' test <- cvd_indicator_group()
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
#' @return tibble
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
#' test <- cvd_indicator_metric_timeseries()
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
#' @return tibble of data
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
#' test <- cvd_indicator_person_timeseries()
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
#' @return tibble of data
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
#' test <- cvd_indicator_metric_systemlevel_comparison()
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
#' @return tibble of data
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
#' test <- cvd_indicator_metric_area_breakdown()
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
#' @return tibble fo data
#' @export
#' @seealso [cvd_data_availability()]
#'
#' @examples
#' test <- cvd_external_resource()
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
#' @return tibble of data
#' @export
#' @seealso [cvd_external_resource()]
#'
#' @examples
#' test <- cvd_data_availability(time_period_id = 3, system_level_id = 5)
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
