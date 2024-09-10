#' -----------------------------------------------------------------------------
#' CVD PREVENT API
#'
#' Using the API from the audit to gather data
#'
#' API documentation:
#' https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation
#' -----------------------------------------------------------------------------

# libraries
# .libPaths(new = "C:/Users/craig.parylo/AppData/Local/Programs/R/R-4.4.0/library")
# library(dplyr)
# library(tidyr)
# library(purrr)
# library(httr2)
# library(glue)
# library(jsonlite)
# library(stringr)

url_base <- 'https://api.cvdprevent.nhs.uk'



## time period -----------------------------------------------------------------
#' List time periods
#'
#' Returns all available time periods
#'
#' https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2FtimePeriod
#'
#' @param opt_indicator_type_id integer - Indicator type ID, e.g. standard or outcome indicator type. If passed will show time periods containing data of the given type (optional)
#'
#' @return Tibble of time period details
#' @export
#'
#' @examples
#' cvd_time_periods <- cvd_time_period_list()
cvd_time_period_list <- function(opt_indicator_type_id) {

  # compose the request
  req <-
    request(url_base) |>
    req_url_path_append('timePeriod')

  # perform the request
  resp <- req |>
    req_perform() |>
    resp_body_json()

  # wrangle to tibble for output
  return <- resp$timePeriodList |>
    map_dfr(
      .f = \(.period_item) {
        .period_item |>
          compact() |>
          as_tibble()
      }
    )
}


## area ------------------------------------------------------------------------

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
#' https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Farea
#'
#' @param time_period_id integer - specifies time period for which to return areas (compulsory)
#' @param parent_area_id integer - specifies the area of which children will be returned (optional)
#' @param system_level_id integer - specifies which system levels to return areas for (optional)
#'
#' @return Tibble of area details
#' @export
#'
#' @examples
#' areas <- cvd_area_list(time_period_id = 17, system_level_id = 5)
cvd_area_list <- function(time_period_id = 1, parent_area_id, system_level_id) {

  # compose the request
  req <-
    request(url_base) |>
    req_url_path_append('area')

  if(missing(parent_area_id) & missing(system_level_id)) {
    # both optional arguments are missing
    req <- req |>
      req_url_query(
        `timePeriodID` = time_period_id
      )
  } else if(missing(parent_area_id)) {
    # system level id provided
    req <- req |>
      req_url_query(
        `timePeriodID` = time_period_id,
        `systemLevelID` = system_level_id
      )
  } else if(missing(system_level_id)) {
    # parent area id provided
    req <- req |>
      req_url_query(
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
    req_perform() |>
    resp_body_json()

  # wrangle to tibble for output
  return <- resp$areaList |>
    map_dfr(
      .f = \(.area_item) {
        .area_item[!names(.area_item) %in% c('Parents')] |>
          compact() |>
          as_tibble()
      }
    )
}

#' Area details
#'
#' Returns details of a specific area at a given time period, including details about any parent and child areas.
#'
#' https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Farea%2F%3Carea_id%3E%2Fdetails
#'
#' @param time_period_id integer - specified time period for which to return details for, i.e. population and participation rate (compulsory)
#' @param area_id integer - specified area id for which to return details for.
#'
#' @return Named list of tibbles containing area 'area_details', child 'area_child_details' (where appropriate) and parent 'area_parent_details' (where appropriate)
#' @export
#'
#' @examples
#' area_details <- cvd_area_details(area_id = 1, time_period_id = 17)
cvd_area_details <- function(time_period_id = 1, area_id = 1) {

  # compose the request
  req <-
    request(url_base) |>
    req_url_path_append(glue('area/{area_id}/details')) |>
    req_url_query(
      `timePeriodID` = time_period_id
    )

  # perform the request
  resp <- req |>
    req_perform() |>
    resp_body_json()

  # wrangle to tibble for output
  data <- resp$areaDetails |>
    compact() |>
    as_tibble()

  # select the base fields, exclude any parent or child details
  area_details <- data |>
    select(-any_of(c('ChildAreaList', 'ParentAreaList'))) |>
    unique()

  return <- list(
    'area_details' = area_details
  )

  # extract any parent details
  if('ParentAreaList' %in% names(data)) {

    area_parent_details <- data$ParentAreaList |>
      map_dfr(
        .f = \(.parent) {
          .parent |>
            compact() |>
            as_tibble() |>
            unique()
        }
      )
    return <- return |> append(list('area_parent_details' = area_parent_details))
  }

  # extract any child details
  if('ChildAreaList' %in% names(data)) {

    area_child_details <- data$ChildAreaList |>
      map_dfr(
        .f = \(.child) {
          .child |>
            compact() |>
            as_tibble() |>
            unique()
        }
      )
    return <- return |> append(list('area_child_details' = area_child_details))
  }
}

#' Unassigned areas
#'
#' Returns a list of all areas which have data in the selected time period,
#' but do not have any parent areas assigned, and therefore are unreachable.
#'
#' https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Farea%2Funassigned
#'
#' @param time_period_id integer - time period for which Area must have data for (compulsory)
#' @param system_level_id integer - system level of areas in the unassigned list (optional)
#'
#' @return tibble
#' @export
#'
#' @examples
#' test <- cvd_area_unassigned(time_period_id = 1, system_level_id = 5)
#' test <- cvd_area_unassigned(time_period_id = 1)
cvd_area_unassigned <- function(time_period_id = 1, system_level_id) {

  # compose the request
  req <-
    request(url_base) |>
    req_url_path_append('area/unassigned')

  if(missing(system_level_id)) {
    # just use the time period
    req <- req |>
      req_url_query(
        `timePeriodID` = time_period_id
      )
  } else {
    req <- req |>
      req_url_query(
        `timePeriodID` = time_period_id,
        `systemLevelID` = system_level_id
      )
  }

  # perform the request
  resp <- req |>
    req_perform() |>
    resp_body_json()

  # wrangle to tibble for output
  data <- resp$unassignedAreaList |>
    map_dfr(
      .f = \(.area) {
        .area |>
          compact() |>
          as_tibble() |>
          unique()
      }
    )
}

#' Search for matching areas
#'
#' Returns a list of Areas that match a partial name for a given time period.
#' Uses simple LIKE '%<partial_area_name>%' comparison.
#'
#' https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Farea%2Fsearch
#'
#' @param partial_area_name string - string to use to search for an Area (compulsory)
#' @param time_period_id integer - limits the search to Areas which have data in specified time period (compulsory)
#'
#' @return Tibble of matching areas
#' @export
#'
#' @examples
#' test <- cvd_area_search(partial_area_name = 'Station', time_period_id = 17)
cvd_area_search <- function(partial_area_name = 'Surgery', time_period_id = 1) {

  # compose the request
  req <-
    request(url_base) |>
    req_url_path_append('area/search') |>
    req_url_query(
      `partialAreaName` = partial_area_name,
      `timePeriodID` = time_period_id
    )

  # perform the request
  resp <- req |>
    req_perform() |>
    resp_body_json()

  # wrangle to tibble for output
  return <- resp$foundAreaList |>
    map_dfr(
      .f = \(.area_item) {
        .area_item |>
          compact() |>
          as_tibble()
      }
    )
}

#' Area neested sub systems
#'
#' Returns given area and children areas in a nested structure
#'
#' https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Farea%2F%3Carea_id%3E%2FnestedSubSystems
#'
#' @param area_id integer - the area to return data for (compulsory)
#'
#' @return tibble of data
#' @export
#'
#' @examples
#' test <- cvd_area_nested_subsystems(area_id = 5)
cvd_area_nested_subsystems <- function(area_id = 5) {

  # compose the request
  req <-
    request(url_base) |>
    req_url_path_append(glue('area/{area_id}/nestedSubSystems'))

  # perform the request
  resp <- req |>
    req_perform() |>
    resp_body_string()

  # wrangle for output
  data <- fromJSON(resp, flatten = T)[[1]] |>
    compact() |>
    as_tibble() |>
    relocate(Children, .after = last_col()) |>
    unnest(cols = Children, names_sep = '_') |>
    relocate(Children_Children, .after = last_col()) |>
    unnest(cols = Children_Children, names_sep = '_') |>
    relocate(Children_Children_Children, .after = last_col()) |>
    unnest(cols = Children_Children_Children, names_sep = '_') #|>
  #relocate(Children_Children_Children_Children, .after = last_col()) |>
  #unnest(cols = Children_Children_Children_Children, names_sep = '_')
  # nb - the tibble seems to stop working after the last calls

}

#' Area flat subsystems
#'
#' Similar to `cvd_area_nested_subsystems()` but the sub-areas are grouped
#' based on their system level.
#'
#' https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Farea%2F%3Carea_id%3E%2FflatSubSystems
#'
#' @param area_id integer - the area to return data for (compulsory)
#'
#' @return tibble of data
#' @export
#'
#' @examples
#' test <- cvd_area_flat_subsystems(area_id = 5)
cvd_area_flat_subsystems <- function(area_id = 5) {

  # compose the request
  req <-
    request(url_base) |>
    req_url_path_append(glue('area/{area_id}/flatSubSystems'))

  # perform the request
  resp <- req |>
    req_perform() |>
    resp_body_string()

  # wrangle for output
  data <- fromJSON(resp, flatten = T)[[1]] |>
    compact() |>
    as_tibble() |>
    relocate(SubSystems, .after = last_col()) |>
    unnest(cols = SubSystems, names_sep = '_')
}

## indicators ------------------------------------------------------------------

#' List indicators
#'
#' Returns basic details of all indicators for a given system level and time period.
#' Only returns indicators for which data exists in selected time period, and on
#' selected system level. Used to populate available indicator list in Data Explorer.
#'
#' https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Findicator%2Flist
#'
#' @param time_period_id integer - time period to reutrn data for (compulsory)
#' @param system_level_id integer - system level to return data for (compulsory)
#'
#' @return tibble of time period details
#' @export
#'
#' @examples
#' cvd_indicators <- cvd_indicator_list()
cvd_indicator_list <- function(time_period_id = 1, system_level_id = 2) {

  # compose the request
  req <-
    request(url_base) |>
    req_url_path_append('indicator/list') |>
    req_url_query(
      `timePeriodID` = time_period_id,
      `systemLevelID` = system_level_id
    )

  # perform the request
  resp <- req |>
    req_perform() |>
    resp_body_json()

  # wrangle to tibble for output
  return <- resp$indicatorList |>
    map_dfr(
      .f = \(.indicator_item) {
        .indicator_item |>
          compact() |>
          as_tibble()
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
#' https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#*Proposed*%2Findicator%2FmetricList
#'
#' @param time_period_id integer - time period to return data for (compulsory)
#' @param system_level_id integer - system level to return data for (compulsory)
#'
#' @return tibble of indicators left-joined with metrics
#' @export
#'
#' @examples
#' test <- cvd_indicator_metric_list(time_period_id = 1, system_level_id = 1)
cvd_indicator_metric_list <- function(time_period_id = 1, system_level_id = 1) {

  # compose the request
  req <-
    request(url_base) |>
    req_url_path_append('indicator/metricList') |>
    req_url_query(
      `timePeriodID` = time_period_id,
      `systemLevelID` = system_level_id
    )

  # perform the request
  resp <- req |>
    req_perform() |>
    resp_body_json()

  # wrangle to tibble for output
  # 1. get the list of indicators
  indicators <- resp$indicatorList |>
    map_dfr(
      .f = \(.indicator_item) {
        .indicator_item |>
          compact() |>
          as_tibble()
      }
    ) |>
    unique()

  # 2. get the metrics
  metrics <- map2_dfr(
    .x = indicators$MetricList,
    .y = indicators$IndicatorID,
    .f = \(.metric_item, .indicator_id) {
      .metric_item |>
        compact() |>
        as_tibble() |>
        bind_cols(IndicatorID = .indicator_id)
    }
  )

  # 3. combine ready for output
  return <-
    indicators |>
    select(-MetricList) |>
    left_join(
      y = metrics,
      by = 'IndicatorID',
      relationship = 'many-to-many'
    )
}


#' Indicators
#'
#' Returns all indicators and data for a given time period and area. Also returns
#' time series data for all time periods available. If tags are specified, only
#' indicators which have one of the specified tags will be returned.
#'
#' https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Findicator
#'
#' @param time_period_id integer - time period to return data for (compulsory)
#' @param area_id integer - area to return data for (compulsory)
#' @param tag_id numeric vector - allows filtering indicators by one or more tags (optional, array)
#'
#' @return list of tibbles
#' @export
#'
#' @examples
#' test <- cvd_indicator()
#' test <- cvd_indicator(time_period_id = 17, area_id = 55, tag_id = c(12, 13))
cvd_indicator <- function(time_period_id = 1, area_id = 1, tag_id) {

  # compose the request
  if(missing(tag_id)) {
    req <- request(url_base) |>
      req_url_path_append('indicator') |>
      req_url_query(
        `timePeriodID` = time_period_id,
        `areaID` = area_id
      )
  } else {
    req <-
      request(url_base) |>
      req_url_path_append('indicator') |>
      req_url_query(
        `timePeriodID` = time_period_id,
        `areaID` = area_id,
        `tagID` = tag_id,
        .multi = 'explode'
      )
  }

  # perform the request
  resp <- req |>
    req_perform() |>
    resp_body_string()

  # wrangle for output
  # 1. get the data from the response
  df <- fromJSON(resp, flatten = T)
  df <- df[['indicatorList']] |>
    as_tibble()

  # 2. unnest the nested list data in 'Categories' and 'TimeSeries'
  df <- df |>
    # expand categories (and data)
    relocate(Categories, .after = last_col()) |>
    unnest(col = Categories, names_sep = '_') |>
    # expand the timeseries
    relocate(Categories_TimeSeries, .after = last_col()) |>
    unnest(col = Categories_TimeSeries, names_sep = '_')

  # 1. get indicator data
  # find out when the 'Categories_' columns begin
  index_categories <- grep(pattern = 'Categories_', colnames(df)) |> min()

  # select columns
  indicators <- df |>
    select(1:min(index_categories) - 1) |>
    unique()

  # 2. get categories
  categories <- df |>
    # select indicator ID and all category columns except Timeseries ones
    select(
      c(
        IndicatorID,
        starts_with('Categories_'),
        -contains('_TimeSeries'),
        -starts_with('.Data')
      )
    ) |>
    # remove 'Categories_' prefix
    rename_with(
      .fn = \(.col_name) {str_remove(.col_name, pattern = 'Categories_')},
      .cols = starts_with('Categories')
    ) |>
    unique()

  # 3. get categories_data
  category_data <- df |>
    select(
      c(
        IndicatorID,
        MetrricID = Categories_MetricID,
        starts_with('Categories_Data.')
      )
    ) |>
    # remove the 'Categories_Data.' prefix
    rename_with(
      .f = \(.col_name) {str_remove(.col_name, pattern = 'Categories_Data.')},
      .cols = starts_with('Categories_Data.')
    ) |>
    unique()

  # 4. get timeseries
  timeseries_data <- df |>
    select(
      c(
        IndicatorID,
        MetrricID = Categories_MetricID,
        starts_with('Categories_TimeSeries')
      )
    ) |>
    # remove the 'Categories_TimeSeries_' prefix
    rename_with(
      .f = \(.col_name) {str_remove(.col_name, pattern = 'Categories_TimeSeries_')},
      .cols = starts_with('Categories_TimeSeries_')
    ) |>
    unique()

  # output as a named list for ease of checking
  return <- list(
    #'raw' = resp,
    #'df' = df,
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
#' @return tibble of tags
#' @export
#'
#' @examples
#' test <- cvd_indicator_tags()
cvd_indicator_tags <- function() {

  # compose the request
  req <-
    request(url_base) |>
    req_url_path_append('indicator/tags')

  # perform the request
  resp <- req |>
    req_perform() |>
    resp_body_string()

  # wrangle to tibble for output
  data <- fromJSON(resp, flatten = T)$indicatorTagList |>
    as_tibble()
}

#' Indicator details
#'
#' Returns details of a single indicator
#'
#' @param indicator_id integer - the ID for the indicator (compulsory)
#'
#' @return tibble
#' @export
#'
#' @examples
#' test <- cvd_indicator_details()
#' test <- cvd_indicator_details(indicator_id = 7)
cvd_indicator_details <- function(indicator_id = 1) {

  # compose the request
  req <-
    request(url_base) |>
    req_url_path_append(glue('indicator/{indicator_id}/details'))

  # perform the request
  resp <- req |>
    req_perform() |>
    resp_body_string()

  # wrangle for output
  data <- fromJSON(resp, flatten = T)$indicatorDetails |>
    compact() |>
    as_tibble() |>
    relocate(MetaData, .after = last_col()) |>
    unnest(col = MetaData)

}

#' Indicator sibling data
#'
#' Returns all sibling areas and their data for specified time period, area
#' and metric. This endpoint is intended to only return data for selected metric,
#' and not all metrics for a chosen indicator, hence the metric_id query parameter.
#'
#' https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Findicator%2FsiblingData
#'
#' @param time_period_id integer - time period for which to return data (compulsory)
#' @param area_id integer - area for which all sibling data will be returned (compulsory)
#' @param metric_id integer - metric for which to return data (compulsory)
#'
#' @return
#' @export
#'
#' @examples
#' est <- cvd_indicator_sibling()
cvd_indicator_sibling <- function(time_period_id = 17, area_id = 30, metric_id = 1) {

  # compose the request
  req <-
    request(url_base) |>
    req_url_path_append('indicator/siblingData') |>
    req_url_query(
      `timePeriodID` = time_period_id,
      `areaID` = area_id,
      `metricID` = metric_id
    )

  # perform the request
  resp <- req |>
    req_perform() |>
    resp_body_string()

  # wrangle for output
  data <- fromJSON(resp, flatten = T)[[2]] |>
    compact() |>
    as_tibble() |>
    relocate(Data, .after = last_col()) |>
    unnest(col = Data)

}

#' Indicator child data
#'
#' Returns all children areas and their data for specified time period, area
#' and metric. This endpoint is intended to only return data for selected
#' metric, and not all metrics for indicators, hence the metricID query
#' parameter.
#'
#' https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Findicator%2FchildData
#'
#' @param time_period_id integer - time period for which to return data (compulsory)
#' @param area_id integer - area for which all children data will be returned (compulsory)
#' @param metric_id integer - metric for which to return data (compulsory)
#'
#' @return tibble of child indicator data
#' @export
#'
#' @examples
#' test <- cvd_indicator_child_data()
cvd_indicator_child_data <- function(time_period_id = 17, area_id = 74, metric_id = 1) {

  # compose the request
  req <-
    request(url_base) |>
    req_url_path_append('indicator/childData') |>
    req_url_query(
      `timePeriodID` = time_period_id,
      `areaID` = area_id,
      `metricID` = metric_id
    )

  # perform the request
  resp <- req |>
    req_perform() |>
    resp_body_string()

  # wrangle for output
  data <- fromJSON(resp, flatten = T)[[1]] |>
    compact() |>
    as_tibble() |>
    relocate(Data, .after = last_col()) |>
    unnest(col = Data)

}

#' Indicator data
#'
#' Returns all metric data for a specified indicator. Data will include values
#' for both selected area, and organisation at National Level (usually England).
#'
#' https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Findicator%2F%3Cindicator_id%3E%2Fdata
#'
#' @param indicator_id integer - indicator for which to return data (compulsory)
#' @param time_period_id integer - time period for which to return data for (compulsory)
#' @param area_id integer - area for which to return data for (compulsory)
#'
#' @return tibble of indicator data
#' @export
#'
#' @examples
#' test <- cvd_indicator_data()
cvd_indicator_data <- function(indicator_id = 2, time_period_id = 1, area_id = 2) {

  # compose the request
  req <-
    request(url_base) |>
    req_url_path_append(glue('indicator/{indicator_id}/data')) |>
    req_url_query(
      `timePeriodID` = time_period_id,
      `areaID` = area_id
    )

  # perform the request
  resp <- req |>
    req_perform() |>
    resp_body_string()

  # wrangle for output
  data <- fromJSON(resp, flatten = T)[[2]] |>
    compact() |>
    as_tibble() |>
    relocate(Categories, .after = last_col()) |>
    unnest(col = Categories)
}


#' Metric data
#'
#' Returns all metric data for a specified metric. Data will include values
#' for both selected area and organisation at National Level (usually England).
#'
#' https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#*Proposed*%2Findicator%2Fmetric%2F%3Cmetric_id%3E%2Fdata
#'
#' @param metric_id integer - metric for which to return data for (compulsory)
#' @param time_period_id integer - time period for which to return data for (compulsory)
#' @param area_id integer - area for which to return data for (compulsory)
#'
#' @return tibble of metric data
#' @export
#'
#' @examples
cvd_indicator_metric_data <- function(metric_id = 7, time_period_id = 1, area_id = 2) {

  # compose the request
  req <-
    request(url_base) |>
    req_url_path_append(glue('indicator/metric/{metric_id}/data')) |>
    req_url_query(
      `timePeriodID` = time_period_id,
      `areaID` = area_id
    )

  # perform the request
  resp <- req |>
    req_perform() |>
    resp_body_string()

  # wrangle for output
  data <- fromJSON(resp, flatten = T)[[2]] |>
    compact() |>
    as_tibble() |>
    relocate(Categories, .after = last_col()) |>
    unnest(col = Categories)

}

#' Indicator raw data (JSON)
#'
#' Returns all metric data for a specified indicator, system level and time
#' period.
#'
#' https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Findicator%2F%3Cindicator_ID%3E%2FrawDataJSON
#'
#' @param indicator_id integer - indicator for which to return data for (compulsory)
#' @param time_period_id integer - time period for which to return data for (compulsory)
#' @param system_level_id integer - system level for which to return data for (compulsory)
#'
#' @return tibble of indicator data
#' @export
#'
#' @examples
#' test <- cvd_indicator_raw_data()
cvd_indicator_raw_data <- function(indicator_id = 1, time_period_id = 1, system_level_id = 1) {

  # compose the request
  req <-
    request(url_base) |>
    req_url_path_append(glue('indicator/{indicator_id}/rawDataJSON')) |>
    req_url_query(
      `timePeriodID` = time_period_id,
      `systemLevelID` = system_level_id
    )

  # perform the request
  resp <- req |>
    req_perform() |>
    resp_body_string()

  # wrangle for output
  data <- fromJSON(resp, flatten = T)[[2]] |>
    compact() |>
    as_tibble()

}

#' Indicator national vs area metric data
#'
#' Returns national and area data for provided metric, area and time period.
#' Target data contains the target value as a percentage stored as whole number
#' up to 100; target patients is the number of patients more needed to reach
#' the target percentage. If there is not data for both national and chosen
#' area an error will be returned.
#'
#' https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Findicator%2FnationalVsAreaMetricData%2F%3Cmetric_ID%3E
#'
#' @param metric_id integer - metric for which to return data (compulsory)
#' @param time_period_id integer - time period for which to return data (compulsory)
#' @param area_id integer - area for which to return data (compulsory)
#'
#' @return tibble of data
#' @export
#'
#' @examples
cvd_indicator_nationalarea_metric_data <- function(metric_id = 1, time_period_id = 17, area_id = 739) {

  # compose the request
  req <-
    request(url_base) |>
    req_url_path_append(glue('indicator/nationalVsAreaMetricData/{metric_id}')) |>
    req_url_query(
      `timePeriodID` = time_period_id,
      `areaID` = area_id
    )

  # perform the request
  resp <- req |>
    req_perform() |>
    resp_body_string()

  # wrangle for output
  data <- fromJSON(resp, flatten = T)[[2]] |>
    compact() |>
    as_tibble() |>
    unnest(cols = AreaData)

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
#' https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Findicator%2FpriorityGroups
#'
#' @return
#' @export
#'
#' @examples
#' test <- cvd_indicator_priority_groups()
cvd_indicator_priority_groups <- function() {

  # compose the request
  req <-
    request(url_base) |>
    req_url_path_append(glue('indicator/priorityGroups'))

  # perform the request
  resp <- req |>
    req_perform() |>
    resp_body_string()

  # wrangle for output
  data <- fromJSON(resp, flatten = T)[[1]] |>
    map_dfr(
      .f = \(.group_item) {
        .group_item |>
          compact() |>
          as_tibble()
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
#' https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Findicator%2FpathwayGroup%2F%3Cpathway_group_id%3E
#'
#'
#' @param pathway_group_id integer - the pathway to return data for (compulsory)
#'
#' @return tibble of data
#' @export
#'
#' @examples
#' test <- cvd_indicator_pathway_group()
cvd_indicator_pathway_group <- function(pathway_group_id = 10) {

  # compose the request
  req <-
    request(url_base) |>
    req_url_path_append(glue('indicator/pathwayGroup/{pathway_group_id}'))

  # perform the request
  resp <- req |>
    req_perform() |>
    resp_body_string()

  # wrangle for output
  data <- fromJSON(resp, flatten = T)[[1]] |>
    compact() |>
    as_tibble() |>
    relocate(Indicators, .after = last_col()) |>
    unnest(cols = Indicators)
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
#' https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Findicator%2FindicatorGroup%2F%3Cindicator_group_ID%3E
#'
#'
#' @param indicator_group_id integer - the group to return data for (compulsory)
#'
#' @return
#' @export
#'
#' @examples
cvd_indicator_group <- function(indicator_group_id = 15) {

  # compose the request
  req <-
    request(url_base) |>
    req_url_path_append(glue('indicator/indicatorGroup/{indicator_group_id}'))

  # perform the request
  resp <- req |>
    req_perform() |>
    resp_body_string()

  # wrangle for output
  data <- fromJSON(resp, flatten = T)[[1]] |>
    compact() |>
    as_tibble() |>
    relocate(Indicators, .after = last_col()) |>
    unnest(cols = Indicators)
}

#' Indicator time series by metric
#'
#' Returns data for the time series chart for specified metric ID and area ID.
#' Contains an array of two areas in `Areas`, one of which is the National data
#' with the other corresponding to the provided area ID. `TargetValue` is also
#' returned in the `Data` dictionary.
#'
#' https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Findicator%2FtimeSeriesByMetric%2F%3Cmetric_ID%3E
#'
#' @param metric_id integer - the metric to return data for (compulsory)
#' @param area_id integer - the area to return data for (compulsory)
#'
#' @return tibble
#' @export
#'
#' @examples
#' test <- cvd_indicator_metric_timeseries()
cvd_indicator_metric_timeseries <- function(metric_id = 1, area_id = 50) {

  # compose the request
  req <-
    request(url_base) |>
    req_url_path_append(glue('indicator/timeSeriesByMetric/{metric_id}')) |>
    req_url_query(
      `areaID` = area_id
    )

  # perform the request
  resp <- req |>
    req_perform() |>
    resp_body_string()

  # wrangle for output
  data <- fromJSON(resp, flatten = T)[[1]] |>
    compact() |>
    as_tibble() |>
    unnest(cols = Areas) |>
    unnest(cols = TimeSeriesData)

}

#' Indicator persons time series by indicator
#'
#' Returns data for the Inequalities Markers Time Series chart for the provided
#' indicator ID and area ID. `Data` contains information about the chosen
#' target value as well as an array `InequalityMarkers` which contains all the
#' time series data grouped into metric category types e.g. age group,
#' ethnicity, etc.
#'
#' https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Findicator%2FpersonsTimeSeriesByIndicator%2F%3Cindicator_ID%3E
#'
#' @param indicator_id
#' @param area_id
#'
#' @return
#' @export
#'
#' @examples
#' test <- cvd_indicator_person_timeseries()
cvd_indicator_person_timeseries <- function(indicator_id = 1, area_id = 1) {

  # compose the request
  req <-
    request(url_base) |>
    req_url_path_append(glue('indicator/personsTimeSeriesByIndicator/{indicator_id}')) |>
    req_url_query(
      `areaID` = area_id
    )

  # perform the request
  resp <- req |>
    req_perform() |>
    resp_body_string()

  # wrangle for output
  data <- fromJSON(resp, flatten = T)[[1]] |>
    compact() |>
    as_tibble() |>
    unnest(cols = InequalityMarkers) |>
    unnest(cols = CategoryData)

}

#' Indicator metric system level comparison
#'
#' Returns data for the SystemLevel Comparison chart for provided metric, area
#' and time period. `Data` contains the target value as well as an array
#' `SystemLevels` which contains data grouped by system level.
#'
#' https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Findicator%2FmetricSystemLevelComparison%2F%3Cmetric_ID%3E
#'
#'
#' @param metric_id integer - the metric to return data for (compulsory)
#' @param time_period_id integer - the time period to return data for (compulsory)
#' @param area_id integer - the area to return data for (compulsory)
#'
#' @return tibble of data
#' @export
#'
#' @examples
#' test <- cvd_indicator_metric_systemlevel_comparison()
cvd_indicator_metric_systemlevel_comparison <- function(metric_id = 1, time_period_id = 1, area_id = 50) {

  # compose the request
  req <-
    request(url_base) |>
    req_url_path_append(glue('indicator/metricSystemLevelComparison/{metric_id}')) |>
    req_url_query(
      `timePeriodID` = time_period_id,
      `areaID` = area_id
    )

  # perform the request
  resp <- req |>
    req_perform() |>
    resp_body_string()

  # wrangle for output
  data <- fromJSON(resp, flatten = T)[[1]] |>
    compact() |>
    as_tibble() |>
    unnest(cols = SystemLevels) |>
    relocate(ComparisonData, .after = last_col()) |>
    unnest(cols = ComparisonData)
}

#' Indicator metric area breakdown
#'
#' Returns data for the Area Breakdown chart for provided metric, area and time
#' period. `Data` contains the target value as well as an array `SystemLevels`
#' which contains data grouped by system level.
#'
#' https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Findicator%2FmetricAreaBreakdown%2F%3Cmetric_ID%3E
#'
#'
#' @param metric_id integer - the metric to return data for (compulsory)
#' @param time_period_id integer - the time period to return data for (compulsory)
#' @param area_id integer - the area to return data for (compulsory)
#'
#' @return tibble of data
#' @export
#'
#' @examples
cvd_indicator_metric_area_breakdown <- function(metric_id = 1, time_period_id = 1, area_id = 1) {

  # compose the request
  req <-
    request(url_base) |>
    req_url_path_append(glue('indicator/metricAreaBreakdown/{metric_id}')) |>
    req_url_query(
      `timePeriodID` = time_period_id,
      `areaID` = area_id
    )

  # perform the request
  resp <- req |>
    req_perform() |>
    resp_body_string()

  # wrangle for output
  data <- fromJSON(resp, flatten = T)[[1]] |>
    compact() |>
    as_tibble() |>
    unnest(cols = SystemLevels) |>
    relocate(ComparisonData, .after = last_col()) |>
    unnest(cols = ComparisonData)

}

## external resource -----------------------------------------------------------
#' External resource
#'
#' Returns a list of all external resources
#'
#' https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2FexternalResource
#'
#' @return
#' @export
#'
#' @examples
cvd_external_resource <- function() {

  # compose the request
  req <-
    request(url_base) |>
    req_url_path_append('externalResource')

  # perform the request
  resp <- req |>
    req_perform() |>
    resp_body_string()

  # wrangle for output
  data <- fromJSON(resp, flatten = T)[[2]] |>
    compact() |>
    as_tibble()
}

#' Data availability
#'
#' Returns the data availability.
#' Response:
#' `DataAvailabilityID` - ID of the resource as found in the database
#' `DataAvailabilityName` - explanation for the data availability
#' `IsAvailable` - `Y` for data is available, `N` for data is unavailable, and NULL for unknown data
#'
#' https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2FdataAvailability
#'
#' @param time_period_id integer - the time period to return data for (compulsory)
#' @param system_level_id integer - the system level to return data for (compulsory)
#' @param indicator_id integer - the indicator to return data for (optional)
#' @param metric_category_type_id integer - the metric category to return data for (optional)
#'
#' @return
#' @export
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
    request(url_base) |>
    req_url_path_append('dataAvailability') |>
    req_url_query(
      `timePeriodID` = time_period_id,
      `systemLevelID` = system_level_id
    )

  if(!missing(indicator_id) & !missing(metric_category_type_id)) {
    req <-
      req |>
      req_url_query(
        `indicatorID` = indicator_id,
        `metricCategoryTypeID` = metric_category_type_id
      )
  } else if(!missing(indicator_id)) {
    req <-
      req |>
      req_url_query(
        `indicatorID` = indicator_id
      )
  } else if(!missing(metric_category_type_id)) {
    req <-
      req |>
      req_url_query(
        `metricCategoryTypeID` = metric_category_type_id
      )
  }

  # perform the request
  resp <- req |>
    req_perform() |>
    resp_body_string()

  # wrangle for output
  data <- fromJSON(resp, flatten = T)[[1]] |>
    compact() |>
    as_tibble()
}


