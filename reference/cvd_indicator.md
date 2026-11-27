# Retrieve all indicators and their data for a given time period and area

Returns all CVD indicators and related data for a specified reporting
period (`time_period_id`) and NHS area (`area_id`) from the CVDPREVENT
API. Also retrieves time series data for all available periods.
Optionally, you can filter results by one or more indicator tags.

The returned object is a named list of tibbles, including details about
indicators, metric categories, metric data and time series, making this
function ideal for comprehensive data extraction and downstream
analysis.

## Usage

``` r
cvd_indicator(time_period_id, area_id, tag_id = NULL)
```

## Arguments

- time_period_id:

  Integer (required). The reporting period (time period) for which to
  return indicator data. Use the
  [`cvd_time_period_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_time_period_list.md)
  to find valid IDs.

- area_id:

  Integer (required). The AreaID for which to return indicator data. Use
  [`cvd_area_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_list.md)
  or
  [`cvd_area_search()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_search.md)
  to find valid IDs.

- tag_id:

  Numeric vector (optional). One or more tag IDs to filter indicators by
  tag. Use
  [`cvd_indicator_tags()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_tags.md)
  to find valid IDs.

## Value

A named list containing up to four tibbles:

- indicators:

  Tibble of indicators for the area and time period.

- metric_categories:

  Tibble of metric categories related to the indicators.

- metric_data:

  Tibble of metric values for the area and indicators.

- timeseries_data:

  Tibble of time series data for metrics and indicators across time
  periods.

If no indicators are found, returns a tibble describing the error.

**indicators** contains the following columns:

- AxisCharacter:

  Character. Symbol used to represent the metric axis (e.g., "%").

- DataUpdateInterval:

  Character. Frequency or interval at which the indicator data is
  updated. Often blank.

- FormatDisplayName:

  Character. Display format for the metric (e.g., "Proportion %").

- HighestPriorityNotificationType:

  Character. Notification priority level, if applicable (e.g., "Red").
  Often blank.

- IndicatorCode:

  Character. Unique code for the indicator (e.g., "CVDP009CHOL").

- IndicatorFormatID:

  Integer. Internal ID for the indicator's format type.

- IndicatorID:

  Integer. Unique identifier for the indicator.

- IndicatorName:

  Character. Full descriptive name of the indicator.

- IndicatorOrder:

  Integer. Display order for the indicator in dashboards or reports.

- IndicatorShortName:

  Character. Abbreviated name of the indicator for display purposes.

- IndicatorStatus:

  Character. Status of the indicator (e.g., active, discontinued). Often
  blank.

- IndicatorTypeID:

  Integer. Unique identifier for the indicator type (e.g., 1 =
  Standard).

- IndicatorTypeName:

  Character. Name of the indicator type (e.g., "Standard").

- NotificationCount:

  Integer. Count of notifications associated with the indicator.

**metric_categories** contains the following columns:

- IndicatorID:

  Integer. Unique identifier for the indicator. Links to the
  corresponding entry in the indicators table.

- CategoryAttribute:

  Character. Grouping label used to define the population subset (e.g.,
  "Male", "Persons").

- MetricCategoryID:

  Integer. Unique identifier for the metric category.

- MetricCategoryName:

  Character. Name of the subgroup or category (e.g., "80+", "Mixed",
  "Female").

- MetricCategoryOrder:

  Integer. Display order for the category within its type.

- MetricCategoryTypeName:

  Character. Type of category used for breakdown (e.g., "Age group",
  "Sex", "Ethnicity").

- MetricID:

  Integer. Unique identifier for the specific metric instance.

**metric_data** contains the following columns:

- MetricID:

  Integer. Unique identifier for the metric instance. Links to the
  corresponding entry in the metric categories table.

- AreaID:

  Integer. Unique identifier for the NHS area.

- Count:

  Integer. Number of records included in the calculation.

- DataID:

  Integer. Unique identifier for the data point.

- Denominator:

  Numeric. Denominator used in the metric calculation.

- Factor:

  Numeric. Scaling factor applied to the metric, if applicable. Often
  blank.

- LowerConfidenceLimit:

  Numeric. Lower bound of the confidence interval.

- Max:

  Numeric. Maximum observed value for the metric.

- Median:

  Numeric. Median value for the metric.

- Min:

  Numeric. Minimum observed value for the metric.

- Numerator:

  Numeric. Numerator used in the metric calculation.

- Q20:

  Numeric. 20th percentile value.

- Q40:

  Numeric. 40th percentile value.

- Q60:

  Numeric. 60th percentile value.

- Q80:

  Numeric. 80th percentile value.

- TimePeriodID:

  Integer. Identifier for the time period associated with the metric.

- UpperConfidenceLimit:

  Numeric. Upper bound of the confidence interval.

- Value:

  Numeric. Final calculated value for the metric.

- ValueNote:

  Character. Notes or flags associated with the value (e.g., suppression
  warnings).

**timeseries_data** contains the following columns:

- MetricID:

  Integer. Unique identifier for the metric instance. Links to the
  corresponding entry in the metric data table.

- EndDate:

  POSIXct. End date of the reporting period (e.g., "2025-06-30").

- Median:

  Numeric. Median value for the metric during the specified time period.

- StartDate:

  POSIXct. Start date of the reporting period. Typically set to a
  default baseline (e.g., "1900-01-01").

- TimePeriodID:

  Integer. Unique identifier for the time period.

- TimePeriodName:

  Character. Display label for the time period (e.g., "To June 2025").

- Value:

  Numeric. Final calculated value for the metric in the given time
  period.

## Details

This function is useful for extracting all indicator data for a given
area and period, including breakdowns by category and time series. The
list output allows easy access to different data tables for further
analysis or visualisation. Filtering by tag enables targeted queries for
specific subsets of indicators.

## Note

This function may take longer than 5 seconds to complete due to API
response time.

## API Documentation

See the [CVDPREVENT API documentation:
Indicator](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Findicator)

## See also

[`cvd_indicator_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_list.md),
[`cvd_indicator_metric_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_metric_list.md),
[`cvd_indicator_tags()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_tags.md),
[`cvd_indicator_details()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_details.md),
[`cvd_indicator_sibling()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_sibling.md),
[`cvd_indicator_child_data()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_child_data.md),
[`cvd_indicator_data()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_data.md),
[`cvd_indicator_metric_data()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_metric_data.md),
[`cvd_indicator_raw_data()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_raw_data.md),
[`cvd_indicator_nationalarea_metric_data()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_nationalarea_metric_data.md),
[`cvd_indicator_priority_groups()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_priority_groups.md),
[`cvd_indicator_pathway_group()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_pathway_group.md),
[`cvd_indicator_group()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_group.md),
[`cvd_indicator_metric_timeseries()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_metric_timeseries.md),
[`cvd_indicator_person_timeseries()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_person_timeseries.md),
[`cvd_indicator_metric_systemlevel_comparison()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_metric_systemlevel_comparison.md),
[`cvd_indicator_metric_area_breakdown()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_metric_area_breakdown.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Get all indicator data for area_id = 1103 in time period 17
return_list <- cvd_indicator(time_period_id = 17, area_id = 1103)

# See what data tables are available
summary(return_list)

# Extract and examine indicators
indicators <- return_list$indicators
indicators |>
  dplyr::select(IndicatorID, IndicatorCode, IndicatorShortName) |>
  dplyr::arrange(IndicatorID) |>
  dplyr::slice_head(n = 4)

# Extract metric categories for a specific indicator and categories
categories <- return_list$metric_categories
categories |>
  dplyr::filter(IndicatorID == 7, MetricCategoryID %in% c(7, 8)) |>
  dplyr::select(
    IndicatorID,
    MetricCategoryTypeName,
    CategoryAttribute,
    MetricCategoryName,
    MetricID
  )

# Extract metric data for specific metrics
metric_data <- return_list$metric_data
metric_data |>
  dplyr::filter(MetricID %in% c(126, 132)) |>
  dplyr::select(MetricID, Value, Numerator, Denominator)

# Extract time series data for selected metrics
timeseries_data <- return_list$timeseries_data
timeseries_data |>
  dplyr::filter(MetricID %in% c(126, 132), !is.na(Value))

# Filter by tags: get indicators tagged with either tag 3 or 4 in area 3, time period 17
return_list <- cvd_indicator(time_period_id = 17, area_id = 3, tag_id = c(3, 4))
} # }
```
