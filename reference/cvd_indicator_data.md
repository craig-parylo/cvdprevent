# Retrieve CVD indicator data for a specific area and time period

Fetches all available metric breakdowns for a single cardiovascular
disease (CVD) indicator from the CVDPREVENT API, scoped to a specified
NHS area and reporting period. This includes subgroup data such as age,
sex, ethnicity, deprivation quintile, and more.

## Usage

``` r
cvd_indicator_data(time_period_id, area_id, indicator_id)
```

## Arguments

- time_period_id:

  Integer (required). The reporting period (time period) to retrieve
  data for. Use
  [`cvd_time_period_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_time_period_list.md)
  to find valid IDs.

- area_id:

  Integer (required). The AreaID for which to retrieve indicator data.
  use
  [`cvd_area_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_list.md)
  or
  [`cvd_area_search()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_search.md)
  to find valid IDs.

- indicator_id:

  Integer (required). The IndicatorID for which to retrieve data. use
  [`cvd_indicator_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_list.md)
  or
  [`cvd_indicator_metric_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_metric_list.md)
  to find valid IDs.

## Value

A named list containing three tibbles:

- indicator_metrics:

  Tibble. Metadata and definitions for the selected indicator and its
  associated metrics.

- area_data:

  Tibble. Metric values for the specified NHS area (`area_id`) across
  all available breakdowns.

- national_data:

  Tibble. Metric values for England, used for benchmarking and
  comparison.

If no indicator data is found, returns a tibble describing the error.

**indicator_metrics** contains the following items:

- AxisCharacter:

  Character. Symbol used to represent the metric axis (e.g., "%").

- FormatDisplayName:

  Character. Display format for the metric (e.g., "Proportion %").

- IndicatorCode:

  Character. Unique code for the indicator (e.g., "CVDP002AF").

- IndicatorFormatID:

  Integer. Internal ID for the indicator's format type.

- IndicatorID:

  Integer. Unique identifier for the indicator.

- IndicatorName:

  Character. Full descriptive name of the indicator.

- IndicatorOrder:

  Integer. Display order for the indicator in dashboards or reports.

- IndicatorShortName:

  Character. Abbreviated name of the indicator for display.

- NotificationCount:

  Integer. Count of notifications associated with the indicator. Often
  zero.

- TimePeriodID:

  Integer. ID of the reporting time period.

- TimePeriodName:

  Character. Label for the reporting time period (e.g., "To March
  2024").

- CategoryAttribute:

  Character. Subgroup label (e.g., "Male", "Female", "Persons").

- MetricCategoryID:

  Integer. Unique ID for the metric category.

- MetricCategoryName:

  Character. Name of the subgroup or category (e.g., "18–39", "Sex").

- MetricCategoryOrder:

  Integer. Display order for the metric category.

- MetricCategoryTypeName:

  Character. Type of subgroup (e.g., "Age group", "Sex").

- MetricID:

  Integer. Unique ID for the specific metric being measured.

**area_data** and **national_data** contain the following items:

- MetricID:

  Integer. Unique identifier for the metric being measured.

- MetricCategoryTypeName:

  Character. Type of subgroup (e.g., "Sex", "Age group").

- MetricCategoryName:

  Character. Name of the subgroup (e.g., "Female", "18–39").

- CategoryAttribute:

  Character. Label used to group individuals (e.g., "Male", "Persons").

- AreaCode:

  Character. ONS code for the NHS area (e.g., "U60176").

- AreaID:

  Integer. Internal ID for the NHS area.

- AreaName:

  Character. Name of the NHS area (e.g., "3 Centres PCN").

- Count:

  Integer. Total number of individuals in the subgroup.

- DataID:

  Integer. Unique identifier for the data record.

- Denominator:

  Numeric. Population or count used as the denominator in metric
  calculation.

- Factor:

  Numeric. Scaling factor applied to the metric, if applicable. Often
  NA.

- HighestPriorityNotificationType:

  Character. Notification priority level, if available. Often NA.

- LowerConfidenceLimit:

  Numeric. Lower bound of the confidence interval for the metric value.

- Max:

  Numeric. Maximum observed value for the metric across comparable
  areas.

- Median:

  Numeric. Median value for the metric across comparable areas.

- Min:

  Numeric. Minimum observed value for the metric across comparable
  areas.

- NotificationCount:

  Integer. Count of notifications associated with the indicator. Often
  zero.

- Numerator:

  Numeric. Count used as the numerator in metric calculation.

- Q20:

  Numeric. 20th percentile value across comparable areas.

- Q40:

  Numeric. 40th percentile value across comparable areas.

- Q60:

  Numeric. 60th percentile value across comparable areas.

- Q80:

  Numeric. 80th percentile value across comparable areas.

- TimePeriodID:

  Integer. ID of the reporting time period.

- TimePeriodName:

  Character. Label for the reporting time period (e.g., "To March
  2024").

- UpperConfidenceLimit:

  Numeric. Upper bound of the confidence interval for the metric value.

- Value:

  Numeric. Calculated metric value (e.g., percentage of patients
  treated).

- ValueNote:

  Character. Additional notes or flags about the value. Often NA.

## Details

Use this function to obtain all metric values for a single indicator in
a particular area and time period, such as for a local dashboard or a
focussed report. For broader queries across multiple indicators, see
[`cvd_indicator()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator.md)
or
[`cvd_indicator_metric_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_metric_list.md).

## Note

This function may take longer than 5 seconds to complete due to API
response time.

## API Documentation

See the [CVDPREVENT API documentation: Indicator
data](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Findicator%2Fdata)

## See also

[`cvd_indicator_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_list.md),
[`cvd_indicator_metric_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_metric_list.md),
[`cvd_indicator()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator.md),
[`cvd_indicator_tags()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_tags.md),
[`cvd_indicator_details()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_details.md),
[`cvd_indicator_sibling()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_sibling.md),
[`cvd_indicator_child_data()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_child_data.md),
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
# Retrieve all metric breakdowns for indicator 7 in area 1103 for time period 17
returned_list <- cvd_indicator_data(time_period_id = 17, area_id = 1103, indicator_id = 7)

# See the structure of this data
returned_list |> dplyr::glimpse()

# See the definition for one metric
returned_list$indicator_metrics |>
  dplyr::slice_head(n = 1) |>
  dplyr::glimpse()

# Compare performance in the specified area (AreaID = 1103) with national results for
# women aged 40-59 years (MetricID = 132)
dplyr::bind_rows(
  returned_list$area_data,
  returned_list$national_data
) |>
dplyr::filter(MetricID == 132)
} # }
```
