# Indicator sibling data

Returns data for all sibling areas (i.e., areas sharing the same parent)
and the specified area itself, for a given metric and reporting period
from the CVDPREVENT API. This endpoint is intended to provide a direct
comparison of a single metric across related areas (e.g., all PCNs
within an ICB, or all practices within a PCN).

Only the selected metric is returned for each sibling area.

## Usage

``` r
cvd_indicator_sibling(time_period_id, area_id, metric_id)
```

## Arguments

- time_period_id:

  Integer (required). The reporting period (time period) for which to
  return sibling data. Use
  [`cvd_time_period_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_time_period_list.md)
  to obtain valid IDs.

- area_id:

  Integer (required). The AreaID for which sibling data will be
  determined. Use
  [`cvd_area_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_list.md)
  or
  [`cvd_area_search()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_search.md)
  to find valid IDs.

- metric_id:

  Integer (required). The MetricID for which to return data. Use
  [`cvd_indicator_metric_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_metric_list.md)
  or
  [`cvd_indicator_data()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_data.md)
  to find valid MetricIDs.

## Value

A tibble containing the data for the specified metric in the selected
area and all its siblings for the given reporting period. Columns
include:

- CategoryAttribute:

  Character. Grouping label used to define the population subset (e.g.,
  "Male").

- IndicatorCode:

  Character. Unique code for the indicator (e.g., "CVDP002AF").

- IndicatorID:

  Integer. Unique identifier for the indicator.

- IndicatorName:

  Character. Full descriptive name of the indicator.

- IndicatorOrder:

  Integer. Display order for the indicator in dashboards or reports.

- IndicatorShortName:

  Character. Abbreviated name of the indicator for display purposes.

- MetricCategoryID:

  Integer. Unique identifier for the metric category.

- MetricCategoryName:

  Character. Name of the subgroup or category (e.g., "40–59").

- MetricCategoryOrder:

  Integer. Display order for the category within its type.

- MetricCategoryTypeName:

  Character. Type of category used for breakdown (e.g., "Age group").

- AreaCode:

  Character. Code for the NHS area (e.g., PCN).

- AreaID:

  Integer. Unique identifier for the NHS area.

- AreaName:

  Character. Name of the NHS area (e.g., "Greenwood PCN").

- Count:

  Integer. Number of records included in the calculation.

- DataID:

  Integer. Unique identifier for the data point.

- Denominator:

  Numeric. Denominator used in the metric calculation.

- Factor:

  Numeric. Scaling factor applied to the metric, if applicable. Often
  blank.

- HighestPriorityNotificationType:

  Character. Notification priority level, if applicable (e.g., "Red").
  Often blank.

- LowerConfidenceLimit:

  Numeric. Lower bound of the confidence interval.

- Max:

  Numeric. Maximum observed value for the metric.

- Median:

  Numeric. Median value for the metric.

- Min:

  Numeric. Minimum observed value for the metric.

- NotificationCount:

  Integer. Count of notifications associated with the indicator.

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

- SystemLevelID:

  Integer. Identifier for the system level (e.g., 4 = PCN).

- SystemLevelName:

  Character. Name of the system level (e.g., "PCN").

- TimePeriodID:

  Integer. Identifier for the time period associated with the metric.

- TimePeriodName:

  Character. Display label for the time period (e.g., "To March 2024").

- UpperConfidenceLimit:

  Numeric. Upper bound of the confidence interval.

- Value:

  Numeric. Final calculated value for the metric.

- ValueNote:

  Character. Notes or flags associated with the value (e.g., suppression
  warnings).

If no sibling data is found, returns a tibble describing the error.

## Details

Use this function to compare a metric across all areas at the same
hierarchical level (e.g., compare all practices in a PCN or all PCNs in
an ICB) for benchmarking and visualisation purposes.

## Note

This function may take longer than 5 seconds to complete due to API
response time.

## API Documentation

See the [CVDPREVENT API documentation: Indicator sibling
data](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Findicator%2FsiblingData)

## See also

[`cvd_indicator_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_list.md),
[`cvd_indicator_metric_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_metric_list.md),
[`cvd_indicator()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator.md),
[`cvd_indicator_tags()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_tags.md),
[`cvd_indicator_details()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_details.md),
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
# Compare the value of metric 126 for area 1103 and all its siblings in time period 17
cvd_indicator_sibling(time_period_id = 17, area_id = 1103, metric_id = 126) |>
  dplyr::select(AreaID, AreaName, Value, LowerConfidenceLimit, UpperConfidenceLimit)

# Find a valid metric ID for an indicator, then get sibling data
metrics <- cvd_indicator_metric_list(time_period_id = 17, system_level_id = 5)
metric_id <- metrics$MetricID[1]
cvd_indicator_sibling(time_period_id = 17, area_id = 1103, metric_id = metric_id)
} # }
```
