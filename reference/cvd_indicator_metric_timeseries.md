# Retrieve time series data for a specific metric and area

Returns a tibble containing time series data for a specified metric and
NHS area. The output includes both national-level (England) and
local-level values across reporting periods, enabling direct comparison
and trend analysis.

## Usage

``` r
cvd_indicator_metric_timeseries(metric_id, area_id)
```

## Arguments

- metric_id:

  Integer (required). The ID of the metric to retrieve. Use
  [`cvd_indicator_metric_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_metric_list.md)
  or
  [`cvd_indicator_data()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_data.md)
  to find valid IDs.

- area_id:

  Integer (required). The ID of the NHS area to retrieve data for. Use
  [`cvd_area_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_list.md)
  or
  [`cvd_area_search()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_search.md)
  to find valid IDs.

## Value

A tibble where each row represents a time period for a specific NHS
area, including the observed metric value and associated target
threshold. Columns include:

- AreaCode:

  Character. Code for the NHS area (e.g., "U60510" for a PCN,
  "E92000001" for England).

- AreaID:

  Integer. Unique identifier for the NHS area.

- AreaName:

  Character. Name of the NHS area (e.g., "Salford South East PCN").

- Count:

  Integer. Number of records included in the calculation (e.g., eligible
  patients).

- Denominator:

  Numeric. Denominator used in the metric calculation.

- Factor:

  Numeric. Scaling factor applied to the metric, if applicable. Often
  blank.

- Numerator:

  Numeric. Numerator used in the metric calculation.

- TimePeriodID:

  Integer. Identifier for the reporting period.

- TimePeriodName:

  Character. Display label for the time period (e.g., "To June 2024").

- Value:

  Numeric. Final calculated value for the metric in the given period.

- TargetLabel:

  Character. Descriptive label for the target threshold (e.g., "Upper
  threshold for QOF").

- TargetValue:

  Numeric. Target value to be achieved (e.g., 95).

If no data is available for the given parameters, a tibble describing
the error is returned.

## Details

This function is designed to support longitudinal analysis of indicator
performance. It returns:

- Time series values for the selected metric in the specified area

- Corresponding national values (AreaID = 1)

- Target thresholds (if defined) for benchmarking

The result includes one row per time period per area, allowing users to:

- Visualise trends over time

- Compare local performance against national averages

- Track progress toward clinical targets

To find valid `metric_id` values, use
[`cvd_indicator_metric_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_metric_list.md)
or
[`cvd_indicator_data()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_data.md).
For valid `area_id` values, use
[`cvd_area_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_list.md)
or
[`cvd_area_search()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_search.md).

## Note

This function may take longer than 5 seconds to complete due to API
response time.

## API Documentation

See the [CVDPREVENT API documentation: Indicator time series
metrics](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Findicator%2FtimeSeriesByMetric%2F%3Cmetric_ID%3E)

## See also

[`cvd_indicator_metric_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_metric_list.md)
to browse available metrics,
[`cvd_area_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_list.md)
and
[`cvd_area_search()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_search.md)
to find valid area IDs,
[`cvd_indicator_data()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_data.md)
to retrieve current metric values,
[`cvd_indicator_priority_groups()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_priority_groups.md)
for grouped indicator metadata,
[`cvd_indicator_metric_area_breakdown()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_metric_area_breakdown.md)
for area-level comparisons,
[`cvd_indicator_person_timeseries()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_person_timeseries.md)
for person-level time series data

## Examples

``` r
# \donttest{
# List data for Salford South East PCN (area ID 705) for 'AF: treatment with
# anticoagulants' for women people aged 60-79 years (metric ID 130):
cvd_indicator_metric_timeseries(metric_id = 130, area_id = 705) |>
  dplyr::select(AreaName, TimePeriodName, TimePeriodID, Value) |>
  tidyr::pivot_wider(
    names_from = AreaName,
    values_from = Value
  )
#> # A tibble: 16 × 4
#>    TimePeriodName    TimePeriodID England `Salford South East PCN`
#>    <chr>                    <int>   <dbl>                    <dbl>
#>  1 To March 2020                1    88.2                     85.9
#>  2 To March 2021                2    88.6                     86  
#>  3 To September 2021            3    88.9                     88.8
#>  4 To March 2022                4    89.3                     90  
#>  5 To June 2022                 5    89.4                     90.2
#>  6 To September 2022            6    89.6                     90.6
#>  7 To December 2022             7    90.0                     91.7
#>  8 To March 2023                8    91.0                     90.1
#>  9 To June 2023                 9    91.0                     91.0
#> 10 To December 2023            15    91.2                     92.2
#> 11 To March 2024               17    92.2                     93.9
#> 12 To June 2024                18    92.2                     92.9
#> 13 To September 2024           20    92                       93.1
#> 14 To December 2024            22    92.0                     92.8
#> 15 To March 2025               24    92.5                     94.6
#> 16 To June 2025                26    92.4                     94.6
# }
```
