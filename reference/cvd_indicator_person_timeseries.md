# Retrieve inequality time series data for a specific indicator and area

Returns a tibble containing time series data for a specified indicator
and NHS area, broken down by inequality markers such as age group,
ethnicity, deprivation quintile, and sex. This function supports the
Inequalities Markers Time Series chart used in CVDPREVENT reporting.

## Usage

``` r
cvd_indicator_person_timeseries(indicator_id, area_id)
```

## Arguments

- indicator_id:

  Integer (required). The ID of the indicator to retrieve. Use
  [`cvd_indicator_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_list.md)
  or
  [`cvd_indicator_priority_groups()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_priority_groups.md)
  to find valid IDs.

- area_id:

  Integer (required). The ID of the NHS area to retrieve data for. Use
  [`cvd_area_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_list.md)
  or
  [`cvd_area_search()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_search.md)
  to find valid IDs.

## Value

A tibble where each row represents a time period for a specific NHS area
and inequality subgroup. Columns include:

- AreaCode:

  Character. Code for the NHS area (e.g., "U60510" for a PCN).

- AreaID:

  Integer. Unique identifier for the NHS area.

- AreaName:

  Character. Name of the NHS area (e.g., "Salford South East PCN").

- MetricCategoryID:

  Integer. Unique identifier for the subgroup category (e.g., age band,
  ethnicity).

- MetricCategoryName:

  Character. Label for the subgroup (e.g., "Female", "80+", "White").

- TimePeriodID:

  Integer. Identifier for the reporting period.

- TimePeriodName:

  Character. Display label for the time period (e.g., "To March 2025").

- Value:

  Numeric. Final calculated value for the metric in the given subgroup
  and time period. May be blank if unavailable.

- MetricCategoryTypeID:

  Integer. Identifier for the type of inequality marker (e.g., 1 = Age
  group, 3 = Ethnicity).

- MetricCategoryTypeName:

  Character. Descriptive name of the inequality marker type (e.g.,
  "Sex", "Age group", "Ethnicity").

- TargetLabel:

  Character. Descriptive label for the target threshold (e.g., "Upper
  threshold for QOF").

- TargetValue:

  Numeric. Target value to be achieved (e.g., 95).

If no data is available for the given parameters, a tibble describing
the error is returned.

## Details

The output includes:

- Time series values for each subgroup within the selected indicator

- Target thresholds (if defined) for benchmarking

- Metric category metadata (e.g., "Age group", "Ethnicity")

This function is useful for:

- Analysing disparities in indicator performance across population
  subgroups

- Tracking progress toward clinical targets over time

- Supporting equity-focused reporting and visualisation

To find valid `indicator_id` values, use
[`cvd_indicator_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_list.md)
or
[`cvd_indicator_priority_groups()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_priority_groups.md).
For valid `area_id` values, use
[`cvd_area_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_list.md)
or
[`cvd_area_search()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_search.md).

## Note

This function may take longer than 5 seconds to complete due to API
response time.

## API Documentation

See the [CVDPREVENT API documentation: Indicator person time
series](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Findicator%2FpersonsTimeSeriesByIndicator%2F%3Cindicator_ID%3E)

## See also

[`cvd_indicator_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_list.md)
to browse indicators,
[`cvd_indicator_priority_groups()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_priority_groups.md)
to explore indicator groupings,
[`cvd_area_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_list.md)
and
[`cvd_area_search()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_search.md)
to find valid area IDs,
[`cvd_indicator_metric_timeseries()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_metric_timeseries.md)
for overall time series data,
[`cvd_indicator_metric_area_breakdown()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_metric_area_breakdown.md)
for area-level comparisons

## Examples

``` r
# \donttest{
# View age group inequalities for indicator ID 7 in Salford South East PCN (area ID 705)
cvd_indicator_person_timeseries(indicator_id = 7, area_id = 705) |>
  dplyr::filter(
    MetricCategoryTypeName == "Age group",
    !is.na(Value)
  ) |>
  dplyr::select(MetricCategoryName, TimePeriodName, TimePeriodID, Value) |>
  tidyr::pivot_wider(
    names_from = MetricCategoryName,
    values_from = Value
  )
#> # A tibble: 16 × 5
#>    TimePeriodName    TimePeriodID `40-59` `60-79` `80+`
#>    <chr>                    <int>   <dbl>   <dbl> <dbl>
#>  1 To March 2020                1    78.3    82.3  85  
#>  2 To March 2021                2    91.3    82.3  84.8
#>  3 To September 2021            3    78      86.9  87.2
#>  4 To March 2022                4    76.6    86.8  90.2
#>  5 To June 2022                 5    82.4    86.8  88.6
#>  6 To September 2022            6    80      86.6  90.6
#>  7 To December 2022             7    76.3    87.5  90.6
#>  8 To March 2023                8    72.7    87.4  91.1
#>  9 To June 2023                 9    71.1    88.5  91.3
#> 10 To December 2023            15    74.4    88.9  90.6
#> 11 To March 2024               17    80.5    91.5  92.4
#> 12 To June 2024                18    82.9    91.6  92.2
#> 13 To September 2024           20    80.4    91.6  91.9
#> 14 To December 2024            22    85.7    91.4  91.6
#> 15 To March 2025               24    85.7    93.3  91.9
#> 16 To June 2025                26    85.7    92.9  92.2
# }
```
