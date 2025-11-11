# Compare metric performance for an area against national and system-level peers

Returns a tibble showing the performance of a specified metric for a
given NHS area, alongside national-level data and other areas within the
same system level (e.g., PCNs within an ICB). This function powers the
Area Breakdown chart in CVDPREVENT reporting.

## Usage

``` r
cvd_indicator_metric_area_breakdown(time_period_id, area_id, metric_id)
```

## Arguments

- time_period_id:

  Integer (required). The ID of the reporting period. Use
  [`cvd_time_period_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_time_period_list.md)
  to find valid IDs.

- area_id:

  Integer (required). The ID of the NHS area to anchor the comparison.
  Use
  [`cvd_area_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_list.md)
  or
  [`cvd_area_search()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_search.md)
  to find valid IDs.

- metric_id:

  Integer (required). The ID of the metric to retrieve. Use
  [`cvd_indicator_metric_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_metric_list.md)
  or
  [`cvd_indicator_data()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_data.md)
  to find valid IDs.

## Value

A tibble comparing metric performance for a specified NHS area and the
national aggregate. Each row represents one area (local or national) and
includes the following columns:

- NationalLevel:

  Character. Indicates whether the row represents national-level data
  ("Y" for national, "N" for local).

- SystemLevelID:

  Integer. Identifier for the system level (e.g., 1 = England, 4 = PCN).

- SystemLevelMedian:

  Numeric. Median value for the metric across all areas in the system
  level.

- SystemLevelName:

  Character. Name of the system level (e.g., "England", "PCN").

- SystemLevelOrder:

  Integer. Display order for the system level.

- TargetLabel:

  Character. Descriptive label for the target threshold (e.g., "Upper
  threshold for QOF").

- TargetValue:

  Numeric. Target value to be achieved (e.g., 95).

- AreaCode:

  Character. Code for the NHS area (e.g., "U60510" for a PCN,
  "E92000001" for England).

- AreaID:

  Integer. Unique identifier for the NHS area.

- AreaName:

  Character. Name of the NHS area (e.g., "Salford South East PCN").

- Value:

  Numeric. Final calculated value for the metric in the area.

If no data is available for the given parameters, a tibble describing
the error is returned.

## Details

The output includes:

- Metric values for the selected area

- Comparison with national performance (AreaID = 1)

- Peer areas within the same system level

- Target thresholds (if defined)

This function is useful for:

- Benchmarking local performance against national and peer averages

- Identifying variation within a system level

- Supporting targeted improvement and equity analysis

To find valid `metric_id` values, use
[`cvd_indicator_metric_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_metric_list.md)
or
[`cvd_indicator_data()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_data.md).
For valid `area_id` values, use
[`cvd_area_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_list.md)
or
[`cvd_area_search()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_search.md).
For valid `time_period_id` values, use
[`cvd_time_period_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_time_period_list.md).

## Note

This function may take longer than 5 seconds to complete due to API
response time.

## API Documentation

See the [CVDPREVENT API documentation: Indicator metric area
breakdown](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Findicator%2FmetricAreaBreakdown%2F%3Cmetric_ID%3E)

## See also

[`cvd_indicator_metric_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_metric_list.md)
to browse available metrics,
[`cvd_area_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_list.md)
and
[`cvd_area_search()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_search.md)
to find valid area IDs,
[`cvd_time_period_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_time_period_list.md)
to explore reporting periods,
[`cvd_indicator_metric_systemlevel_comparison()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_metric_systemlevel_comparison.md)
for peer-level comparisons,
[`cvd_indicator_metric_timeseries()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_metric_timeseries.md)
for longitudinal analysis,
[`cvd_indicator_priority_groups()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_priority_groups.md)
for grouped indicator metadata

## Examples

``` r
# \donttest{
# Compare performance for metric ID 128 in time period 17 for
# Salford South East PCN (area ID 705)
cvd_indicator_metric_area_breakdown(
  metric_id = 128,
  time_period_id = 17,
  area_id = 705
) |>
  dplyr::select(SystemLevelName, AreaID, AreaName, Value)
#> # A tibble: 2 × 4
#>   SystemLevelName AreaID AreaName               Value
#>   <chr>            <int> <chr>                  <dbl>
#> 1 England              1 England                 92.5
#> 2 PCN                705 Salford South East PCN  90  
# }
```
