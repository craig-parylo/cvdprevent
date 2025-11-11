# Compare metric performance across system-level areas

Returns a tibble comparing the performance of a specified metric across
all areas within the same system level (e.g., all PCNs within an ICB)
for a given time period. This function powers the System Level
Comparison chart in CVDPREVENT reporting.

## Usage

``` r
cvd_indicator_metric_systemlevel_comparison(metric_id, time_period_id, area_id)
```

## Arguments

- metric_id:

  Integer (required). The ID of the metric to compare. Use
  [`cvd_indicator_metric_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_metric_list.md)
  or
  [`cvd_indicator_data()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_data.md)
  to find valid IDs.

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

## Value

A tibble where each row represents an NHS area within the same system
level, showing its performance for a specified metric. Columns include:

- NationalLevel:

  Character. Indicates whether the row represents national-level data
  ("Y" or "N").

- SystemLevelID:

  Integer. Identifier for the system level (e.g., 4 = PCN).

- SystemLevelMedian:

  Numeric. Median value for the metric across all areas in the system
  level.

- SystemLevelName:

  Character. Name of the system level (e.g., "PCN").

- SystemLevelOrder:

  Integer. Display order for the system level.

- AreaCode:

  Character. Code for the NHS area (e.g., "U55387").

- AreaID:

  Integer. Unique identifier for the NHS area.

- AreaName:

  Character. Name of the NHS area (e.g., "Barking & Dagenham North
  PCN").

- Value:

  Numeric. Final calculated value for the metric in the area.

If no data is available for the given parameters, a tibble describing
the error is returned.

## Details

The output includes:

- Metric values for the selected area and its system-level peers

- Target thresholds (if defined)

- System-level metadata (e.g., "PCN", "ICB")

This function is useful for:

- Benchmarking local performance against peer organisations

- Identifying variation across system-level areas

- Supporting equity and improvement initiatives at regional levels

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

See the [CVDPREVENT API documentation: Indicator metric system level
comparison](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Findicator%2FmetricSystemLevelComparison%2F%3Cmetric_ID%3E)

## See also

[`cvd_indicator_metric_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_metric_list.md)
to browse available metrics,
[`cvd_area_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_list.md)
and
[`cvd_area_search()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_search.md)
to find valid area IDs,
[`cvd_time_period_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_time_period_list.md)
to explore reporting periods,
[`cvd_indicator_metric_timeseries()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_metric_timeseries.md)
for longitudinal analysis,
[`cvd_indicator_metric_area_breakdown()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_metric_area_breakdown.md)
for localised comparisons,
[`cvd_indicator_priority_groups()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_priority_groups.md)
for grouped indicator metadata

## Examples

``` r
# \donttest{
# Compare performance for metric ID 1270 in time period 17 for Salford South East PCN (area ID 705)
cvd_indicator_metric_systemlevel_comparison(
  metric_id = 1270,
  time_period_id = 17,
  area_id = 705
) |>
  dplyr::filter(AreaID %in% c(705:709), !is.na(Value)) |>
  dplyr::select(SystemLevelName, AreaID, AreaName, Value)
#> # A tibble: 4 × 4
#>   SystemLevelName AreaID AreaName                    Value
#>   <chr>            <int> <chr>                       <dbl>
#> 1 PCN                705 Salford South East PCN       80.5
#> 2 PCN                707 Haringey - North East PCN    88  
#> 3 PCN                708 Teesdale PCN                100  
#> 4 PCN                709 Spen Health & Wellbeing PCN  85.7
# }
```
