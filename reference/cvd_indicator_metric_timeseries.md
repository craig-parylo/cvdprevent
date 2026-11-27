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
if (FALSE) { # \dontrun{
# List data for Salford South East PCN (area ID 705) for 'AF: treatment with
# anticoagulants' for women people aged 60-79 years (metric ID 130):
cvd_indicator_metric_timeseries(metric_id = 130, area_id = 705) |>
  dplyr::select(AreaName, TimePeriodName, TimePeriodID, Value) |>
  tidyr::pivot_wider(
    names_from = AreaName,
    values_from = Value
  )
} # }
```
