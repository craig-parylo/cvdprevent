# Retrieve metric data for a specific area and for national (England) comparison

Returns a named list of tibbles containing: (1) metric data for the
specified NHS area and the national (England, AreaID = 1) aggregate, and
(2) details achieving the target value (if defined), including the
target percentage and the additional number of patients needed to reach
the target. This function supports benchmarking local performance vs.
the national average, and helps quantify gaps to clinical targets.

If there is no data for either national or the chosen area for the given
parameters, an error tibble is returned.

## Usage

``` r
cvd_indicator_nationalarea_metric_data(time_period_id, area_id, metric_id)
```

## Arguments

- time_period_id:

  Integer (required). The reporting period (time period) for which to
  retrieve metric data. Use
  [`cvd_time_period_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_time_period_list.md)
  to find valid IDs.

- area_id:

  Integer (required). The AreaID for which to retrieve data in addition
  to the national aggregate. use
  [`cvd_area_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_list.md)
  or
  [`cvd_area_search()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_search.md)
  to find valid IDs.

- metric_id:

  Integer (required). The MetricID for which to retrieve values. Use
  [`cvd_indicator_metric_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_metric_list.md)
  or
  [`cvd_indicator_data()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_data.md)
  to find valid MetricIDs.

## Value

A named list with up to two tibbles:

- area:

  Tibble with one or more rows, summarising the metric for the specified
  area and the England aggregate (AreaID = 1).

- target:

  Tibble (if available) with target-setting details for the area

If no data exists for both the area and the national aggregate for the
given parameters, returns a tibble describing the error.

**area** contains the following columns:

- AreaCode:

  Character. Code for the NHS area (e.g., "U68943" for Chester South
  PCN, "E92000001" for England).

- AreaID:

  Integer. Unique identifier for the NHS area.

- AreaName:

  Character. Name of the NHS area (e.g., "Chester South PCN",
  "England").

- HighestPriorityNotificationType:

  Character. Notification priority level, if applicable (e.g., "Red").
  Often blank.

- NationalLevel:

  Character. Indicates whether the area represents national-level data
  ("Y" or "N").

- NotificationCount:

  Integer. Count of notifications associated with the area for the given
  metric.

- Value:

  Numeric. Final calculated value for the metric in the specified area.

**target** contains the following columns:

- TargetLabel:

  Character. Descriptive label for the target (e.g., "Upper threshold
  for QOF").

- TargetPatients:

  Integer. Number of additional patients needed to achieve the target
  threshold.

- TargetValue:

  Numeric. Target value or threshold to be achieved (e.g., 95).

## Details

Use this function to benchmark a local area's metric value against the
national figure and to understand the actual gap to a clinically
meaningful target.

## Note

This function may take longer than 5 seconds to complete due to API
response time.

## API Documentation

See the [CVDPREVENT API documentation: Indicator national area metric
data](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Findicator%2FnationalAreaMetricData)

## See also

[`cvd_indicator_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_list.md),
[`cvd_indicator_metric_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_metric_list.md),
[`cvd_indicator_metric_data()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_metric_data.md),
[`cvd_indicator_data()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_data.md),
[`cvd_indicator_raw_data()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_raw_data.md),
[`cvd_area_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_list.md)

## Examples

``` r
# \donttest{
# Compare performance against metric 150  (AF: treatment with anticoagulants
# - all people) in 'Chester South PCN' (area ID 553) with national
# performance:
returned_list <- cvd_indicator_nationalarea_metric_data(
    metric_id = 150,
    time_period_id = 17,
    area_id = 553
)

# See what the list contains
returned_list |> summary()
#>        Length Class  Mode
#> area   7      tbl_df list
#> target 3      tbl_df list

# Extract the `area` details
area_data <- returned_list$area
area_data
#> # A tibble: 2 × 7
#>   AreaCode  AreaID AreaName          HighestPriorityNotification…¹ NationalLevel
#>   <chr>      <int> <chr>             <lgl>                         <chr>        
#> 1 E92000001      1 England           NA                            Y            
#> 2 U68943       553 Chester South PCN NA                            N            
#> # ℹ abbreviated name: ¹​HighestPriorityNotificationType
#> # ℹ 2 more variables: NotificationCount <int>, Value <dbl>

# Extract `target` details
target_data <- returned_list$target
target_data
#> # A tibble: 1 × 3
#>   TargetLabel             TargetPatients TargetValue
#>   <chr>                            <int>       <dbl>
#> 1 Upper threshold for QOF             45          95
# }
```
