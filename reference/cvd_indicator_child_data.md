# Retrieve child area data for a specific metric, time period and area

Returns the value of a single metric for all child areas of a specified
NHS area (and the specified area itself) for a chosen reporting period,
using the CVDPREVENT API. This function enables direct comparison of a
specific metric across all subordinate areas (e.g., all GP practices
within a PCN, or all PCNs within an ICB).

Only the selected metric is returned for each child area.

## Usage

``` r
cvd_indicator_child_data(time_period_id, area_id, metric_id)
```

## Arguments

- time_period_id:

  Integer (required). The reporting period (time period) for which to
  return child data. Use
  [`cvd_time_period_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_time_period_list.md)
  to find valid IDs.

- area_id:

  Integer (required). The AreaID for which to find child areas. Use
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

A tibble with the value of the specified metric for the given area and
all its child areas, for the specified time period. Columns include:

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

  Character. Code for the child NHS area (e.g., PCN).

- AreaID:

  Integer. Unique identifier for the child NHS area.

- AreaName:

  Character. Name of the child NHS area (e.g., "Teldoc PCN").

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

If no child data is found, returns a tibble describing the error.

## Details

Use this function to compare a metric across all immediate child areas
under a parent (for example, to benchmark all GP practices within a PCN
for a specific indicator and reporting period).

## Note

This function may take longer than 5 seconds to complete due to API
response time.

## API Documentation

See the [CVDPREVENT API documentation: Indicator child
data](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Findicator%2FchildData)

## See also

[`cvd_indicator_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_list.md),
[`cvd_indicator_metric_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_metric_list.md),
[`cvd_indicator()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator.md),
[`cvd_indicator_tags()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_tags.md),
[`cvd_indicator_details()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_details.md),
[`cvd_indicator_sibling()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_sibling.md),
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
# \donttest{
# Compare the value of metric 126 for area 74 and all its child areas in time period 22
cvd_indicator_child_data(time_period_id = 22, area_id = 74, metric_id = 126) |>
  dplyr::select(AreaID, AreaName, Value, LowerConfidenceLimit, UpperConfidenceLimit)
#> # A tibble: 8 × 5
#>   AreaID AreaName                Value LowerConfidenceLimit UpperConfidenceLimit
#>    <int> <chr>                   <dbl>                <dbl>                <dbl>
#> 1   1201 North Shropshire PCN     95.4                 80                  100  
#> 2    226 Wrekin PCN               93.3                 72.2                100  
#> 3   1373 Shrewsbury PCN           92.3                 75.9                 96.6
#> 4    435 Newport And Central PCN  89.5                 68.2                 95.4
#> 5    848 Sw Shropshire PCN        88.9                 58.3                100  
#> 6    731 South East Telford PCN   86.7                 61.1                 94.4
#> 7    899 Se Shropshire PCN        86.4                 68                   96  
#> 8   1338 Teldoc PCN               83.9                 67.6                 91.2

# Find a valid metric ID for an indicator, then get child area data
metrics <- cvd_indicator_metric_list(time_period_id = 17, system_level_id = 5)
metric_id <- metrics$MetricID[1]
cvd_indicator_child_data(time_period_id = 17, area_id = 1103, metric_id = metric_id)
#> # A tibble: 5 × 35
#>   CategoryAttribute IndicatorCode IndicatorID IndicatorName       IndicatorOrder
#>   <chr>             <chr>               <int> <chr>                        <int>
#> 1 Persons           CVDP002CKD             13 "Patients whose la…              1
#> 2 Persons           CVDP002CKD             13 "Patients whose la…              1
#> 3 Persons           CVDP002CKD             13 "Patients whose la…              1
#> 4 Persons           CVDP002CKD             13 "Patients whose la…              1
#> 5 Persons           CVDP002CKD             13 "Patients whose la…              1
#> # ℹ 30 more variables: IndicatorShortName <chr>, MetricCategoryID <int>,
#> #   MetricCategoryName <chr>, MetricCategoryOrder <int>,
#> #   MetricCategoryTypeName <chr>, AreaCode <chr>, AreaID <int>, AreaName <chr>,
#> #   Count <dbl>, DataID <int>, Denominator <dbl>, Factor <lgl>,
#> #   HighestPriorityNotificationType <lgl>, LowerConfidenceLimit <dbl>,
#> #   Max <dbl>, Median <dbl>, Min <dbl>, NotificationCount <int>,
#> #   Numerator <dbl>, Q20 <dbl>, Q40 <dbl>, Q60 <dbl>, Q80 <dbl>, …
# }
```
