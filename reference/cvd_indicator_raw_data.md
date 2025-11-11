# Retrieve raw metric values for multiple metrics, a specified area and time period

Returns raw values for multiple metrics within an indicator (specified
as `indicator_id`) for a single NHS system level and reporting period
using the CVDPREVENT API. This function fetches unfiltered raw data at
the metric level, allowing comprehensive extraction for all selected
metrics and their available breakdowns (such as by age, sex or other
category) within the chosen context.

## Usage

``` r
cvd_indicator_raw_data(time_period_id, system_level_id, indicator_id)
```

## Arguments

- time_period_id:

  Integer (required). The reporting period (time period) for which to
  retrieve data. Use
  [`cvd_time_period_system_levels()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_time_period_system_levels.md)
  to find valid IDs.

- system_level_id:

  Integer (required). The SystemLevelID for which to retrieve data. Use
  [`cvd_time_period_system_levels()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_time_period_system_levels.md)
  to find valid IDs.

- indicator_id:

  Integer vector (required). One or more IndicatorIDs specifying which
  indicator and its associated metrics to return. Use
  [`cvd_indicator_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_list.md)
  to find valid IndicatorIDs.

## Value

A tibble with one row per metric breakdown for all requested metrics.
The tibble has the following columns:

- AreaCode:

  Character. ONS geographic code for the area (e.g., "E92000001" for
  England).

- AreaName:

  Character. Name of the geographic area.

- CategoryAttribute:

  Character. Subgroup label (e.g., "Male", "Female", "Persons").

- Denominator:

  Numeric. Population or count used as the denominator in metric
  calculation.

- Factor:

  Numeric. Scaling factor applied to the metric, if applicable. May be
  NA.

- HighestPriorityNotificationType:

  Character. Notification priority level, if available. Often NA.

- IndicatorCode:

  Character. Unique code for the indicator (e.g., "CVDP002AF").

- IndicatorName:

  Character. Full descriptive name of the indicator.

- IndicatorShortName:

  Character. Abbreviated name of the indicator.

- LowerConfidenceLimit:

  Numeric. Lower bound of the confidence interval for the metric value.

- MetricCategoryName:

  Character. Name of the subgroup or category (e.g., "40–59", "Female").

- MetricCategoryTypeName:

  Character. Type of subgroup (e.g., "Age group", "Sex", "Ethnicity").

- NotificationCount:

  Integer. Count of notifications associated with the indicator. Often
  zero.

- Numerator:

  Numeric. Count used as the numerator in metric calculation.

- TimePeriodName:

  Character. Label for the time period (e.g., "To December 2024").

- UpperConfidenceLimit:

  Numeric. Upper bound of the confidence interval for the metric value.

- Value:

  Numeric. Calculated metric value (e.g., percentage of patients
  treated).

- ValueNote:

  Character. Additional notes or flags about the value. Often NA.

## Details

Use this function to retrieve a wide set of metric breakdowns for a
given indicator in a single area and time period - useful for broad data
extractions, dashboards or advanced analytics.

## Note

This function may take longer than 5 seconds to complete due to API
response time.

## API Documentation

See the [CVDPREVENT API documentation: Indicator raw
data](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Findicator%2F%3Cindicator_ID%3E%2FrawDataJSON)

## Examples

``` r
# \donttest{
# Retrieve metric data for 'CVD: All-cause mortality' (IndicatorID = 35) across
# NHS Regions (SystemLevelID = 6) in the period April 2024 to
# March 2025 (TimePeriodID = 27) and view a sample of 4 rows:
cvd_indicator_raw_data(
  time_period_id = 27,
  system_level_id = 6,
  indicator_id = 35
) |>
  dplyr::slice_sample(n = 4)
#> # A tibble: 4 × 18
#>   AreaCode  AreaName CategoryAttribute Denominator Factor HighestPriorityNotif…¹
#>   <chr>     <chr>    <chr>                   <dbl> <lgl>  <lgl>                 
#> 1 E40000006 South W… Persons                 79430 NA     NA                    
#> 2 E40000007 East of… Persons                369405 NA     NA                    
#> 3 E40000010 North W… Persons                  4250 NA     NA                    
#> 4 E40000011 Midlands Persons                148330 NA     NA                    
#> # ℹ abbreviated name: ¹​HighestPriorityNotificationType
#> # ℹ 12 more variables: IndicatorCode <chr>, IndicatorName <chr>,
#> #   IndicatorShortName <chr>, LowerConfidenceLimit <dbl>,
#> #   MetricCategoryName <chr>, MetricCategoryTypeName <chr>,
#> #   NotificationCount <int>, Numerator <dbl>, TimePeriodName <chr>,
#> #   UpperConfidenceLimit <dbl>, Value <dbl>, ValueNote <lgl>

# Find a valid indicator IDs for a specified time period and system level,
# then retrieve raw data for one of these
indicators <- cvd_indicator_list(time_period_id = 22, system_level_id = 4)
cvd_indicator_raw_data(
  time_period_id = 22,
  system_level_id = 4,
  indicator_id = indicators$IndicatorID[1]
)
#> # A tibble: 24,349 × 18
#>    AreaCode AreaName CategoryAttribute Denominator Factor HighestPriorityNotif…¹
#>    <chr>    <chr>    <chr>                   <dbl> <lgl>  <chr>                 
#>  1 U60176   3 Centr… Persons                  6080 NA     Red                   
#>  2 U60176   3 Centr… Female                   2880 NA     Red                   
#>  3 U60176   3 Centr… Male                     3200 NA     Red                   
#>  4 U60176   3 Centr… Male                      105 NA     Red                   
#>  5 U60176   3 Centr… Female                     80 NA     Red                   
#>  6 U60176   3 Centr… Persons                   185 NA     Red                   
#>  7 U60176   3 Centr… Male                     1110 NA     Red                   
#>  8 U60176   3 Centr… Female                    905 NA     Red                   
#>  9 U60176   3 Centr… Persons                  2015 NA     Red                   
#> 10 U60176   3 Centr… Male                     1985 NA     Red                   
#> # ℹ 24,339 more rows
#> # ℹ abbreviated name: ¹​HighestPriorityNotificationType
#> # ℹ 12 more variables: IndicatorCode <chr>, IndicatorName <chr>,
#> #   IndicatorShortName <chr>, LowerConfidenceLimit <dbl>,
#> #   MetricCategoryName <chr>, MetricCategoryTypeName <chr>,
#> #   NotificationCount <int>, Numerator <dbl>, TimePeriodName <chr>,
#> #   UpperConfidenceLimit <dbl>, Value <dbl>, ValueNote <chr>
# }
```
