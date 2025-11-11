# Data availability

Returns the data availability. Response: `DataAvailabilityID` - ID of
the resource as found in the database `DataAvailabilityName` -
explanation for the data availability `IsAvailable` - `Y` for data is
available, `N` for data is unavailable, and NULL for unknown data

## Usage

``` r
cvd_data_availability(
  time_period_id,
  system_level_id,
  indicator_id = NULL,
  metric_category_type_id = NULL
)
```

## Arguments

- time_period_id:

  integer - the time period to return data for (compulsory)

- system_level_id:

  integer - the system level to return data for (compulsory)

- indicator_id:

  integer - the indicator to return data for (optional)

- metric_category_type_id:

  integer - the metric category to return data for (optional)

## Value

Tibble of data availability

## Details

CVD Prevent API documentation: [Data
availability](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2FdataAvailability)

## Note

This function may take longer than 5 seconds to complete due to API
response time.

## See also

[`cvd_external_resource()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_external_resource.md)

## Examples

``` r
# \donttest{
cvd_data_availability(time_period_id = 3, system_level_id = 5)
#> # A tibble: 132 × 15
#>    DataAvailabilityID DataAvailabilityName  HighestPriorityNotif…¹ IndicatorCode
#>                 <int> <chr>                 <chr>                  <chr>        
#>  1                 93 Inequality breakdown… NA                     CVDP001AF    
#>  2                 94 Inequality breakdown… NA                     CVDP001AF    
#>  3                 95 Inequality breakdown… NA                     CVDP001AF    
#>  4                 96 Inequality breakdown… NA                     CVDP001AF    
#>  5               1245 Indicators not avail… Red                    CVDP002HYP   
#>  6               1246 Indicators not avail… Red                    CVDP002HYP   
#>  7               1247 Indicators not avail… Red                    CVDP002HYP   
#>  8               1248 Indicators not avail… Red                    CVDP002HYP   
#>  9               1389 Indicators not avail… Red                    CVDP003HYP   
#> 10               1390 Indicators not avail… Red                    CVDP003HYP   
#> # ℹ 122 more rows
#> # ℹ abbreviated name: ¹​HighestPriorityNotificationType
#> # ℹ 11 more variables: IndicatorID <int>, IndicatorName <chr>,
#> #   IndicatorOrder <int>, IndicatorShortName <chr>, IsAvailable <chr>,
#> #   MetricCategoryTypeID <int>, NotificationCount <int>, SystemLevelID <int>,
#> #   SystemLevelName <chr>, TimePeriodID <int>, TimePeriodName <chr>
# }
```
