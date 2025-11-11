# List indicators and associated metrics for a system level and time period

Retrieves all CVD indicators available for a given reporting period and
given system level from the CVDPREVENT API, with an expanded view that
includes 'MetricList' array for each indicator. This allows you to see
not only which indicators are available, but also the specific metrics
(e.g., breakdowns by age, sex or other attributes) associated with each
indicator in the selected context.

Only indicators with available data for the specified time period and
system level are returned. This function is useful for determining what
granular metric breakdowns are provided for each indicator.

## Usage

``` r
cvd_indicator_metric_list(time_period_id, system_level_id)
```

## Arguments

- time_period_id:

  Integer (required). The reporting period (time period) for which to
  return indicators and metrics. use
  [`cvd_time_period_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_time_period_list.md)
  to find valid IDs.

- system_level_id:

  Integer (required). The system level (e.g., National, Region, ICB,
  PCN, Practice) for which to return indicators and metrics. Use
  [`cvd_area_system_level()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_system_level.md)
  to find valid IDs for a given time period.

## Value

A tibble containing one row for each indicator-metric pair avialable for
the specified system level and time period. Columns typically include:

- AxisCharacter:

  Character. Symbol used to represent the metric axis (e.g., "%").

- FormatDisplayName:

  Character. Display format for the metric (e.g., "Proportion %").

- HighestPriorityNotificationType:

  Character. Notification priority level, if applicable (e.g., "Red").
  Often blank.

- IndicatorCode:

  Character. Unique code for the indicator (e.g., "CVDP002SMOK").

- IndicatorFormatID:

  Integer. Internal ID for the indicator's format type.

- IndicatorID:

  Integer. Unique identifier for the indicator.

- IndicatorName:

  Character. Full descriptive name of the indicator.

- IndicatorOrder:

  Integer. Display order for the indicator in dashboards or reports.

- IndicatorShortName:

  Character. Abbreviated name of the indicator for display purposes.

- NotificationCount:

  Integer. Count of notifications associated with the indicator.

- CategoryAttribute:

  Character. Label used to group individuals (e.g., "Male", "Persons").

- MetricCategoryName:

  Character. Name of the subgroup or category (e.g., "40–59", "Mixed").

- MetricCategoryTypeName:

  Character. Type of subgroup (e.g., "Age group", "Sex", "Ethnicity").

- MetricID:

  Integer. Unique identifier for the specific metric being measured.

If no indicators or metrics are found returns a tibble describing the
error.

## Details

Use this function to explore the detailed metric breakdowns available
for each indicator before performing data extraction or analysis. The
`MetricList` column is unnested for convenience, so each row represents
a single metric linked to an indicator.

## Note

This function may take longer than 5 seconds to complete due to API
response time.

## API Documentation

See the [CVDPREVENT API documentation: Indicator metric
list](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#*Proposed*%2Findicator%2FmetricList)

## Examples

``` r
# \donttest{
# List metrics for the prevalence of atrial fibrillation (indicator ID 1),
# focusing on metrics for the 40-59 years age group at the national level:
cvd_indicator_metric_list(time_period_id = 17, system_level_id = 1) |>
  dplyr::filter(IndicatorID == 1, MetricCategoryName == "40-59") |>
  dplyr::count(IndicatorID, IndicatorShortName, MetricID, MetricCategoryName, CategoryAttribute) |>
  dplyr::select(-n)
#> # A tibble: 3 × 5
#>   IndicatorID IndicatorShortName   MetricID MetricCategoryName CategoryAttribute
#>         <int> <chr>                   <int> <chr>              <chr>            
#> 1           1 AF: Prevalence (CVD…      173 40-59              Female           
#> 2           1 AF: Prevalence (CVD…      175 40-59              Male             
#> 3           1 AF: Prevalence (CVD…      187 40-59              Persons          

# Get all indicator-metric pairs for GP practice level (system level 5) in a given period
cvd_indicator_metric_list(time_period_id = 17, system_level_id = 5)
#> # A tibble: 32 × 14
#>    AxisCharacter FormatDisplayName HighestPriorityNotificationType IndicatorCode
#>    <chr>         <chr>             <chr>                           <chr>        
#>  1 %             Proportion %      NA                              CVDP002CKD   
#>  2 %             Proportion %      NA                              CVDP005HYP   
#>  3 %             Proportion %      NA                              CVDP010CHOL  
#>  4 %             Proportion %      NA                              CVDP001SMOK  
#>  5 %             Proportion %      NA                              CVDP002NDH   
#>  6 %             Proportion %      NA                              CVDP009HYP   
#>  7 %             Proportion %      NA                              CVDP002AF    
#>  8 %             Proportion %      NA                              CVDP003CKD   
#>  9 %             Proportion %      NA                              CVDP002SMOK  
#> 10 %             Proportion %      NA                              CVDP003DM    
#> # ℹ 22 more rows
#> # ℹ 10 more variables: IndicatorFormatID <int>, IndicatorID <int>,
#> #   IndicatorName <chr>, IndicatorOrder <int>, IndicatorShortName <chr>,
#> #   NotificationCount <int>, CategoryAttribute <chr>, MetricCategoryName <chr>,
#> #   MetricCategoryTypeName <chr>, MetricID <int>
# }
```
