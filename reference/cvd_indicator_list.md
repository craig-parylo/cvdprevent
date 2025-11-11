# List available indicators for a system level and time period

Retrieves basic details for all CVD indicators available for a given
system level and reportion period from the CVDPREVENT API. Only
indicators with data for the selected time period and system level are
returned. This function is commonly used to populate indicator pickers
or to discover what data is available for further queries.

## Usage

``` r
cvd_indicator_list(time_period_id, system_level_id)
```

## Arguments

- time_period_id:

  Integer (required). The reporting period (time period) for which to
  return indicators. Use
  [`cvd_time_period_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_time_period_list.md)
  to find valid IDs.

- system_level_id:

  Integer (required). The system level (e.g., National, Region, ICB,
  PCN, Practice) for which to return indicators. Use
  [`cvd_area_system_level()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_system_level.md)
  to find valid IDs for a given time period.

## Value

A tibble with one row per available indicator for the specified system
level and time period. Typical columns include:

- AxisCharacter:

  Character. Symbol used to represent the metric axis (e.g., "%").

- DataUpdateInterval:

  Character. Frequency or interval at which the indicator data is
  updated. Often blank.

- FormatDisplayName:

  Character. Display format for the metric (e.g., "Proportion %").

- HighestPriorityNotificationType:

  Character. Notification priority level, if applicable (e.g., "Red").
  Often blank.

- IndicatorCode:

  Character. Unique code for the indicator (e.g., "CVDP005CKD").

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

- IndicatorStatus:

  Character. Status of the indicator (e.g., active, retired). Often
  blank.

- NotificationCount:

  Integer. Count of notifications associated with the indicator.

If no indicators are found, returns a tibble describing the error.

## Details

Use this function to discover which indicators are available for a
specific combination of system level and time period. The results can be
joined with other outputs for further analysis, or used as the basis for
more detailed indicator, metric, or data queries.

## Note

This function may take longer than 5 seconds to complete due to API
response time.

## API Docuemtnation

See the [CVDPREVENT API documentation: Indicator
list](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Findicator%2Flist)

## See also

[`cvd_indicator_metric_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_metric_list.md),
[`cvd_indicator()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator.md),
[`cvd_indicator_tags()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_tags.md),
[`cvd_indicator_details()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_details.md),
[`cvd_indicator_sibling()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_sibling.md),
[`cvd_indicator_child_data()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_child_data.md),
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
# List four indicators for time period 17 and GP practice level (system level 5)
cvd_indicator_list(time_period_id = 17, system_level_id = 5) |>
  dplyr::select(IndicatorID, IndicatorCode, IndicatorShortName) |>
  dplyr::slice_head(n = 4)
#> # A tibble: 4 × 3
#>   IndicatorID IndicatorCode IndicatorShortName                                  
#>         <int> <chr>         <chr>                                               
#> 1           2 CVDP002HYP    Hypertension: Treated to appropriate threshold (age…
#> 2           3 CVDP003HYP    Hypertension: Treated to appropriate threshold (age…
#> 3           4 CVDP004HYP    Hypertension: BP monitoring (CVDP004HYP)            
#> 4           7 CVDP002AF     AF: Treated with anticoagulants (CVDP002AF)         

# Find valid time period and system level IDs, then list all available indicators
valid_periods <- cvd_time_period_list()
valid_levels <- cvd_area_system_level(time_period_id = 17)
cvd_indicator_list(time_period_id = 17, system_level_id = valid_levels$SystemLevelID[1])
#> # A tibble: 40 × 12
#>    AxisCharacter DataUpdateInterval FormatDisplayName     HighestPriorityNotif…¹
#>    <chr>         <lgl>              <chr>                 <chr>                 
#>  1 "%"           NA                 Proportion %          NA                    
#>  2 "%"           NA                 Proportion %          Red                   
#>  3 "%"           NA                 Proportion %          Red                   
#>  4 "%"           NA                 Proportion %          NA                    
#>  5 "%"           NA                 Proportion %          NA                    
#>  6 "%"           NA                 Proportion %          NA                    
#>  7 " "           NA                 Rate per 10,000 pati… Blue                  
#>  8 " "           NA                 Rate per 10,000 pati… Blue                  
#>  9 "%"           NA                 Proportion %          NA                    
#> 10 "%"           NA                 Proportion %          NA                    
#> # ℹ 30 more rows
#> # ℹ abbreviated name: ¹​HighestPriorityNotificationType
#> # ℹ 8 more variables: IndicatorCode <chr>, IndicatorFormatID <int>,
#> #   IndicatorID <int>, IndicatorName <chr>, IndicatorOrder <int>,
#> #   IndicatorShortName <chr>, IndicatorStatus <lgl>, NotificationCount <int>
# }
```
