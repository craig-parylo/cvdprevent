# List all available indicator tags

Retrieves a list of all tags from the CVDPREVENT API that can be used to
filter indicators. Tags provide a way to categorise and search for
indicators by clinical or reporting groupings (such as "Priority Group",
"Pathway Group" or other clinical categories).

Use this function to obtain valid tag IDs for use in functions that
support filtering by tag, such as
[`cvd_indicator()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator.md).

## Usage

``` r
cvd_indicator_tags()
```

## Value

A tibble with one row per available indicator tag, containing the
following colunns:

- IndicatorTagID:

  Integer. Unique identifier for the tag associated with an indicator.

- IndicatorTagName:

  Character. Descriptive label categorising the indicator (e.g.,
  "monitoring", "prevention", "smoking").

If no tags are found, returns a tibble describing the error.

## Details

Tags are useful for grouping or filtering indicators in dashboards,
reports or scripted analyses. Tag IDs returned by this function can be
supplied to functions like
[`cvd_indicator()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator.md)
via the `tag_id` argument for targeted queries.

## Note

This function may take longer than 5 seconds to complete due to API
response time.

## API Documentation

See the [CVDPREVENT API documentation: Indicator
tags](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Findicator%2Ftags)

## See also

[`cvd_indicator_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_list.md),
[`cvd_indicator_metric_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_metric_list.md),
[`cvd_indicator()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator.md),
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
# List the first five indicator tags
cvd_indicator_tags() |>
  dplyr::arrange(IndicatorTagID) |>
  dplyr::slice_head(n = 5)
#> # A tibble: 5 × 2
#>   IndicatorTagID IndicatorTagName       
#>            <int> <chr>                  
#> 1              1 prevalence             
#> 2              2 atrial fibrillation    
#> 3              3 hypertension           
#> 4              4 blood pressure measures
#> 5              5 cardiovascular disease 

# Use a tag ID to filter indicators in another query
tags <- cvd_indicator_tags()
tag_id <- tags$IndicatorTagID[1]
cvd_indicator(time_period_id = 17, area_id = 3, tag_id = tag_id)
#> $indicators
#> # A tibble: 8 × 14
#>   AxisCharacter DataUpdateInterval FormatDisplayName      HighestPriorityNotif…¹
#>   <chr>         <lgl>              <chr>                  <chr>                 
#> 1 "%"           NA                 Proportion %           NA                    
#> 2 "%"           NA                 Proportion %           NA                    
#> 3 "%"           NA                 Proportion %           NA                    
#> 4 "%"           NA                 Proportion %           NA                    
#> 5 "%"           NA                 Proportion %           NA                    
#> 6 "%"           NA                 Proportion %           NA                    
#> 7 "%"           NA                 Proportion %           NA                    
#> 8 " "           NA                 Rate per 10,000 patie… Blue                  
#> # ℹ abbreviated name: ¹​HighestPriorityNotificationType
#> # ℹ 10 more variables: IndicatorCode <chr>, IndicatorFormatID <int>,
#> #   IndicatorID <int>, IndicatorName <chr>, IndicatorOrder <int>,
#> #   IndicatorShortName <chr>, IndicatorStatus <lgl>, IndicatorTypeID <int>,
#> #   IndicatorTypeName <chr>, NotificationCount <int>
#> 
#> $metric_categories
#> # A tibble: 144 × 7
#>    IndicatorID CategoryAttribute MetricCategoryID MetricCategoryName
#>          <int> <chr>                        <int> <chr>             
#>  1           1 Persons                         30 Persons           
#>  2           1 Female                          28 Female            
#>  3           1 Male                            29 Male              
#>  4           1 Male                             4 18-39             
#>  5           1 Female                           5 18-39             
#>  6           1 Persons                          6 18-39             
#>  7           1 Male                             7 40-59             
#>  8           1 Female                           8 40-59             
#>  9           1 Persons                          9 40-59             
#> 10           1 Male                            10 60-79             
#> # ℹ 134 more rows
#> # ℹ 3 more variables: MetricCategoryOrder <int>, MetricCategoryTypeName <chr>,
#> #   MetricID <int>
#> 
#> $metric_data
#> # A tibble: 144 × 1
#>    MetricID
#>       <int>
#>  1      190
#>  2      171
#>  3      172
#>  4      177
#>  5      180
#>  6      186
#>  7      175
#>  8      173
#>  9      187
#> 10      179
#> # ℹ 134 more rows
#> 
#> $timeseries_data
#> # A tibble: 495 × 7
#>    MetricID EndDate           Median StartDate TimePeriodID TimePeriodName Value
#>       <int> <chr>              <dbl> <chr>            <int> <chr>          <dbl>
#>  1      190 Tue, 31 Mar 2020…   2.65 Mon, 01 …            1 To March 2020   2.4 
#>  2      190 Wed, 31 Mar 2021…   2.5  Mon, 01 …            2 To March 2021   2.4 
#>  3      190 Thu, 30 Sep 2021…   2.5  Mon, 01 …            3 To September …  2.3 
#>  4      190 Thu, 31 Mar 2022…   2.54 Mon, 01 …            4 To March 2022   2.32
#>  5      171 Tue, 31 Mar 2020…   2.2  Mon, 01 …            1 To March 2020   2   
#>  6      171 Wed, 31 Mar 2021…   2.1  Mon, 01 …            2 To March 2021   2   
#>  7      171 Thu, 30 Sep 2021…   2.1  Mon, 01 …            3 To September …  2   
#>  8      171 Thu, 31 Mar 2022…   2.11 Mon, 01 …            4 To March 2022   1.94
#>  9      172 Tue, 31 Mar 2020…   3.1  Mon, 01 …            1 To March 2020   2.8 
#> 10      172 Wed, 31 Mar 2021…   3    Mon, 01 …            2 To March 2021   2.8 
#> # ℹ 485 more rows
#> 
# }
```
