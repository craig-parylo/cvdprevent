# Retrieve CVD indicator data for a specific area and time period

Fetches all available metric breakdowns for a single cardiovascular
disease (CVD) indicator from the CVDPREVENT API, scoped to a specified
NHS area and reporting period. This includes subgroup data such as age,
sex, ethnicity, deprivation quintile, and more.

## Usage

``` r
cvd_indicator_data(time_period_id, area_id, indicator_id)
```

## Arguments

- time_period_id:

  Integer (required). The reporting period (time period) to retrieve
  data for. Use
  [`cvd_time_period_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_time_period_list.md)
  to find valid IDs.

- area_id:

  Integer (required). The AreaID for which to retrieve indicator data.
  use
  [`cvd_area_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_list.md)
  or
  [`cvd_area_search()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_search.md)
  to find valid IDs.

- indicator_id:

  Integer (required). The IndicatorID for which to retrieve data. use
  [`cvd_indicator_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_list.md)
  or
  [`cvd_indicator_metric_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_metric_list.md)
  to find valid IDs.

## Value

A named list containing three tibbles:

- indicator_metrics:

  Tibble. Metadata and definitions for the selected indicator and its
  associated metrics.

- area_data:

  Tibble. Metric values for the specified NHS area (`area_id`) across
  all available breakdowns.

- national_data:

  Tibble. Metric values for England, used for benchmarking and
  comparison.

If no indicator data is found, returns a tibble describing the error.

**indicator_metrics** contains the following items:

- AxisCharacter:

  Character. Symbol used to represent the metric axis (e.g., "%").

- FormatDisplayName:

  Character. Display format for the metric (e.g., "Proportion %").

- IndicatorCode:

  Character. Unique code for the indicator (e.g., "CVDP002AF").

- IndicatorFormatID:

  Integer. Internal ID for the indicator's format type.

- IndicatorID:

  Integer. Unique identifier for the indicator.

- IndicatorName:

  Character. Full descriptive name of the indicator.

- IndicatorOrder:

  Integer. Display order for the indicator in dashboards or reports.

- IndicatorShortName:

  Character. Abbreviated name of the indicator for display.

- NotificationCount:

  Integer. Count of notifications associated with the indicator. Often
  zero.

- TimePeriodID:

  Integer. ID of the reporting time period.

- TimePeriodName:

  Character. Label for the reporting time period (e.g., "To March
  2024").

- CategoryAttribute:

  Character. Subgroup label (e.g., "Male", "Female", "Persons").

- MetricCategoryID:

  Integer. Unique ID for the metric category.

- MetricCategoryName:

  Character. Name of the subgroup or category (e.g., "18–39", "Sex").

- MetricCategoryOrder:

  Integer. Display order for the metric category.

- MetricCategoryTypeName:

  Character. Type of subgroup (e.g., "Age group", "Sex").

- MetricID:

  Integer. Unique ID for the specific metric being measured.

**area_data** and **national_data** contain the following items:

- MetricID:

  Integer. Unique identifier for the metric being measured.

- MetricCategoryTypeName:

  Character. Type of subgroup (e.g., "Sex", "Age group").

- MetricCategoryName:

  Character. Name of the subgroup (e.g., "Female", "18–39").

- CategoryAttribute:

  Character. Label used to group individuals (e.g., "Male", "Persons").

- AreaCode:

  Character. ONS code for the NHS area (e.g., "U60176").

- AreaID:

  Integer. Internal ID for the NHS area.

- AreaName:

  Character. Name of the NHS area (e.g., "3 Centres PCN").

- Count:

  Integer. Total number of individuals in the subgroup.

- DataID:

  Integer. Unique identifier for the data record.

- Denominator:

  Numeric. Population or count used as the denominator in metric
  calculation.

- Factor:

  Numeric. Scaling factor applied to the metric, if applicable. Often
  NA.

- HighestPriorityNotificationType:

  Character. Notification priority level, if available. Often NA.

- LowerConfidenceLimit:

  Numeric. Lower bound of the confidence interval for the metric value.

- Max:

  Numeric. Maximum observed value for the metric across comparable
  areas.

- Median:

  Numeric. Median value for the metric across comparable areas.

- Min:

  Numeric. Minimum observed value for the metric across comparable
  areas.

- NotificationCount:

  Integer. Count of notifications associated with the indicator. Often
  zero.

- Numerator:

  Numeric. Count used as the numerator in metric calculation.

- Q20:

  Numeric. 20th percentile value across comparable areas.

- Q40:

  Numeric. 40th percentile value across comparable areas.

- Q60:

  Numeric. 60th percentile value across comparable areas.

- Q80:

  Numeric. 80th percentile value across comparable areas.

- TimePeriodID:

  Integer. ID of the reporting time period.

- TimePeriodName:

  Character. Label for the reporting time period (e.g., "To March
  2024").

- UpperConfidenceLimit:

  Numeric. Upper bound of the confidence interval for the metric value.

- Value:

  Numeric. Calculated metric value (e.g., percentage of patients
  treated).

- ValueNote:

  Character. Additional notes or flags about the value. Often NA.

## Details

Use this function to obtain all metric values for a single indicator in
a particular area and time period, such as for a local dashboard or a
focussed report. For broader queries across multiple indicators, see
[`cvd_indicator()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator.md)
or
[`cvd_indicator_metric_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_metric_list.md).

## Note

This function may take longer than 5 seconds to complete due to API
response time.

## API Documentation

See the [CVDPREVENT API documentation: Indicator
data](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Findicator%2Fdata)

## See also

[`cvd_indicator_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_list.md),
[`cvd_indicator_metric_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_metric_list.md),
[`cvd_indicator()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator.md),
[`cvd_indicator_tags()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_tags.md),
[`cvd_indicator_details()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_details.md),
[`cvd_indicator_sibling()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_sibling.md),
[`cvd_indicator_child_data()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_child_data.md),
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
# Retrieve all metric breakdowns for indicator 7 in area 1103 for time period 17
returned_list <- cvd_indicator_data(time_period_id = 17, area_id = 1103, indicator_id = 7)

# See the structure of this data
returned_list |> dplyr::glimpse()
#> List of 3
#>  $ indicator_metrics: tibble [22 × 17] (S3: tbl_df/tbl/data.frame)
#>   ..$ AxisCharacter         : chr [1:22] "%" "%" "%" "%" ...
#>   ..$ FormatDisplayName     : chr [1:22] "Proportion %" "Proportion %" "Proportion %" "Proportion %" ...
#>   ..$ IndicatorCode         : chr [1:22] "CVDP002AF" "CVDP002AF" "CVDP002AF" "CVDP002AF" ...
#>   ..$ IndicatorFormatID     : int [1:22] 1 1 1 1 1 1 1 1 1 1 ...
#>   ..$ IndicatorID           : int [1:22] 7 7 7 7 7 7 7 7 7 7 ...
#>   ..$ IndicatorName         : chr [1:22] "Patients with GP recorded atrial fibrillation and with a CHADS2 or CHA2DS2-VASc score of 2 or more, who are cur"| __truncated__ "Patients with GP recorded atrial fibrillation and with a CHADS2 or CHA2DS2-VASc score of 2 or more, who are cur"| __truncated__ "Patients with GP recorded atrial fibrillation and with a CHADS2 or CHA2DS2-VASc score of 2 or more, who are cur"| __truncated__ "Patients with GP recorded atrial fibrillation and with a CHADS2 or CHA2DS2-VASc score of 2 or more, who are cur"| __truncated__ ...
#>   ..$ IndicatorOrder        : int [1:22] 2 2 2 2 2 2 2 2 2 2 ...
#>   ..$ IndicatorShortName    : chr [1:22] "AF: Treated with anticoagulants (CVDP002AF)" "AF: Treated with anticoagulants (CVDP002AF)" "AF: Treated with anticoagulants (CVDP002AF)" "AF: Treated with anticoagulants (CVDP002AF)" ...
#>   ..$ NotificationCount     : int [1:22] 0 0 0 0 0 0 0 0 0 0 ...
#>   ..$ TimePeriodID          : num [1:22] 17 17 17 17 17 17 17 17 17 17 ...
#>   ..$ TimePeriodName        : chr [1:22] "To March 2024" "To March 2024" "To March 2024" "To March 2024" ...
#>   ..$ CategoryAttribute     : chr [1:22] "Persons" "Female" "Male" "Male" ...
#>   ..$ MetricCategoryID      : int [1:22] 30 28 29 4 5 6 7 8 9 10 ...
#>   ..$ MetricCategoryName    : chr [1:22] "Persons" "Female" "Male" "18-39" ...
#>   ..$ MetricCategoryOrder   : int [1:22] 1 2 3 7 8 9 10 11 12 13 ...
#>   ..$ MetricCategoryTypeName: chr [1:22] "Sex" "Sex" "Sex" "Age group" ...
#>   ..$ MetricID              : int [1:22] 150 125 124 127 131 134 126 132 135 128 ...
#>  $ area_data        : tibble [22 × 27] (S3: tbl_df/tbl/data.frame)
#>   ..$ MetricID                       : int [1:22] 150 125 124 127 131 134 126 132 135 128 ...
#>   ..$ MetricCategoryTypeName         : chr [1:22] "Sex" "Sex" "Sex" "Age group" ...
#>   ..$ MetricCategoryName             : chr [1:22] "Persons" "Female" "Male" "18-39" ...
#>   ..$ CategoryAttribute              : chr [1:22] "Persons" "Female" "Male" "Male" ...
#>   ..$ AreaCode                       : chr [1:22] "U60176" "U60176" "U60176" NA ...
#>   ..$ AreaID                         : num [1:22] 1103 1103 1103 NA NA ...
#>   ..$ AreaName                       : chr [1:22] "3 Centres PCN" "3 Centres PCN" "3 Centres PCN" NA ...
#>   ..$ Count                          : num [1:22] 1274 1274 1274 NA NA ...
#>   ..$ DataID                         : num [1:22] 7985473 7968399 7967142 NA NA ...
#>   ..$ Denominator                    : num [1:22] 710 305 405 NA NA NA 15 10 25 215 ...
#>   ..$ Factor                         : logi [1:22] NA NA NA NA NA NA ...
#>   ..$ HighestPriorityNotificationType: logi [1:22] NA NA NA NA NA NA ...
#>   ..$ LowerConfidenceLimit           : num [1:22] 89.9 88.3 89.4 NA NA ...
#>   ..$ Max                            : num [1:22] 97.2 98.6 100 NA NA ...
#>   ..$ Median                         : num [1:22] 91.7 91.5 92 NA NA ...
#>   ..$ Min                            : num [1:22] 54.8 52.4 56.5 NA NA ...
#>   ..$ NotificationCount              : int [1:22] 0 0 0 0 0 0 0 0 0 0 ...
#>   ..$ Numerator                      : num [1:22] 650 280 375 NA NA NA 15 10 25 195 ...
#>   ..$ Q20                            : num [1:22] 54.8 52.4 56.5 NA NA ...
#>   ..$ Q40                            : num [1:22] 90.2 89.5 90.4 NA NA ...
#>   ..$ Q60                            : num [1:22] 91.7 91.5 92 NA NA ...
#>   ..$ Q80                            : num [1:22] 93.2 93.1 93.5 NA NA ...
#>   ..$ TimePeriodID                   : num [1:22] 17 17 17 NA NA NA 17 17 17 17 ...
#>   ..$ TimePeriodName                 : chr [1:22] "To March 2024" "To March 2024" "To March 2024" "To March 2024" ...
#>   ..$ UpperConfidenceLimit           : num [1:22] 93.8 94.5 94.6 NA NA ...
#>   ..$ Value                          : num [1:22] 92.1 91.8 92.3 NA NA ...
#>   ..$ ValueNote                      : chr [1:22] NA NA NA NA ...
#>  $ national_data    : tibble [22 × 26] (S3: tbl_df/tbl/data.frame)
#>   ..$ MetricID                       : int [1:22] 150 125 124 127 131 134 126 132 135 128 ...
#>   ..$ MetricCategoryTypeName         : chr [1:22] "Sex" "Sex" "Sex" "Age group" ...
#>   ..$ MetricCategoryName             : chr [1:22] "Persons" "Female" "Male" "18-39" ...
#>   ..$ CategoryAttribute              : chr [1:22] "Persons" "Female" "Male" "Male" ...
#>   ..$ AreaCode                       : chr [1:22] "E92000001" "E92000001" "E92000001" "E92000001" ...
#>   ..$ AreaID                         : int [1:22] 1 1 1 1 1 1 1 1 1 1 ...
#>   ..$ AreaName                       : chr [1:22] "England" "England" "England" "England" ...
#>   ..$ Count                          : logi [1:22] NA NA NA NA NA NA ...
#>   ..$ DataID                         : int [1:22] 7962889 7962864 7962863 7962872 7962868 7962873 7962866 7962867 7962874 7962871 ...
#>   ..$ Denominator                    : num [1:22] 1012168 454503 557665 354 426 ...
#>   ..$ Factor                         : logi [1:22] NA NA NA NA NA NA ...
#>   ..$ HighestPriorityNotificationType: logi [1:22] NA NA NA NA NA NA ...
#>   ..$ LowerConfidenceLimit           : num [1:22] 91.4 91.1 91.7 69.2 57.8 ...
#>   ..$ Max                            : logi [1:22] NA NA NA NA NA NA ...
#>   ..$ Median                         : logi [1:22] NA NA NA NA NA NA ...
#>   ..$ Min                            : logi [1:22] NA NA NA NA NA NA ...
#>   ..$ NotificationCount              : int [1:22] 0 0 0 0 0 0 0 0 0 0 ...
#>   ..$ Numerator                      : num [1:22] 925978 414349 511629 262 266 ...
#>   ..$ Q20                            : logi [1:22] NA NA NA NA NA NA ...
#>   ..$ Q40                            : logi [1:22] NA NA NA NA NA NA ...
#>   ..$ Q60                            : logi [1:22] NA NA NA NA NA NA ...
#>   ..$ Q80                            : logi [1:22] NA NA NA NA NA NA ...
#>   ..$ TimePeriodID                   : int [1:22] 17 17 17 17 17 17 17 17 17 17 ...
#>   ..$ UpperConfidenceLimit           : num [1:22] 91.5 91.2 91.8 78.2 66.9 ...
#>   ..$ Value                          : num [1:22] 91.5 91.2 91.7 74 62.4 ...
#>   ..$ ValueNote                      : logi [1:22] NA NA NA NA NA NA ...

# See the definition for one metric
returned_list$indicator_metrics |>
  dplyr::slice_head(n = 1) |>
  dplyr::glimpse()
#> Rows: 1
#> Columns: 17
#> $ AxisCharacter          <chr> "%"
#> $ FormatDisplayName      <chr> "Proportion %"
#> $ IndicatorCode          <chr> "CVDP002AF"
#> $ IndicatorFormatID      <int> 1
#> $ IndicatorID            <int> 7
#> $ IndicatorName          <chr> "Patients with GP recorded atrial fibrillation …
#> $ IndicatorOrder         <int> 2
#> $ IndicatorShortName     <chr> "AF: Treated with anticoagulants (CVDP002AF)"
#> $ NotificationCount      <int> 0
#> $ TimePeriodID           <dbl> 17
#> $ TimePeriodName         <chr> "To March 2024"
#> $ CategoryAttribute      <chr> "Persons"
#> $ MetricCategoryID       <int> 30
#> $ MetricCategoryName     <chr> "Persons"
#> $ MetricCategoryOrder    <int> 1
#> $ MetricCategoryTypeName <chr> "Sex"
#> $ MetricID               <int> 150

# Compare performance in the specified area (AreaID = 1103) with national results for
# women aged 40-59 years (MetricID = 132)
dplyr::bind_rows(
  returned_list$area_data,
  returned_list$national_data
) |>
dplyr::filter(MetricID == 132)
#> # A tibble: 2 × 27
#>   MetricID MetricCategoryTypeName MetricCategoryName CategoryAttribute AreaCode 
#>      <int> <chr>                  <chr>              <chr>             <chr>    
#> 1      132 Age group              40-59              Female            U60176   
#> 2      132 Age group              40-59              Female            E92000001
#> # ℹ 22 more variables: AreaID <dbl>, AreaName <chr>, Count <dbl>, DataID <dbl>,
#> #   Denominator <dbl>, Factor <lgl>, HighestPriorityNotificationType <lgl>,
#> #   LowerConfidenceLimit <dbl>, Max <dbl>, Median <dbl>, Min <dbl>,
#> #   NotificationCount <int>, Numerator <dbl>, Q20 <dbl>, Q40 <dbl>, Q60 <dbl>,
#> #   Q80 <dbl>, TimePeriodID <dbl>, TimePeriodName <chr>,
#> #   UpperConfidenceLimit <dbl>, Value <dbl>, ValueNote <chr>
# }
```
