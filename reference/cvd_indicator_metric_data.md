# Retrieve metric data for a specific metric, time period and area

Returns detailed area data for a single CVD metric (`metric_id`) for a
specified NHS area (`area_id`) and reporting period (`time_period_id`)
from the CVDPREVENT API. This function provides all values and
breakdowns available for the selected metric within the chosen context,
allowing analysis and visualisation of precise, granular results (e.g.,
age groups, sexes, ethnicities).

## Usage

``` r
cvd_indicator_metric_data(metric_id, time_period_id, area_id)
```

## Arguments

- metric_id:

  Integer (required). The MetricID for which to retrieve values. Use
  [`cvd_indicator_metric_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_metric_list.md)
  or
  [`cvd_indicator_data()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_data.md)
  to find valid MetricIDs.

- time_period_id:

  Integer (required). The reporting period (time period) for which to
  return metric data. Use
  [`cvd_time_period_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_time_period_list.md)
  to find valid IDs.

- area_id:

  Integer (required). The AreaID for which to return metric data. Use
  [`cvd_area_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_list.md)
  or
  [`cvd_area_search()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_search.md)
  to find valid IDs.

## Value

A named list containing three tibbles:

- metrics:

  Tibble. Metadata and definitions for the selected metric.

- area_data:

  Tibble. Metric values for the specified NHS area (`area_id`).

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

Use this function to retrieve all available breakdowns for a metric in a
specific area and period, such as for in-depth local reporting,
dashboard figures, or subgroup analysis. It is best used when you know
the exact metric required for your query.

## Note

This function may take longer than 5 seconds to complete due to API
response time.

## API Documentation

See the [CVDPREVENT API documentation: Indicator metric
data](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Findicator%2FmetricData)

## See also

[`cvd_indicator_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_list.md),
[`cvd_indicator_metric_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_metric_list.md),
[`cvd_indicator()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator.md),
[`cvd_indicator_tags()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_tags.md),
[`cvd_indicator_details()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_details.md),
[`cvd_indicator_sibling()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_sibling.md),
[`cvd_indicator_child_data()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_child_data.md),
[`cvd_indicator_data()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_data.md),
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
# Retrieve  a single metric breakdown showing how men aged 40-59 years
# are treated with anticoagulants (MetricID = 126) for 3 Centres PCN
# (AreaID = 1103) to March 2020 (TimePeriodID = 1)
returned_list <- cvd_indicator_metric_data(
  time_period_id = 1,
  area_id = 1103,
  metric_id = 126
)

# See the structure of this data
returned_list |> dplyr::glimpse()
#> List of 3
#>  $ metrics      : tibble [1 × 17] (S3: tbl_df/tbl/data.frame)
#>   ..$ AxisCharacter         : chr "%"
#>   ..$ FormatDisplayName     : chr "Proportion %"
#>   ..$ IndicatorCode         : chr "CVDP002AF"
#>   ..$ IndicatorFormatID     : int 1
#>   ..$ IndicatorID           : int 7
#>   ..$ IndicatorName         : chr "Patients with GP recorded atrial fibrillation and with a CHADS2 or CHA2DS2-VASc score of 2 or more, who are cur"| __truncated__
#>   ..$ IndicatorOrder        : int 2
#>   ..$ IndicatorShortName    : chr "AF: Treated with anticoagulants (CVDP002AF)"
#>   ..$ NotificationCount     : int 0
#>   ..$ TimePeriodID          : int 1
#>   ..$ TimePeriodName        : chr "To March 2020"
#>   ..$ CategoryAttribute     : chr "Male"
#>   ..$ MetricCategoryID      : int 7
#>   ..$ MetricCategoryName    : chr "40-59"
#>   ..$ MetricCategoryOrder   : int 10
#>   ..$ MetricCategoryTypeName: chr "Age group"
#>   ..$ MetricID              : int 126
#>  $ area_data    : tibble [1 × 26] (S3: tbl_df/tbl/data.frame)
#>   ..$ MetricID                       : int 126
#>   ..$ MetricCategoryTypeName         : chr "Age group"
#>   ..$ MetricCategoryName             : chr "40-59"
#>   ..$ CategoryAttribute              : chr "Male"
#>   ..$ AreaCode                       : chr "U60176"
#>   ..$ AreaID                         : int 1103
#>   ..$ AreaName                       : chr "3 Centres PCN"
#>   ..$ Count                          : num 1214
#>   ..$ DataID                         : int 150416
#>   ..$ Denominator                    : num 20
#>   ..$ Factor                         : num 1
#>   ..$ HighestPriorityNotificationType: logi NA
#>   ..$ LowerConfidenceLimit           : num 85.7
#>   ..$ Max                            : num 100
#>   ..$ Median                         : num 84.6
#>   ..$ Min                            : num 60
#>   ..$ NotificationCount              : int 0
#>   ..$ Numerator                      : num 20
#>   ..$ Q20                            : num 60
#>   ..$ Q40                            : num 78.4
#>   ..$ Q60                            : num 84.6
#>   ..$ Q80                            : num 91.2
#>   ..$ TimePeriodID                   : int 1
#>   ..$ UpperConfidenceLimit           : num 100
#>   ..$ Value                          : num 100
#>   ..$ ValueNote                      : chr ""
#>  $ national_data: tibble [1 × 26] (S3: tbl_df/tbl/data.frame)
#>   ..$ MetricID                       : int 126
#>   ..$ MetricCategoryTypeName         : chr "Age group"
#>   ..$ MetricCategoryName             : chr "40-59"
#>   ..$ CategoryAttribute              : chr "Male"
#>   ..$ AreaCode                       : chr "E92000001"
#>   ..$ AreaID                         : int 1
#>   ..$ AreaName                       : chr "England"
#>   ..$ Count                          : logi NA
#>   ..$ DataID                         : int 126
#>   ..$ Denominator                    : num 14028
#>   ..$ Factor                         : num 1
#>   ..$ HighestPriorityNotificationType: logi NA
#>   ..$ LowerConfidenceLimit           : num 81.9
#>   ..$ Max                            : logi NA
#>   ..$ Median                         : logi NA
#>   ..$ Min                            : logi NA
#>   ..$ NotificationCount              : int 0
#>   ..$ Numerator                      : num 11576
#>   ..$ Q20                            : logi NA
#>   ..$ Q40                            : logi NA
#>   ..$ Q60                            : logi NA
#>   ..$ Q80                            : logi NA
#>   ..$ TimePeriodID                   : int 1
#>   ..$ UpperConfidenceLimit           : num 83.1
#>   ..$ Value                          : num 82.5
#>   ..$ ValueNote                      : chr ""

# See the definition for this metric
returned_list$metrics |> dplyr::glimpse()
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
#> $ TimePeriodID           <int> 1
#> $ TimePeriodName         <chr> "To March 2020"
#> $ CategoryAttribute      <chr> "Male"
#> $ MetricCategoryID       <int> 7
#> $ MetricCategoryName     <chr> "40-59"
#> $ MetricCategoryOrder    <int> 10
#> $ MetricCategoryTypeName <chr> "Age group"
#> $ MetricID               <int> 126

# Compare performance between our area and the national average
dplyr::bind_rows(
  returned_list$area_data,
  returned_list$national_data
) |>
dplyr::glimpse()
#> Rows: 2
#> Columns: 26
#> $ MetricID                        <int> 126, 126
#> $ MetricCategoryTypeName          <chr> "Age group", "Age group"
#> $ MetricCategoryName              <chr> "40-59", "40-59"
#> $ CategoryAttribute               <chr> "Male", "Male"
#> $ AreaCode                        <chr> "U60176", "E92000001"
#> $ AreaID                          <int> 1103, 1
#> $ AreaName                        <chr> "3 Centres PCN", "England"
#> $ Count                           <dbl> 1214, NA
#> $ DataID                          <int> 150416, 126
#> $ Denominator                     <dbl> 20, 14028
#> $ Factor                          <dbl> 1, 1
#> $ HighestPriorityNotificationType <lgl> NA, NA
#> $ LowerConfidenceLimit            <dbl> 85.7, 81.9
#> $ Max                             <dbl> 100, NA
#> $ Median                          <dbl> 84.6, NA
#> $ Min                             <dbl> 60, NA
#> $ NotificationCount               <int> 0, 0
#> $ Numerator                       <dbl> 20, 11576
#> $ Q20                             <dbl> 60, NA
#> $ Q40                             <dbl> 78.4, NA
#> $ Q60                             <dbl> 84.6, NA
#> $ Q80                             <dbl> 91.2, NA
#> $ TimePeriodID                    <int> 1, 1
#> $ UpperConfidenceLimit            <dbl> 100.0, 83.1
#> $ Value                           <dbl> 100.0, 82.5
#> $ ValueNote                       <chr> "", ""
# }
```
