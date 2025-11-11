# Retrieve indicators for a specified pathway group

Returns a tibble of indicators associated with a single pathway group,
identified by its ID. Pathway groups are thematic sub-groupings of
priority groups and are visible in the Regional & ICS Insights page.
This function enables users to retrieve all indicators linked to a
specific pathway group (e.g., "Chronic Kidney Disease").

## Usage

``` r
cvd_indicator_pathway_group(pathway_group_id)
```

## Arguments

- pathway_group_id:

  Integer (required). The ID of the pathway group to retrieve. Use
  [`cvd_indicator_priority_groups()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_priority_groups.md)
  to find valid IDs.

## Value

A tibble containing indicators associated with the specified pathway
group. Each row represents a single indicator and includes the following
columns:

- PathwayGroupID:

  Integer. Unique identifier for the pathway group (e.g., 9 for "Chronic
  Kidney Disease").

- PathwayGroupName:

  Character. Name of the pathway group (e.g., "Chronic Kidney Disease").

- AxisCharacter:

  Character. Symbol used to represent the metric axis (e.g., "%").

- FormatDisplayName:

  Character. Display format for the metric (e.g., "Proportion %").

- HighestPriorityNotificationType:

  Character. Notification priority level, if applicable (e.g., "Red").
  Often blank.

- IndicatorCode:

  Character. Unique code for the indicator (e.g., "CVDP006CKD").

- IndicatorFormatID:

  Integer. Internal ID for the indicator's format type.

- IndicatorID:

  Integer. Unique identifier for the indicator.

- IndicatorName:

  Character. Full descriptive name of the indicator.

- MetricID:

  Integer. Unique identifier for the specific metric instance.

- NotificationCount:

  Integer. Count of notifications associated with the indicator.

- PathwayGroupDisplayOrder:

  Integer. Display order of the indicator within the pathway group.

- QuestionGroupName:

  Character. Thematic label for the indicator's clinical focus (e.g.,
  "Diagnosis", "Monitoring", "Management").

If the request fails or the ID is invalid, a tibble with error details
is returned instead.

## Note

This function may take longer than 5 seconds to complete due to API
response time.

## API Documentation

See the [CVDPREVENT API documentation: indicator pathway
group](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Findicator%2FpathwayGroup%2F%3Cpathway_group_id%3E)

## See also

[`cvd_indicator_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_list.md),
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
[`cvd_indicator_group()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_group.md),
[`cvd_indicator_metric_timeseries()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_metric_timeseries.md),
[`cvd_indicator_person_timeseries()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_person_timeseries.md),
[`cvd_indicator_metric_systemlevel_comparison()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_metric_systemlevel_comparison.md),
[`cvd_indicator_metric_area_breakdown()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_metric_area_breakdown.md)

## Examples

``` r
# \donttest{
# Return indicators for the 'Chronic Kidney Disease' Pathway Group (ID 9):
cvd_indicator_pathway_group(pathway_group_id = 9) |>
  dplyr::select(
    PathwayGroupName,
    PathwayGroupID,
    IndicatorCode,
    IndicatorID,
    IndicatorName
  )
#> # A tibble: 6 × 5
#>   PathwayGroupName       PathwayGroupID IndicatorCode IndicatorID IndicatorName 
#>   <chr>                           <int> <chr>               <int> <chr>         
#> 1 Chronic Kidney Disease              9 CVDP002CKD             13 "Patients who…
#> 2 Chronic Kidney Disease              9 CVDP001CKD              8 "Prevalence o…
#> 3 Chronic Kidney Disease              9 CVDP006CKD             29 "Patients wit…
#> 4 Chronic Kidney Disease              9 CVDP005CKD             19 "Patients wit…
#> 5 Chronic Kidney Disease              9 CVDP007CKD             31 "Patients wit…
#> 6 Chronic Kidney Disease              9 CVDP010CHOL            23 "Patients wit…
# }
```
