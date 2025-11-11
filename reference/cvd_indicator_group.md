# Retrieve indicators for a specified indicator group

Returns a tibble of indicators belonging to a single indicator group,
identified by its group ID. Indicator groups are thematic collections of
indicators used throughout CVDPREVENT reporting, such as "Monitoring",
"Diagnosis", or "Management". This function enables users to explore the
structure and contents of these groups, including their type and display
order.

## Usage

``` r
cvd_indicator_group(indicator_group_id)
```

## Arguments

- indicator_group_id:

  Integer (required). The ID of the indicator group to retrieve.

## Value

A tibble where each row represents an indicator within the specified
indicator group. Columns include:

- IndicatorGroupID:

  Integer. Unique identifier for the indicator group (e.g., 1 for
  "ABC").

- IndicatorGroupName:

  Character. Name of the indicator group (e.g., "Prevalence", "Smoking
  and BMI").

- IndicatorGroupTypeID:

  Integer. Identifier for the type of indicator group (e.g., 1 =
  Priority Group).

- IndicatorGroupTypeName:

  Character. Descriptive name of the group type (e.g., "Priority
  Group").

- DisplayOrder:

  Integer. Display order of the indicator within the group.

- HighestPriorityNotificationType:

  Character. Notification priority level, if applicable (e.g., "Red",
  "Blue"). Often blank.

- IndicatorCode:

  Character. Unique code for the indicator (e.g., "CVDP004HYP").

- IndicatorID:

  Integer. Unique identifier for the indicator.

- IndicatorName:

  Character. Full descriptive name of the indicator.

- MetricID:

  Integer. Unique identifier for the associated metric.

- NotificationCount:

  Integer. Count of notifications associated with the indicator.

If the request fails or the ID is invalid, a tibble with error details
is returned instead.

## Details

Indicator groups are defined in the CVDPREVENT IndicatorGroup table and
include metadata such as:

- `IndicatorGroupName`: the name of the group (e.g., "Monitoring")

- `IndicatorGroupTypeID`: the type of group (e.g., Priority Group, Key
  Question Group)

- `IndicatorGroupTypeName`: the readable label for the group type

Each group contains an array of indicators, which are returned with
their associated metadata. This function is useful for:

- Exploring which indicators belong to a specific clinical theme

- Building dashboards or reports based on grouped indicators

- Understanding how indicators are organised within the CVDPREVENT
  framework

To find valid `indicator_group_id` values, use
[`cvd_indicator_priority_groups()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_priority_groups.md).

## Note

This function may take longer than 5 seconds to complete due to API
response time.

## API Documentation

See the [CVDPREVENT API documentation: indicator
group](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Findicator%2FindicatorGroup%2F%3Cindicator_group_ID%3E)

## See also

[`cvd_indicator_priority_groups()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_priority_groups.md)
to browse available indicator groups,
[`cvd_indicator_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_list.md)
to view all indicators,
[`cvd_indicator_data()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_data.md)
to retrieve metric values,
[`cvd_indicator_details()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_details.md)
for indicator metadata,
[`cvd_indicator_pathway_group()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_pathway_group.md)
for pathway-based groupings

## Examples

``` r
# \donttest{
#  list the indicators under Indicator Group ID 13 (Monitoring) which lists
# 'Key Question' Indicator Group indicators:
cvd_indicator_group(indicator_group_id = 13) |>
  dplyr::select(IndicatorGroupID, IndicatorGroupName, IndicatorGroupTypeName,
  IndicatorID, IndicatorName)
#> # A tibble: 2 × 5
#>   IndicatorGroupID IndicatorGroupName IndicatorGroupTypeName IndicatorID
#>              <int> <chr>              <chr>                        <int>
#> 1               13 Monitoring         Key Question                     4
#> 2               13 Monitoring         Key Question                    29
#> # ℹ 1 more variable: IndicatorName <chr>
# }
```
