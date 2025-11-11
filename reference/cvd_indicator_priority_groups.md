# List all indicator priority groups

Retrieves a tibble of indicator priority groups from the CVDPREVENT API.
Priority groups reflect high-level clinical, operational or policy
themes (such as "Inequalities" or "NHS Long Term Plan") and provide a
way to cluster or filter multiple indicators for reporting and
analytics.

## Usage

``` r
cvd_indicator_priority_groups()
```

## Value

A tibble with one row per indicator / priority group containing the
following columns:

- PriorityGroup:

  Character. High-level grouping label for the indicator (e.g., "CKD",
  "Prevalence", "ABC").

- AxisCharacter:

  Character. Symbol used to represent the metric axis (e.g., "%").

- FormatDisplayName:

  Character. Display format for the metric (e.g., "Proportion %", "Rate
  per 10,000 patients").

- HighestPriorityNotificationType:

  Character. Notification priority level, if applicable (e.g., "Blue").
  Often blank.

- IndicatorCode:

  Character. Unique code for the indicator (e.g., "CVDP002AF").

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

- PathwayGroupID:

  Integer. Unique identifier for the clinical pathway group.

- PathwayGroupName:

  Character. Name of the clinical pathway group (e.g., "Chronic Kidney
  Disease", "Hypertension").

- PriorityGroupDisplayOrder:

  Integer. Display order for the priority group within its pathway.

- PriorityGroupID:

  Integer. Unique identifier for the priority group.

- QuestionGroupName:

  Character. Thematic label for the indicator's clinical focus (e.g.,
  "Diagnosis", "Management", "Monitoring").

If no priority groups are found, returns a tibble describing the error.

## Details

Use this function to provide grouping / filtering options for dashboards
or reports, or to explore which indicator themes are tracked in
CVDPREVENT. Typically, you will select the priority group's name and ID
for grouping or filtering tasks.

## Note

This function may take longer than 5 seconds to complete due to API
response time.

## API Documentation

See the [CVDPREVENT API documentation: Indicator priority
groups](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Findicator%2FpriorityGroup)

## See also

[`cvd_indicator_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_list.md),
[`cvd_indicator_group()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_group.md),
[`cvd_area_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_list.md),
[`cvd_time_period_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_time_period_list.md)

## Examples

``` r
# \donttest{
# List all available priority group display names and their IDs
cvd_indicator_priority_groups() |>
  dplyr::select(PriorityGroupID, PriorityGroup)
#>    PriorityGroupID   PriorityGroup
#> 1                1             ABC
#> 2                1             ABC
#> 3                1             ABC
#> 4                1             ABC
#> 5                1             ABC
#> 6                1             ABC
#> 7                1             ABC
#> 8                1             ABC
#> 9                4             CKD
#> 10               4             CKD
#> 11               4             CKD
#> 12               4             CKD
#> 13               2      Prevalence
#> 14               2      Prevalence
#> 15               2      Prevalence
#> 16               2      Prevalence
#> 17               2      Prevalence
#> 18               2      Prevalence
#> 19               3 Smoking and BMI
#> 20               3 Smoking and BMI
#> 21               3 Smoking and BMI

# Preview group names for a sidebar filter in a dashboard
groups <- cvd_indicator_priority_groups()
unique(groups$PriorityGroup)
#> [1] "ABC"             "CKD"             "Prevalence"      "Smoking and BMI"
# }
```
