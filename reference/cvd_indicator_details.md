# Retrieve details for a specific indicator

Returns metadata and descriptive information for a single CVD indicator,
identified by its IndicatorID, from the CVDPREVENT API. This function
allows you to programmatically access the definitions, titles and
metadata fields associated with specific indicators for use in
reporting, dashboards or documentation.

## Usage

``` r
cvd_indicator_details(indicator_id)
```

## Arguments

- indicator_id:

  Integer (required). The IndicatorID for which to return details. Use
  [`cvd_indicator_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_list.md)
  or
  [`cvd_indicator_metric_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_metric_list.md)
  to find valid IDs.

## Value

A tibble containing metadata and details for the specified indicator
containing the following columns:

- IndicatorCode:

  Character. Unique code for the indicator (e.g., "CVDP002AF").

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

- AgeStandardised:

  Character. Indicates whether the indicator is age-standardised ("Y" or
  "N").

- CategoryName:

  Character. Section heading or thematic grouping for the metadata
  (e.g., "Section 2: Data and Construction").

- MetaData:

  Character. Detailed explanatory text or notes associated with the
  indicator. May include rationale, definitions, sources, or caveats.

- MetaDataCategoryID:

  Integer. Unique identifier for the metadata category.

- MetaDataTitle:

  Character. Title or label describing the metadata content (e.g.,
  "Rationale", "Disclosure control").

If no indicator details are found, returns a tibble describing the
error.

## Details

Use this function to retrieve indicator definitions, full names, and
metadata fields for use in custom reports or to provide documentation /
tooltips in analytical applications. Metadata fields are unnested for
convenience.

## Note

This function may take longer than 5 seconds to complete due to API
response time.

## API Documentation

See the [CVDPREVENT API documentation: Indicator
details](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Findicator%2F%3Cindicator_ID%3E%2Fdetails)

## See also

[`cvd_indicator_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_list.md),
[`cvd_indicator_metric_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_metric_list.md),
[`cvd_indicator()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator.md),
[`cvd_indicator_tags()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_tags.md),
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
# Retrieve details for indicator with ID 7
cvd_indicator_details(indicator_id = 7) |>
  dplyr::select(IndicatorID, MetaDataTitle, MetaData) |>
  dplyr::slice_head(n = 5)
#> # A tibble: 5 × 3
#>   IndicatorID MetaDataTitle         MetaData                                    
#>         <int> <chr>                 <chr>                                       
#> 1           7 Copyright             "Produced by Office for Health Improvement …
#> 2           7 Data source           "Cardiovascular Disease Prevention Audit (C…
#> 3           7 Definition            "Percentage of patients aged 18 and over wi…
#> 4           7 Indicator ID          "CVDP002AF"                                 
#> 5           7 Indicator short title "AF: Treated with anticoagulants (CVDP002AF…

# Find a valid indicator ID, then get its details
indicators <- cvd_indicator_list(time_period_id = 17, system_level_id = 5)
cvd_indicator_details(indicator_id = indicators$IndicatorID[1])
#> # A tibble: 28 × 11
#>    IndicatorCode IndicatorID IndicatorName     IndicatorOrder IndicatorShortName
#>    <chr>               <int> <chr>                      <int> <chr>             
#>  1 CVDP002HYP              2 Patients with GP…              5 Hypertension: Tre…
#>  2 CVDP002HYP              2 Patients with GP…              5 Hypertension: Tre…
#>  3 CVDP002HYP              2 Patients with GP…              5 Hypertension: Tre…
#>  4 CVDP002HYP              2 Patients with GP…              5 Hypertension: Tre…
#>  5 CVDP002HYP              2 Patients with GP…              5 Hypertension: Tre…
#>  6 CVDP002HYP              2 Patients with GP…              5 Hypertension: Tre…
#>  7 CVDP002HYP              2 Patients with GP…              5 Hypertension: Tre…
#>  8 CVDP002HYP              2 Patients with GP…              5 Hypertension: Tre…
#>  9 CVDP002HYP              2 Patients with GP…              5 Hypertension: Tre…
#> 10 CVDP002HYP              2 Patients with GP…              5 Hypertension: Tre…
#> # ℹ 18 more rows
#> # ℹ 6 more variables: NotificationCount <int>, AgeStandardised <chr>,
#> #   CategoryName <chr>, MetaData <chr>, MetaDataCategoryID <int>,
#> #   MetaDataTitle <chr>
# }
```
