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
if (FALSE) { # \dontrun{
# List the first five indicator tags
cvd_indicator_tags() |>
  dplyr::arrange(IndicatorTagID) |>
  dplyr::slice_head(n = 5)

# Use a tag ID to filter indicators in another query
tags <- cvd_indicator_tags()
tag_id <- tags$IndicatorTagID[1]
cvd_indicator(time_period_id = 17, area_id = 3, tag_id = tag_id)
} # }
```
