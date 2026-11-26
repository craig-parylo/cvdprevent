# -----------------------------------------------------------------------------
# Create vignette example data
# -----------------------------------------------------------------------------
#
# Purpose
# -------
# The package vignette is intended to demonstrate how to use the CVDPREVENT API
# wrapper, rather than to test the live API itself.
#
# Historically, the vignettes called the API directly during rendering.
# However, changes in the upstream API structure or availability can cause
# vignette builds to fail, including during CRAN checks.
#
# This script retrieves a representative set of API responses and stores them
# as RDS files within:
#
# inst/extdata/
#
# Vignettes can then use these stored responses to provide stable, reproducible
# examples that are independent of the current state of the API.
#
# Regenerating the data
# ---------------------
# Re-run this script when:
#
# * releasing a new package version
# * adding or modifying vignette examples
# * intentionally updating examples to reflect changes in the API
# * investigating API schema changes
#
# Before committing updated files, verify that:
# * all vignette chunks still execute successfully
# * returned objects have the expected structure
# * example outputs remain representative of typical API responses
#
# Note
# ----
# Integration tests should continue to exercise the live API. These stored
# datasets exist solely to support reproducible package documentation and
# should not be regarded as authoritative reference data.
#
# -----------------------------------------------------------------------------

# define a function to save the example data
save_vignette_data <- function(object) {
  # get the name of the expression used in the call
  object_name <- deparse(substitute(object))

  # save the object for later use
  saveRDS(object = object, file = glue::glue("inst/extdata/{object_name}.rds"))
}

# Time periods ----------------------------------------------------------------
## Listing available time periods ----
cvd_time_period_list_noarg <- cvdprevent::cvd_time_period_list()
save_vignette_data(cvd_time_period_list_noarg)

cvd_time_period_list_2 <- cvdprevent::cvd_time_period_list(
  indicator_type_id = 2
)
save_vignette_data(cvd_time_period_list_2)

## Listing system levels by time period ----
cvd_time_period_system_levels <- cvdprevent::cvd_time_period_system_levels()
save_vignette_data(cvd_time_period_system_levels)

# Areas -----------------------------------------------------------------------
## List available system levels for a given time period ----
cvd_area_system_level_17 <- cvdprevent::cvd_area_system_level(
  time_period_id = 17
)
save_vignette_data(cvd_area_system_level_17)

## List all available reporting periods for each system level ----
cvd_area_system_level_time_periods <- cvdprevent::cvd_area_system_level_time_periods()
save_vignette_data(cvd_area_system_level_time_periods)

## List areas for a time period and system level or parent area ----
cvd_area_list_17_4 <- cvdprevent::cvd_area_list(
  time_period_id = 17,
  system_level_id = 4
)
save_vignette_data(cvd_area_list_17_4)

## View details for a specific area ----
cvd_area_details_17_7922 <- cvdprevent::cvd_area_details(
  time_period_id = 17,
  area_id = 7922
)
save_vignette_data(cvd_area_details_17_7922)

## List areas without parent details ----
cvd_area_unassigned_17_5 <- cvdprevent::cvd_area_unassigned(
  time_period_id = 17,
  system_level_id = 5
)
save_vignette_data(cvd_area_unassigned_17_5)

cvd_area_unassigned_17_1 <- cvdprevent::cvd_area_unassigned(
  time_period_id = 17,
  system_level_id = 1
)
save_vignette_data(cvd_area_unassigned_17_1)

## Search for area by keyword ----
cvd_area_search_foo_17 <- cvdprevent::cvd_area_search(
  partial_area_name = "foo",
  time_period_id = 17
)
save_vignette_data(cvd_area_search_foo_17)

## Area details ----

### Nested -----
cvd_area_nested_subsystems_8030 <- cvdprevent::cvd_area_nested_subsystems(
  area_id = 8030
)
save_vignette_data(cvd_area_nested_subsystems_8030)

### Flat ----
cvd_area_flat_subsystems_5 <- cvdprevent::cvd_area_flat_subsystems(area_id = 5)
save_vignette_data(cvd_area_flat_subsystems_5)

# Indicators ------------------------------------------------------------------

## Listing indicators ----
cvd_indicator_list_17_5 <- cvdprevent::cvd_indicator_list(
  time_period_id = 17,
  system_level_id = 5
)
save_vignette_data(cvd_indicator_list_17_5)

## Listing metrics for each indicator ----
cvd_indicator_metric_list_17_1 <- cvdprevent::cvd_indicator_metric_list(
  time_period_id = 17,
  system_level_id = 1
)
save_vignette_data(cvd_indicator_metric_list_17_1)

## List all indicator data for a given area ----
cvd_indicator_17_1103 <- cvdprevent::cvd_indicator(
  time_period_id = 17,
  area_id = 1103
)
save_vignette_data(cvd_indicator_17_1103)

### Tags ----
cvd_indicator_17_3_34 <- cvdprevent::cvd_indicator(
  time_period_id = 17,
  area_id = 3,
  tag_id = c(3, 4)
)
save_vignette_data(cvd_indicator_17_3_34)

## List indicator tags ----
cvd_indicator_tags <- cvdprevent::cvd_indicator_tags()
save_vignette_data(cvd_indicator_tags)

## Get meta-data for an indicator ----
cvd_indicator_details_7 <- cvdprevent::cvd_indicator_details(indicator_id = 7)
save_vignette_data(cvd_indicator_details_7)

## Sibling area data ----
cvd_indicator_sibling_17_1103_126 <- cvdprevent::cvd_indicator_sibling(
  time_period_id = 17,
  area_id = 1103,
  metric_id = 126
)
save_vignette_data(cvd_indicator_sibling_17_1103_126)

## Child area data ----
cvd_indicator_child_data_17_74_126 <- cvdprevent::cvd_indicator_child_data(
  time_period_id = 17,
  area_id = 74,
  metric_id = 126
)
save_vignette_data(cvd_indicator_child_data_17_74_126)

## All metric data for a given time, area and indicator ----
cvd_indicator_data_17_7_701 <- cvdprevent::cvd_indicator_data(
  time_period_id = 17,
  indicator_id = 7,
  area_id = 701
)
save_vignette_data(cvd_indicator_data_17_7_701)

## All indicator metric data for system level and time period ----
cvd_indicator_raw_data_17_7_5 <- cvdprevent::cvd_indicator_raw_data(
  time_period_id = 17,
  indicator_id = 7,
  system_level_id = 5
)
save_vignette_data(cvd_indicator_raw_data_17_7_5)

## Benchmark metric data with national results ----
cvd_indicator_nationalarea_metric_data_17_553_150 <- cvdprevent::cvd_indicator_nationalarea_metric_data(
  time_period_id = 17,
  area_id = 553,
  metric_id = 150
)
# NOTE: having to 'fake up' target data as this seems to have disappeared
# from the API, but I'm adding in just in case this feature returns
fake_target <- tibble::tibble(
  TargetLabel = "Upper threshold for QOF",
  TargetPatients = 45,
  TargetValue = 95
)
cvd_indicator_nationalarea_metric_data_17_553_150 <- c(
  cvd_indicator_nationalarea_metric_data_17_553_150,
  list("target" = fake_target)
)
save_vignette_data(cvd_indicator_nationalarea_metric_data_17_553_150)

## List priority group indicators ----
cvd_indicator_priority_groups <- cvdprevent::cvd_indicator_priority_groups()
save_vignette_data(cvd_indicator_priority_groups)

## List pathway group indicators ----
cvd_indicator_pathway_group_51 <- cvdprevent::cvd_indicator_pathway_group(
  pathway_group_id = 51
)
save_vignette_data(cvd_indicator_pathway_group_51)

## List indicator group indicators ----
cvd_indicator_group_51 <- cvdprevent::cvd_indicator_group(
  indicator_group_id = 51
)
save_vignette_data(cvd_indicator_group_51)

## Return time series data for a metric and area ----
cvd_indicator_metric_timeseries_130_705 <- cvdprevent::cvd_indicator_metric_timeseries(
  metric_id = 130,
  area_id = 705
)
save_vignette_data(cvd_indicator_metric_timeseries_130_705)

## Return time series data for an indicator and area - inequalities ----
cvd_indicator_person_timeseries_7_705 <- cvdprevent::cvd_indicator_person_timeseries(
  indicator_id = 7,
  area_id = 705
)
save_vignette_data(cvd_indicator_person_timeseries_7_705)

## Return system level comparisons for a metric ----
cvd_indicator_metric_systemlevel_comparison_17_1270_705 <- cvdprevent::cvd_indicator_metric_systemlevel_comparison(
  time_period_id = 17,
  metric_id = 1270,
  area_id = 705
)
save_vignette_data(cvd_indicator_metric_systemlevel_comparison_17_1270_705)

## Metric area breakdown ----
cvd_indicator_metric_area_breakdown_17_128_705 <- cvdprevent::cvd_indicator_metric_area_breakdown(
  time_period_id = 17,
  metric_id = 128,
  area_id = 705
)
save_vignette_data(cvd_indicator_metric_area_breakdown_17_128_705)

# Misc ------------------------------------------------------------------------

## List external resources ----
cvd_external_resource <- cvdprevent::cvd_external_resource()
save_vignette_data(cvd_external_resource)

## Show data availability ----
cvd_data_availability_6_5 <- cvdprevent::cvd_data_availability(
  time_period_id = 6,
  system_level_id = 5
)
save_vignette_data(cvd_data_availability_6_5)
