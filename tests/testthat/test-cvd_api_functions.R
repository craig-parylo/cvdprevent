

# time period ------------------------------------------------------------------
test_that('cvd_time_period_list works', {


  # expecting no error
  testthat::expect_no_error(test <- cvd_time_period_list())

  # expecting at least 17 time periods
  testthat::expect_gte(test$TimePeriodID |> max(), 17)
})

test_that('cvd_time_period_system_levels works', {

  # expecting no error
  testthat::expect_no_error(test <- cvd_time_period_system_levels())

})

test_that('cvd_area_system_level works', {

  # expecting no error
  testthat::expect_no_error(test <- cvd_area_system_level())

})

# area -------------------------------------------------------------------------
test_that('cvd_area_system_level_time_periods works', {

  # expecting no error
  testthat::expect_no_error(test <- cvd_area_system_level_time_periods())

})

test_that('cvd_area_list works', {

  # expecting no error
  testthat::expect_no_error(test <- cvd_area_list(system_level_id = 2))

})

test_that('cvd_area_details works', {

  # expecting no error
  testthat::expect_no_error(test <- cvd_area_details())

  # expecting a list of at least one named tibble
  testthat::expect_named(test, c('area_details'))

})

test_that('cvd_area_unassigned works', {

  # expecting no error
  testthat::expect_no_error(test <- cvd_area_unassigned(system_level_id = 1))

})

test_that('cvd_area_search works', {

  # expecting no error
  testthat::expect_no_error(test <- cvd_area_search(partial_area_name = 'foo'))

})

test_that('cvd_area_nested_subsystems works', {

  # expecting no error
  testthat::expect_no_error(test <- cvd_area_nested_subsystems())

  # expecting a list of 4 named tibbles
  testthat::expect_named(test, c('level_1', 'level_2', 'level_3', 'level_4'))

})

test_that('cvd_area_flat_subsystems works', {

  # expecting no error
  testthat::expect_no_error(test <- cvd_area_flat_subsystems())

})

# indicators -------------------------------------------------------------------
test_that('cvd_indicator_list works', {

  # expecting no error
  testthat::expect_no_error(test <- cvd_indicator_list())

})

test_that('cvd_indicator_metric_list works', {

  # expecting no error
  testthat::expect_no_error(test <- cvd_indicator_metric_list())

})

test_that('cvd_indicator works', {

  # expecting no error
  testthat::expect_no_error(test <- cvd_indicator())

})

test_that('cvd_indicator works', {

  # expecting no error
  testthat::expect_no_error(test <- cvd_indicator())

  # expecting a list of 4 named tibbles
  testthat::expect_named(test, c('indicators', 'metric_categories', 'metric_data', 'timeseries_data'))

})

test_that('cvd_indicator_tags works', {

  # expecting no error
  testthat::expect_no_error(test <- cvd_indicator_tags())

})

test_that('cvd_indicator_details works', {

  # expecting no error
  testthat::expect_no_error(test <- cvd_indicator_details())

})

test_that('cvd_indicator_sibling works', {

  # expecting no error
  testthat::expect_no_error(test <- cvd_indicator_sibling())

})

test_that('cvd_indicator_child_data works', {

  # expecting no error
  testthat::expect_no_error(test <- cvd_indicator_child_data())

})

test_that('cvd_indicator_data works', {

  # expecting no error
  testthat::expect_no_error(test <- cvd_indicator_data())

})

test_that('cvd_indicator_metric_data works', {

  # expecting no error
  testthat::expect_no_error(test <- cvd_indicator_metric_data())

})

test_that('cvd_indicator_raw_data works', {

  # expecting no error
  testthat::expect_no_error(test <- cvd_indicator_raw_data())

})

test_that('cvd_indicator_nationalarea_metric_data works', {

  # expecting no error
  testthat::expect_no_error(test <- cvd_indicator_nationalarea_metric_data())

})

test_that('cvd_indicator_priority_groups works', {

  # expecting no error
  testthat::expect_no_error(test <- cvd_indicator_priority_groups())

})

test_that('cvd_indicator_pathway_group works', {

  # expecting no error
  testthat::expect_no_error(test <- cvd_indicator_pathway_group())

})

test_that('cvd_indicator_group works', {

  # expecting no error
  testthat::expect_no_error(test <- cvd_indicator_group())

})

test_that('cvd_indicator_metric_timeseries works', {

  # expecting no error
  testthat::expect_no_error(test <- cvd_indicator_metric_timeseries())

})

test_that('cvd_indicator_person_timeseries works', {

  # expecting no error
  testthat::expect_no_error(test <- cvd_indicator_person_timeseries())

})

test_that('cvd_indicator_metric_systemlevel_comparison works', {

  # expecting no error
  testthat::expect_no_error(test <- cvd_indicator_metric_systemlevel_comparison())

})

test_that('cvd_indicator_metric_area_breakdown works', {

  # expecting no error
  testthat::expect_no_error(test <- cvd_indicator_metric_area_breakdown())

})

# external resource ------------------------------------------------------------

test_that('cvd_external_resource works', {

  # expecting no error
  testthat::expect_no_error(test <- cvd_external_resource())

})

test_that('cvd_data_availability works', {

  # expecting no error
  testthat::expect_no_error(test <- cvd_data_availability())

})

# internal functions -----------------------------------------------------------











