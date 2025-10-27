# time period ------------------------------------------------------------------
test_that('cvd_time_period_list works', {
  # expecting no error
  testthat::expect_no_error(test <- cvd_time_period_list())

  # expecting at least 17 time periods
  testthat::expect_gte(test$TimePeriodID |> max(), 17)

  # expecting no error - with indictor type id
  testthat::expect_no_error(
    test <- cvd_time_period_list(
      indicator_type_id = get_random_valid_indicator_type_id(n = 1)
    )
  )
})

test_that('cvd_time_period_system_levels works', {
  # expecting no error
  testthat::expect_no_error(test <- cvd_time_period_system_levels())
})

test_that('cvd_area_system_level works', {
  # expecting no error
  test_val <- get_random_valid_time_period_id(n = 1)
  purrr::walk(
    .x = test_val,
    .f = \(.x) {
      testthat::expect_no_error(
        test <- cvd_area_system_level(time_period_id = .x)
      )
    }
  )
})

# area -------------------------------------------------------------------------
test_that('cvd_area_system_level_time_periods works', {
  # expecting no error
  testthat::expect_no_error(test <- cvd_area_system_level_time_periods())
})

test_that('cvd_area_list works', {
  # expecting no error
  test_val <- get_random_valid_time_period_id(n = 1)
  purrr::walk(
    .x = test_val,
    .f = \(.x) {
      testthat::expect_no_error(
        test <- cvd_area_list(
          time_period_id = .x,
          system_level_id = get_random_system_level_for_time_period_id(
            time_period_id = .x
          )
        )
      )
    }
  )
})

test_that('cvd_area_details works', {
  test_val <- get_random_valid_time_period_id(n = 1)
  purrr::walk(
    .x = test_val,
    .f = \(.x) {
      # expecting no error from the function
      testthat::expect_no_error(
        test <- cvd_area_details(
          time_period_id = .x,
          area_id = get_random_valid_area_id_for_time_period_id(
            n = 1,
            time_period_id = .x
          )
        )
      )

      # expecting a list of at least one named tibble called "area_details"
      testthat::expect_true(
        "area_details" %in%
          names(test) &&
          tibble::is_tibble(test[["area_details"]])
      )
    }
  )
})

test_that('cvd_area_unassigned works', {
  # expecting no error
  test_val <- get_random_valid_time_period_id(n = 1)
  purrr::walk(
    .x = test_val,
    .f = \(.x) {
      testthat::expect_no_error({
        # without a system_level_id
        test <- cvd_area_unassigned(time_period = .x)

        # supplying a system_level_id
        test <- cvd_area_unassigned(time_period = .x, system_level_id = 1)
      })
    }
  )
})

test_that('cvd_area_search works', {
  # expecting no error
  test_val <- get_random_valid_time_period_id(n = 1)
  purrr::walk(
    .x = test_val,
    .f = \(.x) {
      testthat::expect_no_error(
        test <- cvd_area_search(partial_area_name = "foo", time_period_id = .x)
      )
    }
  )
})

test_that('cvd_area_nested_subsystems works', {
  # run the function - expecting no error
  testthat::expect_no_error(test <- cvd_area_nested_subsystems(area_id = 5))

  # check the result is a list
  testthat::expect_type(test, "list")

  # check the result has between 1 and 4 elements
  testthat::expect_gte(length(test), 1)
  testthat::expect_lte(length(test), 4)

  # check names are among expected levels
  expected_names <- c("level_1", "level_2", "level_3", "level_4")
  testthat::expect_true(all(names(test) %in% expected_names))

  # check each element is a tibble
  purrr::walk(test, ~ testthat::expect_true(tibble::is_tibble(.x)))
})

test_that('cvd_area_flat_subsystems works', {
  # expecting no error
  testthat::expect_no_error(test <- cvd_area_flat_subsystems(area_id = 5))
})

# indicators -------------------------------------------------------------------
test_that('cvd_indicator_list works', {
  # expecting no error
  test_val <- get_random_valid_time_period_id(n = 1)
  purrr::walk(
    .x = test_val,
    .f = \(.x) {
      testthat::expect_no_error(
        test <- cvd_indicator_list(
          time_period_id = .x,
          system_level_id = get_random_system_level_for_time_period_id(
            time_period_id = .x
          )
        )
      )
    }
  )
})

test_that('cvd_indicator_metric_list works', {
  # expecting no error
  test_val <- get_random_valid_time_period_id(n = 1)
  purrr::walk(
    .x = test_val,
    .f = \(.x) {
      testthat::expect_no_error(
        test <- cvd_indicator_metric_list(
          time_period_id = .x,
          system_level_id = get_random_system_level_for_time_period_id(
            time_period_id = .x
          )
        )
      )
    }
  )
})

test_that('cvd_indicator works', {
  # expecting no error
  test_val <- get_random_valid_time_period_id(n = 1)
  purrr::walk(
    .x = test_val,
    .f = \(.x) {
      # passing in a sample of valid tag ids
      testthat::expect_no_error({
        test <- cvd_indicator(
          time_period_id = .x,
          area_id = get_random_valid_area_id_for_time_period_id(
            n = 1,
            time_period_id = .x
          ),
          tag_id = get_random_valid_tag_id(n = 4)
        )
      })

      # without passing in a vector of tag ids
      testthat::expect_no_error({
        test <- cvd_indicator(
          time_period_id = .x,
          area_id = get_random_valid_area_id_for_time_period_id(
            n = 1,
            time_period_id = .x
          )
        )
      })

      # check the result is a list
      testthat::expect_type(test, "list")

      # check it has 4 elements
      testthat::expect_length(test, n = 4)

      # check names are among the expected levels
      expected_names <- c(
        "indicators",
        "metric_categories",
        "metric_data",
        "timeseries_data"
      )
      testthat::expect_true(all(names(test) %in% expected_names))
    }
  )
})

test_that('cvd_indicator_tags works', {
  # expecting no error
  testthat::expect_no_error(test <- cvd_indicator_tags())
})

test_that('cvd_indicator_details works', {
  # expecting no error
  testthat::expect_no_error(test <- cvd_indicator_details(indicator_id = 1))
})

test_that('cvd_indicator_sibling works', {
  # expecting no error
  testthat::expect_no_error({
    test_time_period_id <- get_random_valid_time_period_id(n = 1)
    test <- cvd_indicator_sibling(
      time_period_id = test_time_period_id,
      area_id = get_random_valid_area_id_for_time_period_id(
        n = 1,
        time_period_id = test_time_period_id
      ),
      metric_id = 1
    )
  })
})

test_that('cvd_indicator_child_data works', {
  # expecting no error
  testthat::expect_no_error({
    test_time_period_id <- get_random_valid_time_period_id(n = 1)
    test <- cvd_indicator_child_data(
      time_period_id = test_time_period_id,
      area_id = get_random_valid_area_id_for_time_period_id(
        n = 1,
        time_period_id = test_time_period_id
      ),
      metric_id = 1
    )
  })
})

test_that('cvd_indicator_data works', {
  # expecting no error
  testthat::expect_no_error({
    test_time_period_id <- get_random_valid_time_period_id(n = 1)
    test <- cvd_indicator_data(
      indicator_id = 1,
      time_period_id = test_time_period_id,
      area_id = get_random_valid_area_id_for_time_period_id(
        n = 1,
        time_period_id = test_time_period_id
      )
    )
  })
})

test_that('cvd_indicator_metric_data works', {
  # expecting no error
  testthat::expect_no_error({
    test_time_period_id <- get_random_valid_time_period_id(n = 1)
    test <- cvd_indicator_metric_data(
      metric_id = 7,
      time_period_id = test_time_period_id,
      area_id = get_random_valid_area_id_for_time_period_id(
        n = 1,
        time_period_id = test_time_period_id
      )
    )
  })
})

test_that('cvd_indicator_raw_data works', {
  # expecting no error
  testthat::expect_no_error({
    test_time_period_id <- get_random_valid_time_period_id(n = 1)
    test <- cvd_indicator_raw_data(
      indicator_id = 7,
      time_period_id = test_time_period_id,
      system_level_id = get_random_system_level_for_time_period_id(
        n = 1,
        time_period_id = test_time_period_id
      )
    )
  })
})

test_that('cvd_indicator_nationalarea_metric_data works', {
  # expecting no error
  testthat::expect_no_error({
    test_time_period_id <- get_random_valid_time_period_id(n = 1)
    test <- cvd_indicator_nationalarea_metric_data(
      metric_id = 1,
      time_period_id = test_time_period_id,
      area_id = get_random_valid_area_id_for_time_period_id(
        n = 1,
        time_period_id = test_time_period_id
      )
    )
  })

  # expecting error caused by missing parameters
  testthat::expect_error({
    test <- cvd_indicator_nationalarea_metric_data(
      metric_id = NA,
      time_period_id = get_random_valid_time_period_id(n = 1),
      area_id = 739
    )
    test <- cvd_indicator_nationalarea_metric_data(
      metric_id = 1,
      time_period_id = get_random_valid_time_period_id(n = 1),
      area_id = 739
    )
    test <- cvd_indicator_nationalarea_metric_data(
      metric_id = 1,
      time_period_id = get_random_valid_time_period_id(n = 1),
      area_id = NA
    )
  })
})

test_that('cvd_indicator_priority_groups works', {
  # expecting no error
  testthat::expect_no_error(test <- cvd_indicator_priority_groups())
})

test_that('cvd_indicator_pathway_group works', {
  # expecting no error
  testthat::expect_no_error(
    test <- cvd_indicator_pathway_group(pathway_group_id = 10)
  )
})

test_that('cvd_indicator_group works', {
  # expecting no error
  testthat::expect_no_error(
    test <- cvd_indicator_group(indicator_group_id = 15)
  )
})

test_that('cvd_indicator_metric_timeseries works', {
  # expecting no error
  testthat::expect_no_error(
    test <- cvd_indicator_metric_timeseries(metric_id = 1, area_id = 50)
  )
})

test_that('cvd_indicator_person_timeseries works', {
  # expecting no error
  testthat::expect_no_error(
    test <- cvd_indicator_person_timeseries(indicator_id = 1, area_id = 1)
  )
})

test_that('cvd_indicator_metric_systemlevel_comparison works', {
  # expecting no error
  testthat::expect_no_error({
    test_time_period_id <- get_random_valid_time_period_id(n = 1)
    test <- cvd_indicator_metric_systemlevel_comparison(
      metric_id = 1,
      time_period_id = test_time_period_id,
      area_id = get_random_valid_area_id_for_time_period_id(
        n = 1,
        time_period_id = test_time_period_id
      )
    )
  })
})

test_that('cvd_indicator_metric_area_breakdown works', {
  # expecting no error
  testthat::expect_no_error({
    test_time_period_id <- get_random_valid_time_period_id(n = 1)
    test <- cvd_indicator_metric_area_breakdown(
      metric_id = 1,
      time_period_id = test_time_period_id,
      area_id = get_random_valid_area_id_for_time_period_id(
        n = 1,
        time_period_id = test_time_period_id
      )
    )
  })
})

# external resource ------------------------------------------------------------

test_that('cvd_external_resource works', {
  # expecting no error
  testthat::expect_no_error(test <- cvd_external_resource())
})

test_that('cvd_data_availability works', {
  # expecting no error
  testthat::expect_no_error({
    test_time_period_id <- get_random_valid_time_period_id(n = 1)
    test <- cvd_data_availability(
      time_period_id = get_random_valid_time_period_id(n = 1),
      system_level_id = get_random_system_level_for_time_period_id(
        n = 1,
        time_period_id = test_time_period_id
      )
    )
  })
})
