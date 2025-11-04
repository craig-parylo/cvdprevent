testthat::test_that("cvd_time_period_list works against live API", {
  # Skip in environments where live calls are not appropriate
  skip_if_no_live_api()

  # Genuine call
  res <- cvd_time_period_list()

  # Basic structural checks
  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_true("timePeriodID" %in% names(res) || nrow(res) == 0)
})
