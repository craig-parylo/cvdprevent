skip_if_no_live_api <- function() {
  testthat::skip_on_cran()
  testthat::skip_if_offline()

  # Check both environment variable and R option
  run_live <- identical(Sys.getenv("RUN_LIVE_API_TESTS"), "true") ||
    isTRUE(getOption("RUN_LIVE_API_TESTS", FALSE))

  if (!run_live) {
    testthat::skip(
      "Live API tests disabled (set RUN_LIVE_API_TESTS=true or options(RUN_LIVE_API_TESTS = TRUE))"
    )
  }
}
