RUN_LIVE_API_TESTS <- FALSE # default

enable_live_tests <- function() {
  assign("RUN_LIVE_API_TESTS", TRUE, envir = .GlobalEnv)
}
