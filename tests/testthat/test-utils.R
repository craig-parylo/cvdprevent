# # validate `time_period_id` ---------------------------------------------------
# testthat::test_that("Check that `validate_input_id()` works as expected for time periods", {
#   # test where we expect no errors - parameter required
#   valid_time_period_ids <- m_get_valid_time_period_ids() # get a list of valid time periods
#   test_vals <- sample(x = valid_time_period_ids, size = 4) # get 4 random valid time periods
#   purrr::walk(
#     .x = test_vals,
#     .f = \(.x) {
#       testthat::expect_no_error(
#         validate_input_id(
#           id = .x,
#           param_name = "time_period_id",
#           required = TRUE,
#           valid_ids = valid_time_period_ids
#         )
#       )
#     }
#   )

#   # test where we expect no errors - parameter not required
#   test_vals <- c(NA, NULL, 1, 10)
#   purrr::walk(
#     .x = test_vals,
#     .f = \(.x) {
#       testthat::expect_no_error(
#         validate_input_id(
#           id = .x,
#           param_name = "time_period_id",
#           required = FALSE,
#           valid_ids = valid_time_period_ids
#         )
#       )
#     }
#   )

#   # test where we expect errors - parameter required
#   test_vals <- c(NA, NULL, "five", "5")
#   purrr::walk(
#     .x = test_vals,
#     .f = \(.x) {
#       testthat::expect_error(
#         validate_input_id(
#           id = .x,
#           param_name = "time_period_id",
#           required = TRUE,
#           valid_ids = valid_time_period_ids
#         )
#       )
#     }
#   )
# })
