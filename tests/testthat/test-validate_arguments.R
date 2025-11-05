# # tests/testthat/test-validate-input.R

# test_that("validate_input_id passes valid ids", {
#   expect_true(validate_input_id(
#     5,
#     param_name = "indicator_id",
#     suppress_cli = TRUE
#   ))
#   expect_true(validate_input_id(
#     5L,
#     param_name = "indicator_id",
#     suppress_cli = TRUE
#   ))
# })

# test_that("validate_input_id fails on missing or wrong type", {
#   res <- validate_input_id(NA, param_name = "indicator_id", suppress_cli = TRUE)
#   expect_s3_class(res, "tbl_df")
#   expect_match(res$error, "required")

#   res <- validate_input_id(
#     3.5,
#     param_name = "indicator_id",
#     suppress_cli = TRUE
#   )
#   expect_match(res$error, "whole number")

#   res <- validate_input_id(
#     13,
#     param_name = "indicator_id",
#     valid_ids = c(1, 3, 4),
#     suppress_cli = TRUE
#   )
#   expect_match(res$error, "must be one of")
# })

# test_that("validate_input_string passes valid strings", {
#   expect_true(validate_input_string(
#     "hello",
#     param_name = "area_name",
#     suppress_cli = TRUE
#   ))
#   expect_true(validate_input_string(
#     "  hello  ",
#     param_name = "area_name",
#     suppress_cli = TRUE
#   ))
# })

# test_that("validate_input_string fails on NA, empty, wrong type, or invalid value", {
#   res <- validate_input_string(
#     NA,
#     param_name = "area_name",
#     suppress_cli = TRUE
#   )
#   expect_match(res$error, "required")

#   res <- validate_input_string(
#     "",
#     param_name = "area_name",
#     suppress_cli = TRUE
#   )
#   expect_match(res$error, "must not be empty")

#   res <- validate_input_string(
#     123,
#     param_name = "area_name",
#     suppress_cli = TRUE
#   )
#   expect_match(res$error, "character string")

#   res <- validate_input_string(
#     "foo",
#     param_name = "area_name",
#     valid_values = c("bar", "baz"),
#     suppress_cli = TRUE
#   )
#   expect_match(res$error, "must be one of")
# })

# test_that("validate_input_id_vector passes valid vectors", {
#   expect_true(validate_input_id_vector(
#     c(1, 2, 3),
#     param_name = "ids",
#     suppress_cli = TRUE
#   ))
#   expect_true(validate_input_id_vector(
#     c("1", "2"),
#     param_name = "ids",
#     suppress_cli = TRUE
#   ))
#   expect_true(validate_input_id_vector(
#     integer(0),
#     required = FALSE,
#     suppress_cli = TRUE
#   ))
# })

# test_that("validate_input_id_vector fails on invalid elements", {
#   res <- validate_input_id_vector(NULL, param_name = "ids", suppress_cli = TRUE)
#   expect_match(res$error, "required")

#   res <- validate_input_id_vector(
#     c(1.2, 2),
#     param_name = "ids",
#     suppress_cli = TRUE
#   )
#   expect_match(res$error, "fractional")

#   res <- validate_input_id_vector(
#     c("a", "2"),
#     param_name = "ids",
#     suppress_cli = TRUE
#   )
#   expect_match(res$error, "non-integer-like")

#   res <- validate_input_id_vector(
#     c(10, 11),
#     param_name = "ids",
#     valid_ids = c(1, 2, 3),
#     suppress_cli = TRUE
#   )
#   expect_match(res$error, "invalid ids")
# })

# tests/testthat/test-validators.R

testthat::test_that("ensure_error_tibble returns tibble with expected columns", {
  res <- ensure_error_tibble(
    "ctx",
    "msg",
    status = 400,
    url = "u",
    params = "p",
    resp = "r"
  )

  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_true(all(
    c("context", "error", "status", "url", "params", "resp", "timestamp") %in%
      names(res)
  ))
  testthat::expect_equal(res$context, "ctx")
  testthat::expect_equal(res$error, "msg")
  testthat::expect_equal(res$status, 400L)
})

testthat::test_that("ensure_error_tibble uses cvd_error_tibble if available", {
  fake <- tibble::tibble(
    context = "x",
    error = "y",
    status = 1L,
    url = "u",
    params = "p",
    resp = "r",
    timestamp = Sys.time()
  )

  mockery::stub(ensure_error_tibble, "cvd_error_tibble", function(...) fake)

  res <- ensure_error_tibble("ctx", "msg")
  testthat::expect_equal(res, fake)
})

# ------------------------------------------------------------------------------

testthat::test_that("validate_input_id returns error if required and missing", {
  res <- validate_input_id(NULL, required = TRUE, suppress_cli = TRUE)
  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_match(res$error, "required")
})

testthat::test_that("validate_input_id returns TRUE if not required and missing", {
  res <- validate_input_id(NULL, required = FALSE, suppress_cli = TRUE)
  testthat::expect_true(res)
})

testthat::test_that("validate_input_id enforces scalar length", {
  res <- validate_input_id(c(1, 2), suppress_cli = TRUE)
  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_match(res$error, "single value")
})

testthat::test_that("validate_input_id enforces numeric whole number", {
  res1 <- validate_input_id("a", suppress_cli = TRUE)
  res2 <- validate_input_id(1.5, suppress_cli = TRUE)

  testthat::expect_match(res1$error, "whole number")
  testthat::expect_match(res2$error, "fractional")
})

testthat::test_that("validate_input_id checks domain", {
  res <- validate_input_id(5, valid_ids = c(1, 2, 3), suppress_cli = TRUE)
  testthat::expect_match(res$error, "must be one of")

  res_ok <- validate_input_id(2, valid_ids = c(1, 2, 3), suppress_cli = TRUE)
  testthat::expect_true(res_ok)
})

testthat::test_that("validate_input_id returns TRUE for valid integer", {
  res <- validate_input_id(10, suppress_cli = TRUE)
  testthat::expect_true(res)
})
