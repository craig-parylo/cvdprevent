# tests/testthat/test-cvd_error_tibble.R

test_that("cvd_error_tibble returns a tibble with correct columns", {
  err <- cvd_error_tibble(
    context = "unit_test",
    error = "Something went wrong",
    status = 500,
    url = "https://example.com",
    params = "id=1",
    resp = "Internal Server Error"
  )

  expect_s3_class(err, "tbl_df")
  expect_named(
    err,
    c("context", "error", "status", "url", "params", "resp", "timestamp")
  )
  expect_equal(nrow(err), 1L)
  expect_equal(err$context, "unit_test")
  expect_equal(err$error, "Something went wrong")
  expect_equal(err$status, 500L)
  expect_equal(err$url, "https://example.com")
  expect_equal(err$params, "id=1")
  expect_equal(err$resp, "Internal Server Error")
  expect_s3_class(err$timestamp, "POSIXct")
})

test_that("cvd_error_tibble handles missing and NULL inputs", {
  err <- cvd_error_tibble(context = NULL, error = NULL)
  expect_true(is.na(err$context))
  expect_true(is.na(err$error))
  expect_true(is.na(err$status))
  expect_true(is.na(err$url))
  expect_true(is.na(err$params))
  expect_true(is.na(err$resp))
})

test_that("cvd_error_tibble truncates long response text", {
  long_resp <- paste(rep("x", 1000), collapse = "")
  err <- cvd_error_tibble(
    context = "trunc_test",
    error = "Too long",
    resp = long_resp,
    max_resp_chars = 100
  )
  expect_true(nchar(err$resp) <= 112) # 100 + " [truncated]"
  expect_match(err$resp, "truncated", fixed = TRUE)
})

test_that("cvd_error_tibble coerces types correctly", {
  err <- cvd_error_tibble(
    context = 123,
    error = TRUE,
    status = "404",
    url = 456,
    params = 789,
    resp = 101112
  )
  expect_type(err$context, "character")
  expect_type(err$error, "character")
  expect_type(err$status, "integer")
  expect_type(err$url, "character")
  expect_type(err$params, "character")
  expect_type(err$resp, "character")
})
