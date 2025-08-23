# tests/testthat/test-make_file_name_date.R
#
# Unit tests for the internal helper make_file_name_date():
# - Accepts Date vectors and POSIXt (coerces to Date).
# - Single unique non-NA date -> "YYYY-MM-DD".
# - Multiple non-NA dates -> "maxDate_minDate" (YYYY-MM-DD_YYYY-MM-DD).
# - Ignores NA values when computing uniqueness/range.
# - All-NA dates -> classed error.
# - Wrong type -> classed error.
# - Unsorted / duplicate inputs handled consistently.

test_that("single unique date returns a single YYYY-MM-DD string", {
  d <- as.Date(c("2025-05-01", "2025-05-01", NA))
  out <- econanalyzr:::make_file_name_date(d)
  expect_type(out, "character")
  expect_equal(out, "2025-05-01")
})

test_that("multiple dates return max_min with underscore separator", {
  d <- as.Date(c("2024-12-31", "2024-01-01", "2024-06-15"))
  # Expected max first, min second
  out <- econanalyzr:::make_file_name_date(d)
  expect_equal(out, "2024-12-31_2024-01-01")
})

test_that("NA dates are ignored in uniqueness and range", {
  d <- as.Date(c(NA, "2023-02-01", NA, "2023-02-01", NA))
  out <- econanalyzr:::make_file_name_date(d)
  expect_equal(out, "2023-02-01")

  d2 <- as.Date(c(NA, "2023-02-01", "2023-04-01", NA))
  out2 <- econanalyzr:::make_file_name_date(d2)
  expect_equal(out2, "2023-04-01_2023-02-01")
})

test_that("POSIXct input is accepted and coerced to Date", {
  # Midday time to avoid timezone edge cases
  p <- as.POSIXct(c("2022-07-04 12:00:00", "2022-07-05 12:00:00"), tz = "UTC")
  out <- econanalyzr:::make_file_name_date(p)
  expect_equal(out, "2022-07-05_2022-07-04")
})

test_that("POSIXlt input is accepted and coerced to Date", {
  p <- as.POSIXlt(c("2020-02-29 12:34:00", "2020-03-01 01:00:00"), tz = "UTC")
  out <- econanalyzr:::make_file_name_date(p)
  expect_equal(out, "2020-03-01_2020-02-29")
})

test_that("all-NA dates error with class 'make_file_name_date_all_na'", {
  d <- as.Date(c(NA, NA))
  expect_error(
    econanalyzr:::make_file_name_date(d),
    class = "make_file_name_date_all_na"
  )
})

test_that("wrong type errors with class 'make_file_name_date_bad_type'", {
  expect_error(
    econanalyzr:::make_file_name_date("2024-01-01"),
    class = "make_file_name_date_bad_type"
  )
  expect_error(
    econanalyzr:::make_file_name_date(1:3),
    class = "make_file_name_date_bad_type"
  )
})

test_that("unsorted input and duplicates do not affect output", {
  d <- as.Date(c("2021-10-10", "2021-01-01", "2021-10-10", "2021-05-05"))
  out <- econanalyzr:::make_file_name_date(d)
  expect_equal(out, "2021-10-10_2021-01-01")
})
