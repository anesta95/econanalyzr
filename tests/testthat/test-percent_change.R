test_that("computes correct percent change for scalar values", {
  expect_equal(percent_change(100, 120), 0.20)
  expect_equal(percent_change(200, 100), -0.50)
  expect_equal(percent_change(100, 100), 0)
})

test_that("vectorized behavior with automatic recycling", {
  expect_equal(
    percent_change(c(100, 200, 300), 330),
    c(2.30, 0.65, 0.10)
  )
  expect_equal(
    percent_change(100, c(120, 140, 160)),
    c(0.20, 0.40, 0.60)
  )
})

test_that("returns NA and warns when any inputs are NA", {
  expect_warning(
    result <- percent_change(c(100, NA, 200), c(110, 120, NA)),
    regexp = "One or more input values are NA"
  )
  expect_equal(result, c(0.10, NA, NA))
})

test_that("throws errors for non-numeric or non-finite inputs", {
  expect_error(percent_change("100", 120), "must be numeric")
  expect_error(percent_change(100, "120"), "must be numeric")
  expect_error(percent_change(Inf, 120), "must be finite")
  expect_error(percent_change(100, NaN), "must be finite")
})

test_that("throws error if either input is NULL", {
  expect_error(percent_change(NULL, 100), "cannot be NULL")
  expect_error(percent_change(100, NULL), "cannot be NULL")
})

test_that("warns when dividing by positive or negative zero", {
  expect_warning(res1 <- percent_change(0, 100), "Division by zero")
  expect_equal(res1, Inf)

  expect_warning(res2 <- percent_change(-0, 100), "Division by zero")
  expect_equal(res2, -Inf)

  expect_warning(res3 <- percent_change(0, 0), "Division by zero")
  expect_true(is.nan(res3))
})
