# tests/testthat/test-percent_change.R

# These tests exercise percent_change():
# - Correct math for scalar and vector inputs
# - Recycling semantics via vctrs (length-1 or common length only)
# - Classed errors for type/length/finiteness problems
# - Classed warnings for NA inputs and zero denominators
# - Safe behavior in pipelines (no crashes on NA or zero)

test_that("happy path: scalar and vectorized math are correct", {
  # Scalar → single value
  expect_equal(percent_change(100, 120), 0.20, tolerance = 1e-12)

  # Vectorized pairwise computation
  expect_equal(
    percent_change(c(100, 80), c(110, 72)),
    c(0.10, -0.10),
    tolerance = 1e-12
  )

  # No warnings in happy path
  expect_silent(percent_change(c(10, 20, 50), c(15, 10, 60)))
})

test_that("recycling: scalars recycle to common length; incompatible lengths error", {
  # start scalar recycles to match end length
  expect_equal(
    percent_change(100, c(110, 120, 130)),
    c(0.10, 0.20, 0.30),
    tolerance = 1e-12
  )

  # end scalar recycles to match start length
  expect_equal(
    percent_change(c(100, 200, 400), 440),
    c(3.40, 1.20, 0.10),
    tolerance = 1e-12
  )

  # Incompatible lengths (none scalar, not equal) → classed error
  expect_error(
    percent_change(1:2, 1:3),
    class = "percent_change_bad_length_mismatch"
  )
})

test_that("NULL inputs are rejected with a classed error", {
  expect_error(percent_change(NULL, 1), class = "percent_change_null_input")
  expect_error(percent_change(1, NULL), class = "percent_change_null_input")
})

test_that("type validation: non-numeric inputs raise a classed error", {
  expect_error(percent_change("100", 120), class = "percent_change_bad_type")
  expect_error(percent_change(100, list(120)), class = "percent_change_bad_type")
})

test_that("finiteness validation: Inf/-Inf/NaN are rejected; NA is allowed", {
  # Error on non-finite numbers
  expect_error(
    percent_change(c(100, Inf), c(110, 120)),
    class = "percent_change_nonfinite_values"
  )
  expect_error(
    percent_change(c(100, NaN), c(110, 120)),
    class = "percent_change_nonfinite_values"
  )
  expect_error(
    percent_change(100, -Inf),
    class = "percent_change_nonfinite_values"
  )

  # NA is allowed and propagates (with a warning)
  expect_warning(
    res <- percent_change(c(100, NA), c(110, 120)),
    class = "percent_change_na_input"
  )
  expect_equal(res, c(0.10, NA_real_), tolerance = 1e-12)
})

test_that("zero denominators return NA with a single classed warning", {
  # start == 0 is undefined; function returns NA for those positions and warns
  expect_warning(
    res <- percent_change(c(0, 10, 0), c(5, 15, 0)),
    class = "percent_change_zero_denominator"
  )
  expect_equal(res, c(NA_real_, 0.5, NA_real_))
})

test_that("mixed NA and zero cases warn at most once and return expected values", {
  # Contains both NA and zero; we expect two distinct warnings (NA + zero)
  expect_warning(
    expect_warning(
      res <- percent_change(c(0, NA, 10), c(5, 12, 15)),
      class = "percent_change_na_input"
    ),
    class = "percent_change_zero_denominator"
  )
  # Positions: 1 -> zero denom => NA, 2 -> NA input => NA, 3 -> valid
  expect_equal(res, c(NA_real_, NA_real_, 0.5))
})

test_that("no spurious warnings when inputs are clean", {
  expect_silent(percent_change(c(1, 2, 3), c(2, 4, 6)))
})

# Optional: numerical sanity on tiny deltas (should track linear approx)
# This is not strictly necessary for percent_change, but it's a nice smoke test
test_that("tiny changes behave sensibly (linear regime)", {
  start <- 100
  end   <- 100 + 1e-9
  pc <- percent_change(start, end)
  # Approximately (end - start)/start by definition
  expect_equal(pc, (end - start) / start, tolerance = 1e-16)
})

