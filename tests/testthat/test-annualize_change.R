# tests/testthat/test-annualize_change.R
#
# Comprehensive tests for `annualize_change()`:
# - Correctness of the annualization formula across units (monthly, quarterly, annually, daily, weekly)
# - Vectorization and three-way scalar recycling (`start_values`, `end_values`, `time_elapsed`)
# - Classed validation errors for types, lengths, finiteness, and positivity
# - Proper handling of NA values (propagate to NA; do NOT error)
# - Numerical stability via log-space + expm1() for tiny changes
# - Overflow behavior mapped to NA for extreme inputs
#
# Reference formula checked in most tests:
#   r = expm1( (units_per_year / time_elapsed) * (log(end) - log(start)) )

test_that("scalar monthly example returns expected annualized rate", {
  # Scenario: single pair of values over 1 month.
  # Expectation: matches exact math using 12 months/year and log-space + expm1().
  r <- annualize_change(100, 103, time_elapsed = 1, time_unit = "monthly")

  # Compute the mathematically equivalent expression explicitly for clarity:
  # exponent = 12 / 1; z = exponent * (log(103) - log(100)); r = expm1(z)
  expected <- expm1(12 * (log(103) - log(100)))

  expect_type(r, "double")   # numeric double output
  expect_length(r, 1L)       # scalar in = scalar out
  expect_equal(r, expected, tolerance = 1e-12)
})

test_that("vectorization and scalar recycling work for start/end/time_elapsed", {
  # Case 1: start_values is scalar, end_values is vector, time_elapsed is scalar.
  # We expect start/time to recycle to the length of end_values.
  r1 <- annualize_change(
    start_values = 100,
    end_values   = c(101, 98, 120),
    time_elapsed = 2,
    time_unit    = "quarterly"
  )
  # quarterly => units_per_year = 4; elapsed = 2 => exponent = 4/2 = 2
  expnt <- 4 / 2
  expected <- expm1(expnt * (log(c(101, 98, 120)) - log(100)))
  expect_length(r1, 3L)
  expect_equal(r1, expected, tolerance = 1e-12)

  # Case 2: end_values is scalar, start_values & time_elapsed are vectors.
  # We expect end/time to recycle appropriately, element-wise.
  r2 <- annualize_change(
    start_values = c(100, 110, 80),
    end_values   = 120,
    time_elapsed = c(1, 2, 3),
    time_unit    = "monthly"
  )
  # monthly => units_per_year = 12 => exponent = 12 / time_elapsed
  expnt2 <- 12 / c(1, 2, 3)
  expected2 <- expm1(expnt2 * (log(120) - log(c(100, 110, 80))))
  expect_length(r2, 3L)
  expect_equal(r2, expected2, tolerance = 1e-12)
})

test_that("three-way incompatible lengths error with specific class", {
  # All three inputs must be either scalar or share a common length.
  # Here: lengths are 2, 3, and 4 => incompatible => classed error.
  expect_error(
    annualize_change(
      start_values = c(100, 110),           # len 2
      end_values   = c(101, 105, 120),      # len 3
      time_elapsed = c(1, 2, 3, 4),         # len 4
      time_unit    = "monthly"
    ),
    class = "annualize_change_bad_length_mismatch"
  )
})

test_that("type validation errors are classed and informative", {
  # Non-numeric start/end => classed type error.
  expect_error(
    annualize_change("100", 103, 1, "monthly"),
    class = "annualize_change_bad_type"
  )
  # Non-numeric time_elapsed => classed type error.
  expect_error(
    annualize_change(100, 103, "1", "monthly"),
    class = "annualize_change_bad_time_elapsed_type"
  )
  # year_length must be a numeric scalar => classed error.
  expect_error(
    annualize_change(100, 103, 1, "monthly", year_length = "365"),
    class = "annualize_change_bad_year_length"
  )
})

test_that("magnitude validation (positivity/finite) triggers classed errors", {
  # Non-finite start/end (Inf/NaN) are not allowed; NA is allowed separately.
  expect_error(
    annualize_change(c(100, Inf), c(101, 102), c(1, 1), "monthly"),
    class = "annualize_change_nonfinite_values"
  )
  # Start/end must be strictly positive (log requires > 0).
  expect_error(
    annualize_change(c(100, 0), c(101, 102), c(1, 1), "monthly"),
    class = "annualize_change_nonpositive_values"
  )
  expect_error(
    annualize_change(c(100, -5), c(101, 102), c(1, 1), "monthly"),
    class = "annualize_change_nonpositive_values"
  )
  # time_elapsed must be finite and > 0 (can't repeat negative/zero times).
  expect_error(
    annualize_change(100, 101, c(1, 0), "monthly"),
    class = "annualize_change_bad_time_elapsed_values"
  )
  expect_error(
    annualize_change(100, 101, c(1, Inf), "monthly"),
    class = "annualize_change_bad_time_elapsed_values"
  )
})

test_that("annually unit behaves as expected", {
  # Over 2 years, moving 100 -> 121: annualized rate should be sqrt(1.21) - 1 = 0.1.
  # This checks the special case units_per_year = 1 (annually).
  r <- annualize_change(100, 121, time_elapsed = 2, time_unit = "annually")
  expect_equal(r, sqrt(121/100) - 1, tolerance = 1e-12)
})

test_that("daily and weekly use year_length consistently", {
  # Daily: exponent = year_length / t. Default year_length = 365.2425 (Gregorian average).
  r_daily <- annualize_change(100, 101, time_elapsed = 10, time_unit = "daily")
  expected_daily <- expm1(365.2425 / 10 * (log(101) - log(100)))
  expect_equal(r_daily, expected_daily, tolerance = 1e-12)

  # Weekly: exponent = (year_length / 7) / t (≈ 52.1775 weeks/year by default).
  r_weekly <- annualize_change(100, 101, time_elapsed = 2, time_unit = "weekly")
  expected_weekly <- expm1((365.2425 / 7) / 2 * (log(101) - log(100)))
  expect_equal(r_weekly, expected_weekly, tolerance = 1e-12)

  # Override year_length to test a different weekly convention (exact 52 weeks/year => 364 days).
  r_weekly_364 <- annualize_change(
    100, 101, time_elapsed = 2, time_unit = "weekly", year_length = 364
  )
  expected_weekly_364 <- expm1((364 / 7) / 2 * (log(101) - log(100)))
  expect_equal(r_weekly_364, expected_weekly_364, tolerance = 1e-12)
})

test_that("NA values propagate to NA results (no error)", {
  # NA inputs should NOT trigger an error; they propagate to NA outputs.
  # Only Inf/-Inf/NaN should error (covered in the finiteness test above).
  r <- annualize_change(
    start_values = c(100, NA, 120),
    end_values   = c(110, 115, NA),
    time_elapsed = 1,
    time_unit    = "monthly"
  )
  expect_length(r, 3L)
  expect_true(is.na(r[2L]))   # NA start -> NA result
  expect_true(is.na(r[3L]))   # NA end   -> NA result
  expect_false(is.na(r[1L]))  # non-NA pair remains computed
})

test_that("numerical stability: small changes use expm1 and match log-space formula", {
  # Stress the tiny-change regime: r = expm1(z) with |z| << 1.
  # Using expm1(z) avoids catastrophic cancellation vs exp(z) - 1.
  start <- 1
  end   <- 1 + 1e-12
  z     <- 12 * (log(end) - log(start))

  # Primary check: implementation matches the mathematically correct expm1(z).
  r <- annualize_change(start, end, time_elapsed = 1, time_unit = "monthly")
  expect_equal(r, expm1(z), tolerance = 1e-15)

  # Secondary check: first-order approximation r ≈ z; the error should be ~ z^2/2.
  # We bound |r - z| by ~0.6 * z^2 to allow for floating-point noise, plus a tiny floor.
  err <- abs(r - z)
  expect_lt(err, 0.6 * z^2 + 1e-24)
})

test_that("extreme cases that would overflow are mapped to NA", {
  # Huge ratios/exponents can overflow expm1(z) to +Inf; we sanitize to NA.
  # This verifies the cleanup path that sets non-finite results to NA_real_.
  r <- annualize_change(
    start_values = 1,
    end_values   = 1e308,     # astronomically large end value
    time_elapsed = 1,
    time_unit    = "monthly"  # exponent = 12
  )
  expect_true(is.na(r))
})
