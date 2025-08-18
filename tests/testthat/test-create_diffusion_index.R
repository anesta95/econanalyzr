# tests/testthat/test-create_diffusion_index.R
#
# Comprehensive tests for `create_diffusion_index()`:
# - Correct math for each method (Federal Reserve, IHS-PMI, Conference Board)
# - Vectorization and scalar recycling (where applicable)
# - Classed validation errors (types, finiteness, bounds, missing args, length mismatch)
# - Clear, classed warnings for ignored arguments and NA handling (Conference Board)
# - Threshold boundary behavior (±0.05%) for Conference Board encoding
# - Return-shape guarantees (vector for FR/IHS, scalar for CB)

test_that("Federal Reserve: base formula and vectorization are correct", {
  # FR formula: (pct_increased - pct_decreased) * 100
  inc <- c(0.60, 0.55, 0.40)
  dec <- c(0.20, 0.25, 0.35)
  res <- create_diffusion_index(pct_increased = inc, pct_decreased = dec, idx_type = "Federal Reserve")

  expected <- (inc - dec) * 100
  expect_type(res, "double")
  expect_length(res, length(inc))
  expect_equal(res, expected, tolerance = 1e-12)

  # Scalar recycling: scalar inc with vector dec (and vice versa)
  res1 <- create_diffusion_index(pct_increased = 0.50, pct_decreased = dec, idx_type = "Federal Reserve")
  expect_equal(res1, (0.50 - dec) * 100, tolerance = 1e-12)

  res2 <- create_diffusion_index(pct_increased = inc, pct_decreased = 0.30, idx_type = "Federal Reserve")
  expect_equal(res2, (inc - 0.30) * 100, tolerance = 1e-12)
})

test_that("Federal Reserve: NA inputs propagate; no warning for NA proportions", {
  # NA should propagate to NA; function should not warn on NA proportions
  inc <- c(0.60, NA, 0.40)
  dec <- c(0.20, 0.25, NA)
  expect_silent(
    res <- create_diffusion_index(pct_increased = inc, pct_decreased = dec, idx_type = "Federal Reserve")
  )
  expect_equal(res[1], (0.60 - 0.20) * 100, tolerance = 1e-12)
  expect_true(is.na(res[2]))
  expect_true(is.na(res[3]))
})

test_that("Federal Reserve: supplying unused args triggers a single classed warning", {
  # pct_unchanged and pct_change should be ignored with a clear warning
  expect_warning(
    create_diffusion_index(
      pct_increased = 0.55,
      pct_decreased = 0.10,
      pct_unchanged = 0.35,
      pct_change    = 0.01,
      idx_type      = "Federal Reserve"
    ),
    class = "create_diffusion_index_ignoring_args"
  )
})

test_that("IHS-PMI: base formula and vectorization are correct", {
  # IHS-PMI formula: (pct_increased + 0.5 * pct_unchanged) * 100
  inc <- c(0.55, 0.50, 0.45)
  unc <- c(0.30, 0.35, 0.40)
  res <- create_diffusion_index(pct_increased = inc, pct_unchanged = unc, idx_type = "IHS-PMI")

  expected <- (inc + 0.5 * unc) * 100
  expect_type(res, "double")
  expect_length(res, length(inc))
  expect_equal(res, expected, tolerance = 1e-12)

  # Scalar recycling
  res1 <- create_diffusion_index(pct_increased = 0.5, pct_unchanged = unc, idx_type = "IHS-PMI")
  expect_equal(res1, (0.5 + 0.5 * unc) * 100, tolerance = 1e-12)

  res2 <- create_diffusion_index(pct_increased = inc, pct_unchanged = 0.3, idx_type = "IHS-PMI")
  expect_equal(res2, (inc + 0.5 * 0.3) * 100, tolerance = 1e-12)
})

test_that("IHS-PMI: NA inputs propagate; no warning for NA proportions", {
  inc <- c(0.55, NA, 0.45)
  unc <- c(0.30, 0.35, NA)
  expect_silent(
    res <- create_diffusion_index(pct_increased = inc, pct_unchanged = unc, idx_type = "IHS-PMI")
  )
  expect_equal(res[1], (0.55 + 0.5 * 0.30) * 100, tolerance = 1e-12)
  expect_true(is.na(res[2]))
  expect_true(is.na(res[3]))
})

test_that("IHS-PMI: supplying unused args triggers a single classed warning", {
  expect_warning(
    create_diffusion_index(
      pct_increased = 0.55,
      pct_unchanged = 0.30,
      pct_decreased = 0.15,
      pct_change    = 0.01,
      idx_type      = "IHS-PMI"
    ),
    class = "create_diffusion_index_ignoring_args"
  )
})

test_that("Conference Board: encoding, averaging, and shape are correct", {
  # Threshold is ±0.0005 (0.05%):
  # > +0.0005  -> 1
  # < -0.0005  -> 0
  # otherwise  -> 0.5
  pc <- c(+0.0010, +0.0005, 0.0000, -0.0005, -0.0020)
  # Encoded:    1.0        0.5       0.5       0.5        0.0
  # Mean = (1 + 0.5 + 0.5 + 0.5 + 0) / 5 = 0.5; index = 50
  res <- create_diffusion_index(pct_change = pc, idx_type = "Conference Board")
  expect_type(res, "double")
  expect_length(res, 1L)      # Conference Board returns a scalar (mean * 100)
  expect_equal(res, 50, tolerance = 1e-12)

  # Pure up/down
  res_up   <- create_diffusion_index(pct_change = c(0.01, 0.02), idx_type = "Conference Board")
  res_down <- create_diffusion_index(pct_change = c(-0.02, -0.03), idx_type = "Conference Board")
  expect_equal(res_up, 100, tolerance = 1e-12)   # all ups => mean 1 * 100
  expect_equal(res_down, 0, tolerance = 1e-12)   # all downs => mean 0 * 100
})

test_that("Conference Board: NA handling warns and drops NA from mean", {
  pc <- c(0.001, NA, -0.001)   # encoded: 1, NA, 0 => mean = 0.5 => index = 50
  expect_warning(
    res <- create_diffusion_index(pct_change = pc, idx_type = "Conference Board"),
    class = "create_diffusion_index_na_dropped"
  )
  expect_equal(res, 50, tolerance = 1e-12)
})

test_that("Conference Board: all NA returns NA with a specific warning", {
  pc <- c(NA_real_, NA_real_)
  expect_warning(
    res <- create_diffusion_index(pct_change = pc, idx_type = "Conference Board"),
    class = "create_diffusion_index_all_na"
  )
  expect_true(is.na(res))
})

test_that("Conference Board: supplying unused args triggers a classed warning", {
  expect_warning(
    create_diffusion_index(
      pct_change    = c(0.001, -0.001),
      pct_increased = 0.6,
      pct_decreased = 0.2,
      pct_unchanged = 0.2,
      idx_type      = "Conference Board"
    ),
    class = "create_diffusion_index_ignoring_args"
  )
})

# ----------------------------
# Validation: types & finiteness
# ----------------------------

test_that("Type validation errors are classed", {
  expect_error(
    create_diffusion_index(pct_increased = "0.6", pct_decreased = 0.2, idx_type = "Federal Reserve"),
    class = "create_diffusion_index_bad_type"
  )
  expect_error(
    create_diffusion_index(pct_increased = 0.6, pct_unchanged = "0.3", idx_type = "IHS-PMI"),
    class = "create_diffusion_index_bad_type"
  )
  expect_error(
    create_diffusion_index(pct_change = "0.001", idx_type = "Conference Board"),
    class = "create_diffusion_index_bad_type"
  )
})

test_that("Non-finite values (Inf/-Inf/NaN) are rejected with classed errors", {
  expect_error(
    create_diffusion_index(pct_increased = c(0.5, Inf), pct_decreased = 0.1, idx_type = "Federal Reserve"),
    class = "create_diffusion_index_nonfinite"
  )
  expect_error(
    create_diffusion_index(pct_increased = 0.5, pct_unchanged = NaN, idx_type = "IHS-PMI"),
    class = "create_diffusion_index_nonfinite"
  )
  expect_error(
    create_diffusion_index(pct_change = c(0.001, -Inf), idx_type = "Conference Board"),
    class = "create_diffusion_index_nonfinite"
  )
})

test_that("Bounds validation for proportions [0,1] is enforced", {
  # pct_increased outside [0,1] -> error
  expect_error(
    create_diffusion_index(pct_increased = 1.01, pct_decreased = 0.0, idx_type = "Federal Reserve"),
    class = "create_diffusion_index_out_of_bounds"
  )
  # pct_unchanged outside [0,1] -> error
  expect_error(
    create_diffusion_index(pct_increased = 0.6, pct_unchanged = -0.05, idx_type = "IHS-PMI"),
    class = "create_diffusion_index_out_of_bounds"
  )
})

# ----------------------------
# Validation: missing args & lengths
# ----------------------------

test_that("Missing required arguments raise classed errors", {
  expect_error(
    create_diffusion_index(pct_increased = 0.6, idx_type = "Federal Reserve"),
    class = "create_diffusion_index_missing_args"
  )
  expect_error(
    create_diffusion_index(pct_unchanged = 0.3, idx_type = "IHS-PMI"),
    class = "create_diffusion_index_missing_args"
  )
  expect_error(
    create_diffusion_index(idx_type = "Conference Board"),
    class = "create_diffusion_index_missing_args"
  )
})

test_that("Length mismatch in paired inputs raises a classed error", {
  # Incompatible (none scalar): lengths 2 vs 3
  expect_error(
    create_diffusion_index(
      pct_increased = c(0.6, 0.5),
      pct_decreased = c(0.2, 0.3, 0.1),
      idx_type      = "Federal Reserve"
    ),
    class = "create_diffusion_index_bad_length"
  )

  expect_error(
    create_diffusion_index(
      pct_increased = c(0.6, 0.5, 0.4),
      pct_unchanged = c(0.3, 0.2),
      idx_type      = "IHS-PMI"
    ),
    class = "create_diffusion_index_bad_length"
  )
})

# ----------------------------
# Threshold boundary behavior (Conference Board)
# ----------------------------

test_that("Conference Board threshold boundaries (±0.05%) encode as unchanged (0.5)", {
  pc <- c(+5e-4, -5e-4)  # exactly on the boundary
  # Both should map to 0.5; mean = 0.5 -> index = 50
  res <- create_diffusion_index(pct_change = pc, idx_type = "Conference Board")
  expect_equal(res, 50, tolerance = 1e-12)
})
