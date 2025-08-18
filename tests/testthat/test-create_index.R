# tests/testthat/test-create_index.R

# These tests exercise create_index():
# - Correct math & behavior for base-100 and percent-change variants
# - Position vs. name selection of the base (names only when available)
# - Validation of idx_pos (bounds, integer-ish, NA/Inf, bad names, duplicates)
# - Validation of idx_type (must be 100 or 0)
# - Validation of num_vec (type & length)
# - Base value invalid (0/NA/NaN/Inf) -> classed error
# - Non-finite non-base values -> single classed warning + propagation
# - Names preservation in output

test_that("happy path: base-100 index scales relative to base position", {
  x <- c(50, 100, 150)
  # Default: idx_pos = 1, idx_type = 100
  res1 <- create_index(x)
  # Base-100: value/base * 100
  expect_equal(res1, (x / x[1]) * 100, tolerance = 1e-12)

  # Base at second position
  res2 <- create_index(x, idx_pos = 2)
  expect_equal(res2, (x / x[2]) * 100, tolerance = 1e-12)

  # Base at third position
  res3 <- create_index(x, idx_pos = 3)
  expect_equal(res3, (x / x[3]) * 100, tolerance = 1e-12)
})

test_that("happy path: percent-change index returns 0 at base and %Diff elsewhere", {
  x <- c(90, 100, 120)
  # idx_type = 0 -> ((value/base) - 1) * 100
  res <- create_index(x, idx_pos = 2, idx_type = 0)
  expected <- ((x / x[2]) - 1) * 100
  expect_equal(res, expected, tolerance = 1e-12)
  expect_equal(res[2], 0) # base becomes exactly 0%
})

test_that("idx_pos accepts integer-ish numerics (double whole numbers)", {
  x <- c(10, 20, 40)
  # 2 is a double by default in R; function should accept it as a valid index
  res <- create_index(x, idx_pos = 2.0, idx_type = 100)
  expect_equal(res, (x / x[2]) * 100, tolerance = 1e-12)
})

test_that("name-based base selection works only when names are present", {
  x_named <- c(a = 80, b = 100, c = 120)

  # Select by name (unique)
  res_b <- create_index(x_named, idx_pos = "b", idx_type = 100)
  expect_equal(res_b, (x_named / x_named["b"]) * 100, tolerance = 1e-12)

  # Percent-change by name
  res_c <- create_index(x_named, idx_pos = "c", idx_type = 0)
  expect_equal(res_c, ((x_named / x_named["c"]) - 1) * 100, tolerance = 1e-12)

  # Unnamed vector: character idx_pos should be rejected
  x_unnamed <- unname(x_named)
  expect_error(
    create_index(x_unnamed, idx_pos = "b"),
    class = "create_index_bad_idx_pos"
  )
})

test_that("duplicate names cause an ambiguity error when selecting by name", {
  x_dup <- c(a = 10, a = 20, b = 30)  # duplicate "a"
  # Asking for "a" would match two positions -> ambiguous -> error
  expect_error(
    create_index(x_dup, idx_pos = "a"),
    class = "create_index_ambiguous_idx_pos"
  )
})

test_that("idx_pos validation: out-of-bounds, fractional, NA/Inf are rejected", {
  x <- c(1, 2, 3, 4)

  # Out of bounds: 0, negative, and > length
  expect_error(create_index(x, idx_pos = 0),  class = "create_index_bad_idx_pos")
  expect_error(create_index(x, idx_pos = -1), class = "create_index_bad_idx_pos")
  expect_error(create_index(x, idx_pos = 5),  class = "create_index_bad_idx_pos")

  # Fractional (non integer-ish)
  expect_error(create_index(x, idx_pos = 2.7), class = "create_index_bad_idx_pos")

  # NA or Inf
  expect_error(create_index(x, idx_pos = NA_real_), class = "create_index_bad_idx_pos")
  expect_error(create_index(x, idx_pos = Inf),      class = "create_index_bad_idx_pos")
})

test_that("idx_type validation: must be 100 or 0 (numeric scalar)", {
  x <- c(10, 20, 30)
  expect_error(create_index(x, idx_type = 50),       class = "create_index_bad_idx_type")
  expect_error(create_index(x, idx_type = c(100,0)), class = "create_index_bad_idx_type")
  expect_error(create_index(x, idx_type = "100"),    class = "create_index_bad_idx_type")

  # Sanity: valid types work
  expect_silent(create_index(x, idx_type = 100))
  expect_silent(create_index(x, idx_type = 0))
})

test_that("num_vec validation: must be numeric and length > 1", {
  expect_error(create_index(1, idx_pos = 1),          class = "create_index_bad_num_vec") # length 1
  expect_error(create_index(list(1,2), idx_pos = 1),  class = "create_index_bad_num_vec") # not numeric
  expect_error(create_index("a", idx_pos = 1),        class = "create_index_bad_num_vec")
})

test_that("base value invalid: 0/NA/NaN/Inf triggers classed error", {
  # Base is zero
  expect_error(create_index(c(0, 1, 2), idx_pos = 1), class = "create_index_bad_base_value")

  # Base is NA
  expect_error(create_index(c(NA, 1, 2), idx_pos = 1), class = "create_index_bad_base_value")

  # Base is NaN
  expect_error(create_index(c(NaN, 1, 2), idx_pos = 1), class = "create_index_bad_base_value")

  # Base is Inf
  expect_error(create_index(c(Inf, 1, 2), idx_pos = 1), class = "create_index_bad_base_value")
})

test_that("non-base NA/NaN/Inf values trigger a single warning and propagate", {
  x <- c(100, NA, 200, NaN, Inf)
  # Choose base at position 1 (100), idx_type 100
  expect_warning(
    idx <- create_index(x, idx_pos = 1, idx_type = 100),
    class = "create_index_nonfinite_values"
  )
  # Positions with NA/NaN/Inf should produce NA/NaN/Inf divisions as usual
  expect_equal(idx[1], 100)                 # base -> 100
  expect_true(is.na(idx[2]))                #  NA / 100 -> NA
  expect_true(is.finite(idx[3]))            # 200 / 100 * 100 -> 200
  expect_true(is.nan(idx[4]))               # NaN / 100 -> NaN
  expect_true(is.infinite(idx[5]))          # Inf / 100 -> Inf
})

test_that("names are preserved in the output", {
  x <- c(a = 80, b = 100, c = 120)

  res_100 <- create_index(x, idx_pos = "b", idx_type = 100)
  expect_equal(names(res_100), names(x))    # names preserved
  expect_equal(res_100[["b"]], 100)           # base -> 100

  res_0 <- create_index(x, idx_pos = "b", idx_type = 0)
  expect_equal(names(res_0), names(x))
  expect_equal(res_0[["b"]], 0)               # base -> 0%
})
