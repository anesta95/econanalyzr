# tests/testthat/test-trim_ws.R
#
# Tests for trim_ws():
# - Trims leading/trailing whitespace
# - Preserves internal spaces
# - Leaves NA as NA
# - Coerces non-character inputs safely
# - Vectorized behavior

test_that("trim_ws trims only edges and preserves NA", {
  x <- c("  a  ", "\tb\t", "\n c\n", NA, "  spaced   out  ")
  y <- econanalyzr:::trim_ws(x)  # internal helper

  expect_equal(y[1], "a")
  expect_equal(y[2], "b")
  expect_equal(y[3], "c")
  expect_true(is.na(y[4]))
  expect_equal(y[5], "spaced   out")  # internal spaces preserved
})

test_that("trim_ws handles non-character by coercion", {
  x <- c(1, 2.5, NA)
  y <- econanalyzr:::trim_ws(x)
  expect_identical(y, c("1", "2.5", NA_character_))
})

test_that("trim_ws is vectorized and stable on already-trimmed input", {
  x <- c("alpha", "beta", "gamma")
  y <- econanalyzr:::trim_ws(x)
  expect_identical(y, x)
})

