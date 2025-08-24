# tests/testthat/test-sanitize_filename.R
#
# Tests for sanitize_filename():
# - Lowercases
# - Replaces whitespace with underscores
# - Removes disallowed characters
# - Collapses multiple underscores
# - Trims leading/trailing underscores/dots
# - Preserves NA
# - Vectorized behavior

test_that("sanitize_filename produces conservative, safe slugs", {
  x <- "  My File.Name v1.0  "
  y <- econanalyzr:::sanitize_filename(x)
  # "my file.name v1.0" -> "my_file.name_v1.0" (whitespace->_, lowercased, trimmed)
  expect_equal(y, "my_file.name_v1.0")
})

test_that("sanitize_filename removes disallowed characters and collapses underscores", {
  x <- "A@B#C$ D%E^F&G* (H) + I = J__K"
  y <- econanalyzr:::sanitize_filename(x)
  # Disallowed symbols -> underscores; multiple underscores collapsed; lowercased
  expect_equal(y, "a_b_c_d_e_f_g_h_i_j_k")
})

test_that("sanitize_filename trims leading/trailing underscores and dots", {
  x <- "___..weird..name__"
  y <- econanalyzr:::sanitize_filename(x)
  expect_equal(y, "weird..name")  # internal dots preserved; edges trimmed
})

test_that("sanitize_filename preserves NA and is vectorized", {
  x <- c("A B", NA, "C")
  y <- econanalyzr:::sanitize_filename(x)
  expect_identical(y, c("a_b", NA_character_, "c"))
})

