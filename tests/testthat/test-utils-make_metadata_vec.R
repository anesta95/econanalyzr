# tests/testthat/test-utils-make_metadata_vec.R
#
# Tests for make_metadata_vec():
# - Single unique value -> returns that value, lowercased and trimmed
# - Multiple unique values -> "n_<aspect>" token
# - All-NA -> warning with class 'econ_csv_meta_all_na' and uses 'na' token
# - Semicolons are replaced with spaces
# - Order of tokens follows column order
# - Zero-column input -> character(0)

test_that("make_metadata_vec basic tokenization behavior", {
  df <- data.frame(
    data_element_text    = c("Quits Rate", "Quits Rate"),
    data_transform_text  = c("Seasonally Adjusted", "Seasonally Adjusted"),
    geo_entity_text      = c("US", "Canada"),
    stringsAsFactors = FALSE
  )

  toks <- econanalyzr:::make_metadata_vec(df)

  # Order follows column order
  expect_length(toks, 3L)

  # Singletons lowercased / space-preserved (underscoring happens later in sanitize step)
  expect_equal(toks[1], "quits rate")
  expect_equal(toks[2], "seasonally adjusted")

  # Multi-valued column -> "2_geo_entity"
  expect_equal(toks[3], "2_geo_entity")
})

test_that("make_metadata_vec handles all-NA column with a classed warning", {
  df <- data.frame(
    data_element_text   = c(NA, NA),
    geo_entity_text     = c("US", "US"),
    stringsAsFactors = FALSE
  )

  expect_warning(
    toks <- econanalyzr:::make_metadata_vec(df),
    class = "econ_csv_meta_all_na"
  )
  expect_equal(toks[1], "na")
  expect_equal(toks[2], "us")
})

test_that("make_metadata_vec replaces semicolons, trims whitespace, preserves order", {
  df <- data.frame(
    weird_text   = c("A;B ; C", "A;B ; C"),
    other_text   = c("  Foo  ", "  Foo  "),
    stringsAsFactors = FALSE
  )
  colnames(df) <- c("weird_text", "other_text")

  toks <- econanalyzr:::make_metadata_vec(df)
  expect_equal(toks[1], "a b   c")  # semicolons -> spaces, lowercased
  expect_equal(toks[2], "foo")      # trimmed & lowercased
})

test_that("make_metadata_vec returns character(0) for 0-column data frame", {
  d0 <- data.frame(row.names = 1)
  toks <- econanalyzr:::make_metadata_vec(d0)
  expect_identical(toks, character(0))
})
