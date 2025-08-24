# tests/testthat/test-check_econanalyzr_df.R
#
# Comprehensive tests for `check_econanalyzr_df()` covering:
# - Happy path (already valid data): silent, returns a tibble, preserves names
# - Minimum column count, presence of required columns, and presence of final viz column
# - Duplicate name detection (including passing a tibble that already has duplicates)
# - Reordering behavior when all required columns + viz exist but are out of order
# - Date normalization:
#     * Date subclass -> warn & coerce to plain Date
#     * POSIXct/POSIXlt -> warn & coerce to Date using supplied timezone
#     * Invalid date type -> error
# - Value normalization:
#     * Integer (or other numeric non-double) -> warn & coerce to double
#     * Non-numeric -> error
# - Final schema checks for type/class mismatches in required columns
# - Final column name and type validations
# - Idempotence: running twice remains silent and unchanged
#
# NOTE: Do not call `library()` here. In `tests/testthat.R`, you should have:
#   library(testthat)
#   library(econanalyzr)
#   test_check("econanalyzr")

# Helper: build a minimal valid tibble in the correct order ---------------------
valid_df <- function(n = 3L) {
  tibble::tibble(
    date                  = as.Date("2024-01-01") + seq_len(n) - 1L,
    date_period_text      = rep("2024-01", n),
    value                 = as.double(seq_len(n)),    # double
    data_element_text     = rep("elem", n),
    data_measure_text     = rep("measure", n),
    date_measure_text     = rep("dm", n),
    data_transform_text   = rep("transform", n),
    geo_entity_type_text  = rep("nation", n),
    geo_entity_text       = rep("US", n),
    viz_type_text         = rep("line", n)
  )
}

# Expected required column order (first 9)
req_names <- c(
  "date", "date_period_text", "value", "data_element_text",
  "data_measure_text", "date_measure_text", "data_transform_text",
  "geo_entity_type_text", "geo_entity_text"
)

# ------------------------------------------------------------------------------
# Happy path: already valid
# ------------------------------------------------------------------------------

test_that("valid input: silent, returns tibble, preserves names", {
  x <- valid_df()
  expect_silent(y <- check_econanalyzr_df(x))
  expect_s3_class(y, "tbl_df")
  expect_identical(names(y), names(x))
  # Types/classes for first 9 columns meet spec
  expect_true(inherits(y$date, "Date"))
  expect_true(is.double(y$value))
  expect_type(y$date_period_text, "character")
  expect_identical(names(y)[length(y)], "viz_type_text")
  expect_type(y$viz_type_text, "character")
})

# ------------------------------------------------------------------------------
# Minimum columns / presence checks
# ------------------------------------------------------------------------------

test_that("too few columns (ncol < 10) errors with specific class", {
  df <- tibble::tibble(
    date = as.Date("2024-01-01"),
    date_period_text = "2024-01",
    value = 1,
    data_element_text = "elem",
    data_measure_text = "measure",
    date_measure_text = "dm",
    data_transform_text = "transform",
    geo_entity_type_text = "nation"
  )
  expect_error(check_econanalyzr_df(df), class = "econ_df_too_few_columns")
})

test_that("missing required columns triggers classed error", {
  df <- valid_df()
  df$geo_entity_text <- NULL                  # remove a required column
  df$optional_col <- "x"                      # keep width >= 10 so we pass the min-cols check
  expect_error(check_econanalyzr_df(df), class = "econ_df_missing_required_columns")
})

test_that("missing viz_type_text triggers classed error", {
  df <- valid_df()
  df$viz_type_text <- NULL
  df$optional_col  <- "x"                     # keep width >= 10
  expect_error(check_econanalyzr_df(df), class = "econ_df_missing_viz")
})


# ------------------------------------------------------------------------------
# Duplicate names
# ------------------------------------------------------------------------------

test_that("duplicate names are detected with classed error", {
  # Create a tibble that ALREADY has duplicate names; use name_repair='minimal'
  df <- tibble::tibble(
    date = as.Date("2024-01-01"),
    date_period_text = "2024-01",
    value = 1.0,
    data_element_text = "elem",
    data_measure_text = "measure",
    date_measure_text = "dm",
    data_transform_text = "transform",
    geo_entity_type_text = "nation",
    geo_entity_text = "US",
    viz_type_text = "line",
    value = 2.0,                          # <-- duplicate name
    .name_repair = "minimal"
  )
  expect_error(check_econanalyzr_df(df), class = "econ_df_duplicate_names")
})

# ------------------------------------------------------------------------------
# Reordering behavior
# ------------------------------------------------------------------------------

test_that("when required + viz exist but are out of order, function warns and reorders", {
  x <- valid_df()
  # Shuffle columns: put viz in the middle, move some required columns around,
  # and add an optional column in between.
  x$optional_col <- "keep_me"

  new_order <- c(
    "date_period_text", "date", "value", "optional_col", "geo_entity_text",
    "data_element_text", "data_measure_text", "date_measure_text",
    "geo_entity_type_text", "data_transform_text", "viz_type_text"
  )
  x <- x[, new_order]

  expect_warning(y <- check_econanalyzr_df(x), class = "econ_df_reordered_columns")
  # Expected final order: required 9 (in spec order), then optionals, then viz last
  expect_identical(
    names(y),
    c(req_names, "optional_col", "viz_type_text")
  )
})

# ------------------------------------------------------------------------------
# Date normalization
# ------------------------------------------------------------------------------

test_that("Date subclass is coerced to plain Date with a warning", {
  x <- valid_df()
  # Attach an extra class in front of "Date"
  subclass_date <- structure(x$date, class = c("my_date_subclass", class(x$date)))
  x$date <- subclass_date

  expect_warning(y <- check_econanalyzr_df(x), class = "econ_df_date_subclass_coerced_to_Date")
  expect_identical(class(y$date)[1], "Date")
})

test_that("POSIXct date is coerced using provided timezone, with a warning", {
  x <- valid_df(n = 1L)

  # 2024-03-10 23:30:00 America/New_York
  posix_et <- as.POSIXct("2024-03-10 23:30:00", tz = "America/New_York")
  x$date <- posix_et

  # Using UTC should bump the calendar date to 2024-03-11
  expect_warning(y_utc <- check_econanalyzr_df(x, datetime_tz = "UTC"),
                 class = "econ_df_datetime_coerced_to_Date")
  expect_equal(y_utc$date, as.Date("2024-03-11"))

  # Using America/New_York should keep 2024-03-10
  expect_warning(y_ny <- check_econanalyzr_df(x, datetime_tz = "America/New_York"),
                 class = "econ_df_datetime_coerced_to_Date")
  expect_equal(y_ny$date, as.Date("2024-03-10"))
})

test_that("invalid date type errors with classed error", {
  x <- valid_df()
  x$date <- "2024-01-01"  # character, not allowed
  expect_error(check_econanalyzr_df(x), class = "econ_df_bad_date_type")
})

# ------------------------------------------------------------------------------
# Value normalization
# ------------------------------------------------------------------------------

test_that("integer value column is coerced to double, with a warning", {
  x <- valid_df()
  x$value <- as.integer(c(1L, 2L, 3L))
  expect_warning(y <- check_econanalyzr_df(x), class = "econ_df_value_coerced_to_double")
  expect_true(is.double(y$value))
})

test_that("numeric-but-not-double value column is coerced to double (warning)", {
  # Simulate an unusual numeric class (e.g., custom numeric subclass)
  x <- valid_df()
  class(x$value) <- c("my_numeric", class(x$value))
  expect_warning(y <- check_econanalyzr_df(x), class = "econ_df_value_class_normalized")
  expect_true(is.double(y$value))
})

test_that("non-numeric value column errors with classed error", {
  x <- valid_df()
  x$value <- c("a", "b", "c")
  expect_error(check_econanalyzr_df(x), class = "econ_df_bad_required_types")
})

# ------------------------------------------------------------------------------
# Schema mismatches (other required columns)
# ------------------------------------------------------------------------------

test_that("required character columns that are factors trigger type/class mismatch error", {
  x <- valid_df()
  x$date_period_text <- factor(x$date_period_text)  # factor: typeof integer, class factor
  expect_error(check_econanalyzr_df(x), class = "econ_df_bad_required_types")
})

# ------------------------------------------------------------------------------
# Final column validations
# ------------------------------------------------------------------------------

test_that("final column name must be viz_type_text (missing viz errors)", {
  x <- valid_df()
  names(x)[ncol(x)] <- "not_viz"  # removes viz column
  expect_error(check_econanalyzr_df(x), class = "econ_df_missing_viz")
})


test_that("final column type must be character", {
  x <- valid_df()
  x$viz_type_text <- 1:3   # wrong type
  expect_error(check_econanalyzr_df(x), class = "econ_df_bad_last_type")
})

# ------------------------------------------------------------------------------
# Idempotence: running twice should be silent and unchanged
# ------------------------------------------------------------------------------

test_that("running the validator twice is idempotent", {
  x <- valid_df()
  y <- check_econanalyzr_df(x)                 # first run
  expect_silent(z <- check_econanalyzr_df(y))  # second run should be silent
  expect_identical(y, z)
})
