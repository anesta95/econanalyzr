# tests/testthat/test-econ_value_summary.R
#
# PURPOSE
# -------
# Comprehensive tests for econ_value_summary():
# - Validates date filtering via membership (dates) and closed ranges (date_range)
# - Enforces numeric-only target column
# - Properly applies arbitrary .fun with NA handling done *before* .fun
# - Emits classed warnings for empty filters; classed errors for misuse
# - Coerces POSIXt inputs to Date deterministically
#
# STYLE NOTES
# -----------
# - A small helper builds a correctly structured econanalyzr tibble so tests
#   focus on econ_value_summary() logic (not schema issues).

# Helper: build a minimal, *valid* econanalyzr tibble with known values/dates.
# Values are 1..n as double; if n >= 4, insert an NA at position 4 to exercise NA logic.
.valid_econ_df <- function(n = 6L, start = as.Date("2025-01-01")) {
  vals <- as.double(seq_len(n))
  if (n >= 4L) vals[4L] <- NA_real_
  tibble::tibble(
    date                  = start + (seq_len(n) - 1L),
    date_period_text      = rep(format(start, "%Y-%m"), n),
    value                 = vals,
    data_element_text     = rep("Quits Rate", n),
    data_measure_text     = rep("Percent", n),
    date_measure_text     = rep("Monthly", n),
    data_transform_text   = rep("Seasonally Adjusted", n),
    geo_entity_type_text  = rep("Nation", n),
    geo_entity_text       = rep("US", n),
    viz_type_text         = rep("Line", n)
  )
}

test_that("basic mean over membership dates (inclusive) works", {
  # WHY: baseline correctness for membership filtering + numeric summary.
  df <- .valid_econ_df(5L, start = as.Date("2025-01-01"))
  dates <- as.Date(c("2025-01-01", "2025-01-03"))  # picks value 1 and 3

  out <- econanalyzr::econ_value_summary(
    df, dates = dates, val_col = value, .fun = mean
  )
  expect_type(out, "double")
  expect_equal(out, mean(c(1, 3)))
})

test_that("exclusive membership filter drops matching dates", {
  # WHY: ensure filter_type = 'exclusive' excludes the given dates.
  df <- .valid_econ_df(5L, start = as.Date("2025-01-01"))
  dates <- as.Date(c("2025-01-01", "2025-01-03"))  # drop 1 and 3

  out <- econanalyzr::econ_value_summary(
    df, dates = dates, filter_type = "exclusive", .fun = sum
  )
  # Remaining values are 2, NA, 5; na_rm = TRUE by default, so 2 + 5 = 7.
  expect_equal(out, 7)
})

test_that("closed date_range inclusive filter works", {
  # WHY: validate range logic uses a *closed* interval [from, to].
  df <- .valid_econ_df(5L, start = as.Date("2025-01-01"))
  # Covers 2025-01-02..2025-01-04 -> values 2, 3, NA
  r <- as.Date(c("2025-01-02", "2025-01-04"))

  out <- econanalyzr::econ_value_summary(
    df, date_range = r, .fun = mean
  )
  expect_equal(out, mean(c(2, 3), na.rm = TRUE))
})

test_that("conflicting filters (dates + date_range) throw classed error", {
  # WHY: the function forbids ambiguous use; only one filter mode is allowed.
  df <- .valid_econ_df()
  expect_error(
    econanalyzr::econ_value_summary(
      df,
      dates      = df$date[1:2],
      date_range = range(df$date),
      .fun       = mean
    ),
    class = "econ_value_summary_conflicting_filters"
  )
})

test_that("POSIXct dates are coerced to Date and used for filtering", {
  # WHY: support POSIXt inputs deterministically via dates_tz coercion.
  df <- .valid_econ_df(3L, start = as.Date("2025-01-01"))
  posix_dates <- as.POSIXct(df$date[1:2])

  out <- econanalyzr::econ_value_summary(
    df, dates = posix_dates, .fun = mean
  )
  expect_equal(out, mean(c(1, 2)))
})

test_that("bad dates type yields classed error", {
  # WHY: reject non-Date / non-POSIXt inputs for `dates`.
  df <- .valid_econ_df()
  expect_error(
    econanalyzr::econ_value_summary(
      df, dates = 1:3, .fun = mean
    ),
    class = "econ_value_summary_bad_dates_type"
  )
})

test_that("bad date_range type/length yields classed error", {
  # WHY: guardrails for `date_range`: must be length-2, Date/POSIXt-coercible.
  df <- .valid_econ_df()
  # length-1 -> error
  expect_error(
    econanalyzr::econ_value_summary(
      df, date_range = as.Date("2025-01-01"), .fun = mean
    ),
    class = "econ_value_summary_bad_range_type"
  )
  # wrong type (character) -> error
  expect_error(
    econanalyzr::econ_value_summary(
      df, date_range = c("2025-01-01", "2025-02-01"), .fun = mean
    ),
    class = "econ_value_summary_bad_range_type"
  )
})

test_that("non-numeric val_col is rejected with classed error", {
  # WHY: function is numeric-only by design—fail fast on character columns.
  df <- .valid_econ_df()
  expect_error(
    econanalyzr::econ_value_summary(
      df, dates = df$date, val_col = data_element_text, .fun = mean
    ),
    class = "econ_value_summary_non_numeric"
  )
})

test_that("val_col may be selected by numeric position", {
  # WHY: allow tidy-select by position as well as by name.
  df <- .valid_econ_df(3L)
  # In schema, 'value' is column #3; sum with NA removal by default.
  out <- econanalyzr::econ_value_summary(
    df, dates = df$date, val_col = 3L, .fun = sum
  )
  expect_equal(out, sum(df$value, na.rm = TRUE))
})

test_that("NA handling: na_rm=TRUE removes NA before function call", {
  # WHY: confirm NA removal happens *before* calling .fun, so .fun needn't support na.rm.
  df <- .valid_econ_df(5L)
  # values: 1, 2, 3, NA, 5
  out1 <- econanalyzr::econ_value_summary(df, dates = df$date, .fun = mean, na_rm = TRUE)
  out2 <- econanalyzr::econ_value_summary(df, dates = df$date, .fun = mean, na_rm = FALSE)
  expect_equal(out1, mean(c(1, 2, 3, NA, 5), na.rm = TRUE))
  # mean() with NA (and na.rm = FALSE) returns NA
  expect_true(is.na(out2))
})

test_that("empty filter warns (classed) and returns NA by default", {
  # WHY: empty subset should not crash; warn and return NA to signal 'no data'.
  df <- .valid_econ_df(3L, start = as.Date("2025-01-01"))
  missing_dates <- as.Date(c("1999-01-01", "1999-01-02"))

  res <- NULL
  expect_warning(
    {
      res <- econanalyzr::econ_value_summary(
        df, dates = missing_dates, .fun = mean
      )
    },
    class = "econ_value_summary_empty_filter"
  )
  expect_true(is.na(res))
})

test_that(".fun may return a vector (e.g., quantiles)", {
  # WHY: ensure return type is not forced to scalar—vector outputs are allowed.
  df <- .valid_econ_df(5L)
  probs <- c(.25, .5, .75)
  out <- econanalyzr::econ_value_summary(
    df, dates = df$date,
    .fun = function(x) stats::quantile(x, probs = probs)
  )
  expect_true(is.numeric(out))
  expect_length(out, length(probs))
  # sanity: quantiles should be non-decreasing
  expect_true(all(diff(out) >= 0))
})

test_that("errors thrown inside .fun are surfaced with classed error", {
  # WHY: protect callers with a classed error if their function fails internally.
  df <- .valid_econ_df(3L)
  boom <- function(x) stop("boom!")
  expect_error(
    econanalyzr::econ_value_summary(df, dates = df$date, .fun = boom),
    class = "econ_value_summary_fun_error"
  )
})

test_that("using the entire data (no dates/date_range) applies .fun to all rows", {
  # WHY: default behavior should apply summary to the full dataset when no filter is given.
  df <- .valid_econ_df(4L)
  # values: 1, 2, 3, NA  -> sum = 6 with NA removal
  out <- econanalyzr::econ_value_summary(df, .fun = sum)
  expect_equal(out, sum(c(1, 2, 3, NA), na.rm = TRUE))
})
