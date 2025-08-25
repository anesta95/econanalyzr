# Tests for econ_filter_dates()
# - We validate both "happy path" behaviors and robust error handling.
# - We never call library(); we use namespace-qualified calls in tests.
# - We build small, valid econanalyzr data frames inline so tests are hermetic.

test_that("explicit closed interval is honored and results are newest-first", {
  # Minimal valid econ df with 5 daily dates (and an NA to ensure NA is ignored)
  make_df <- function() {
    tibble::tibble(
      date                   = as.Date("2024-01-01") + c(0, 1, 2, 3, NA_integer_),
      date_period_text       = rep("Daily", 5),
      value                  = c(10, 20, 30, 40, 50),
      data_element_text      = "Quits Rate",
      data_measure_text      = "Percent",
      date_measure_text      = "Daily",
      data_transform_text    = "NSA",
      geo_entity_type_text   = "Nation",
      geo_entity_text        = "US",
      viz_type_text          = "Line"
    )
  }

  df <- make_df()
  # Filter explicitly from 2024-01-02 to 2024-01-03 (inclusive)
  out <- econanalyzr::econ_filter_dates(
    df,
    start_date = as.Date("2024-01-02"),
    end_date   = as.Date("2024-01-03"),
    quiet      = TRUE
  )

  # Expect only those two dates, ordered newest-first (descending)
  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_identical(out$date, as.Date(c("2024-01-03", "2024-01-02")))
  testthat::expect_identical(out$value, c(30, 20))
})

test_that("end_date defaults to latest non-NA date; days period derives start_date", {
  # Latest non-NA date is 2024-01-05 (one NA included)
  df <- tibble::tibble(
    date                   = as.Date("2024-01-01") + c(0, 1, 2, 4, NA_integer_),
    date_period_text       = rep("Daily", 5),
    value                  = c(10, 20, 30, 50, 999),
    data_element_text      = "Quits Rate",
    data_measure_text      = "Percent",
    date_measure_text      = "Daily",
    data_transform_text    = "NSA",
    geo_entity_type_text   = "Nation",
    geo_entity_text        = "US",
    viz_type_text          = "Line"
  )

  # Ask for last 3 days ending at the table's latest date (2024-01-05)
  out <- econanalyzr::econ_filter_dates(
    df,
    period_type   = "days",
    period_amount = 3L,
    quiet         = TRUE
  )
  # Derived start_date should be 2024-01-02 (inclusive), 2024-01-02..2024-01-05
  testthat::expect_identical(out$date, as.Date(c("2024-01-05", "2024-01-03", "2024-01-02")))
})

test_that("weeks/months/quarters/years derive the correct calendar-aware start_date", {
  # Use mid-month dates to avoid month-end ambiguities
  df <- tibble::tibble(
    date                 = as.Date(c("2024-01-15", "2024-02-15", "2024-03-15", "2024-04-15")),
    date_period_text     = rep("Monthly", 4),
    value                = as.double(1:4),
    data_element_text    = "Quits Rate",
    data_measure_text    = "Percent",
    date_measure_text    = "Monthly",
    data_transform_text  = "SA",
    geo_entity_type_text = "Nation",
    geo_entity_text      = "US",
    viz_type_text        = "Line"
  )

  # Latest date defaults to 2024-04-15
  # Last 1 week -> start_date = 2024-04-08 (but our data has only 2024-04-15)
  out_w <- econanalyzr::econ_filter_dates(df, period_type = "weeks", period_amount = 1L, quiet = TRUE)
  testthat::expect_identical(out_w$date, as.Date("2024-04-15"))

  # Last 2 months -> start_date = 2024-02-15; expect Feb, Mar, Apr
  out_m <- econanalyzr::econ_filter_dates(df, period_type = "months", period_amount = 2L, quiet = TRUE)
  testthat::expect_identical(out_m$date, as.Date(c("2024-04-15", "2024-03-15", "2024-02-15")))

  # Last 1 quarter -> start_date = 2024-01-15; expect Jan..Apr (all rows)
  out_q <- econanalyzr::econ_filter_dates(df, period_type = "quarters", period_amount = 1L, quiet = TRUE)
  testthat::expect_identical(out_q$date, as.Date(c("2024-04-15", "2024-03-15", "2024-02-15", "2024-01-15")))

  # Last 1 year -> start_date = 2023-04-15; expect all rows
  out_y <- econanalyzr::econ_filter_dates(df, period_type = "years", period_amount = 1L, quiet = TRUE)
  testthat::expect_identical(out_y$date, as.Date(c("2024-04-15", "2024-03-15", "2024-02-15", "2024-01-15")))
})

test_that("POSIXt inputs for start/end are coerced to Date using datetime_tz", {
  df <- tibble::tibble(
    date                 = as.Date("2024-01-01") + 0:6,
    date_period_text     = rep("Daily", 7),
    value                = as.double(1:7),
    data_element_text    = "Quits Rate",
    data_measure_text    = "Percent",
    date_measure_text    = "Daily",
    data_transform_text  = "NSA",
    geo_entity_type_text = "Nation",
    geo_entity_text      = "US",
    viz_type_text        = "Line"
  )

  start_pt <- as.POSIXct("2024-01-03 23:59:59", tz = "America/New_York")
  end_pt   <- as.POSIXct("2024-01-05 00:00:00", tz = "America/New_York")

  out <- econanalyzr::econ_filter_dates(
    df,
    start_date  = start_pt,
    end_date    = end_pt,
    datetime_tz = "America/New_York",  # match the inputs’ TZ
    quiet       = TRUE
  )
  testthat::expect_identical(out$date, as.Date(c("2024-01-05", "2024-01-04", "2024-01-03")))

})

test_that("informational messages are emitted unless quiet = TRUE", {
  df <- tibble::tibble(
    date                 = as.Date("2024-01-01") + 0:2,
    date_period_text     = rep("Daily", 3),
    value                = c(1, 2, 3),
    data_element_text    = "Quits Rate",
    data_measure_text    = "Percent",
    date_measure_text    = "Daily",
    data_transform_text  = "NSA",
    geo_entity_type_text = "Nation",
    geo_entity_text      = "US",
    viz_type_text        = "Line"
  )

  # We expect an informational message mentioning the interval
  testthat::expect_message(
    econanalyzr::econ_filter_dates(
      df,
      start_date = as.Date("2024-01-01"),
      end_date   = as.Date("2024-01-03"),
      quiet      = FALSE
    ),
    regexp = "Filtered to \\[2024-01-01, 2024-01-03\\]"
  )

  # And no message when quiet = TRUE
  testthat::expect_silent(
    econanalyzr::econ_filter_dates(
      df,
      start_date = as.Date("2024-01-01"),
      end_date   = as.Date("2024-01-03"),
      quiet      = TRUE
    )
  )
})

test_that("empty-date table yields 0 rows and a message when end_date is inferred", {
  # All dates are NA → function returns 0 rows and emits an info message
  df <- tibble::tibble(
    date                 = rep(as.Date(NA), 3),   # <-- clean NA Date values
    date_period_text     = rep("Daily", 3),
    value                = c(1, 2, 3),            # make this double as well
    data_element_text    = "Quits Rate",
    data_measure_text    = "Percent",
    date_measure_text    = "Daily",
    data_transform_text  = "NSA",
    geo_entity_type_text = "Nation",
    geo_entity_text      = "US",
    viz_type_text        = "Line"
  )
  testthat::expect_message(
    out <- econanalyzr::econ_filter_dates(df, period_type = "days", period_amount = 7L),
    regexp = "No non-NA dates found; returning 0 rows\\."
  )
  testthat::expect_equal(nrow(out), 0L)

})

# --------------------- Error handling ------------------------------------------

test_that("df must be econanalyzr-valid (delegates to check_econanalyzr_df)", {
  # Supply an invalid df: missing many required columns
  bad <- tibble::tibble(date = as.Date("2024-01-01"), value = 1)
  testthat::expect_error(
    econanalyzr::econ_filter_dates(bad, start_date = as.Date("2024-01-01")),
    class = "econanalyzr_validation_error"
  )
})

test_that("start_date must be Date/POSIXt; character errors with class", {
  df <- tibble::tibble(
    date                 = as.Date("2024-01-01") + 0:2,
    date_period_text     = rep("Daily", 3),
    value                = as.double(1:3),
    data_element_text    = "Quits Rate",
    data_measure_text    = "Percent",
    date_measure_text    = "Daily",
    data_transform_text  = "NSA",
    geo_entity_type_text = "Nation",
    geo_entity_text      = "US",
    viz_type_text        = "Line"
  )
  testthat::expect_error(
    econanalyzr::econ_filter_dates(df, start_date = "2024-01-01"),
    class = "econ_filter_dates_bad_date_type"
  )
})

test_that("missing start_date requires period_type + positive integer-ish period_amount", {
  df <- tibble::tibble(
    date                 = as.Date("2024-01-01") + 0:10,
    date_period_text     = rep("Daily", 11),
    value                = as.double(1:11),
    data_element_text    = "Quits Rate",
    data_measure_text    = "Percent",
    date_measure_text    = "Daily",
    data_transform_text  = "NSA",
    geo_entity_type_text = "Nation",
    geo_entity_text      = "US",
    viz_type_text        = "Line"
  )

  # No start_date and no period_amount → error
  testthat::expect_error(
    econanalyzr::econ_filter_dates(df, period_type = "days"),
    class = "econ_filter_dates_missing_start"
  )

  # Non-integer-ish / non-positive period_amount → error
  testthat::expect_error(
    econanalyzr::econ_filter_dates(df, period_type = "days", period_amount = 0),
    class = "econ_filter_dates_bad_period_amount"
  )
  testthat::expect_error(
    econanalyzr::econ_filter_dates(df, period_type = "days", period_amount = 2.5),
    class = "econ_filter_dates_bad_period_amount"
  )
  testthat::expect_error(
    econanalyzr::econ_filter_dates(df, period_type = "days", period_amount = -3),
    class = "econ_filter_dates_bad_period_amount"
  )
})

test_that("conflicting args: start_date provided but period_amount also supplied", {
  df <- tibble::tibble(
    date                 = as.Date("2024-01-01") + 0:3,
    date_period_text     = rep("Daily", 4),
    value                = as.double(1:4),
    data_element_text    = "Quits Rate",
    data_measure_text    = "Percent",
    date_measure_text    = "Daily",
    data_transform_text  = "NSA",
    geo_entity_type_text = "Nation",
    geo_entity_text      = "US",
    viz_type_text        = "Line"
  )

  testthat::expect_error(
    econanalyzr::econ_filter_dates(
      df,
      start_date    = as.Date("2024-01-01"),
      period_type   = "days",
      period_amount = 7L
    ),
    class = "econ_filter_dates_conflicting_args"
  )
})

test_that("start_date after end_date raises classed error", {
  df <- tibble::tibble(
    date                 = as.Date("2024-01-01") + 0:3,
    date_period_text     = rep("Daily", 4),
    value                = as.double(1:4),
    data_element_text    = "Quits Rate",
    data_measure_text    = "Percent",
    date_measure_text    = "Daily",
    data_transform_text  = "NSA",
    geo_entity_type_text = "Nation",
    geo_entity_text      = "US",
    viz_type_text        = "Line"
  )

  testthat::expect_error(
    econanalyzr::econ_filter_dates(
      df, start_date = as.Date("2024-02-01"), end_date = as.Date("2024-01-15")
    ),
    class = "econ_filter_dates_start_after_end"
  )
})
