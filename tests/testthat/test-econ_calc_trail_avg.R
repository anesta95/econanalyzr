# Tests for econ_calc_trail_avg()
# - We verify correctness in ungrouped and grouped settings.
# - We assert window alignment & NA behavior.
# - We ensure original rows are unchanged and trailing rows have the suffix appended.
# - We test argument validation and schema validation paths.
# - We avoid library() calls; use namespace-qualified calls.

# Helper to build a tiny valid econanalyzr tibble quickly
.valid_econ_df <- function(n = 5L, start = as.Date("2024-01-01")) {
  tibble::tibble(
    date                 = start + 0:(n - 1L),
    date_period_text     = rep("Daily", n),
    value                = as.double(seq_len(n) * 10),  # double vector: 10,20,30,...
    data_element_text    = "Quits Rate",
    data_measure_text    = "Percent",
    date_measure_text    = "Daily",
    data_transform_text  = "NSA",
    geo_entity_type_text = "Nation",
    geo_entity_text      = "US",
    viz_type_text        = "Line"
  )
}

testthat::test_that("ungrouped: appends trailing rows with right-aligned, .complete = TRUE", {
  # Build a simple 5-point series: values 10,20,30,40,50
  df <- .valid_econ_df(5L)
  # 3-period trailing mean (right-aligned):
  # positions 1-2 => NA, pos3 => mean(10,20,30)=20, pos4=>30, pos5=>40
  out <- econanalyzr::econ_calc_trail_avg(df, trail_amount = 3L)

  # 1) Same columns as input
  testthat::expect_identical(names(out), names(df))

  # 2) Row count doubled (original + trailing)
  testthat::expect_equal(nrow(out), 2L * nrow(df))

  # 3) Original rows are unchanged (at the top of the bound result)
  testthat::expect_identical(out$value[seq_len(nrow(df))], df$value)
  testthat::expect_identical(out$data_transform_text[seq_len(nrow(df))], df$data_transform_text)

  # 4) Trailing rows have expected trailing means and appended transform text
  trailing_vals <- out$value[(nrow(df) + 1L):(2L * nrow(df))]
  testthat::expect_true(is.na(trailing_vals[1L]) && is.na(trailing_vals[2L]))
  testthat::expect_equal(trailing_vals[3:5], c(20, 30, 40))

  trailing_tx <- out$data_transform_text[(nrow(df) + 1L):(2L * nrow(df))]
  testthat::expect_true(all(grepl("; Trail 3$", trailing_tx)))
})

testthat::test_that("grouped: trailing average is computed within each group", {
  # Two interleaved series by geo_entity_text: 'US' and 'CA'
  df <- tibble::tibble(
    date                 = rep(as.Date("2024-01-01") + 0:4, each = 2),
    date_period_text     = rep("Daily", 10),
    value                = as.double(c(1,10,  2,20,  3,30,  4,40,  5,50)),
    data_element_text    = "Quits Rate",
    data_measure_text    = "Percent",
    date_measure_text    = "Daily",
    data_transform_text  = "NSA",
    geo_entity_type_text = "Nation",
    geo_entity_text      = rep(c("US","CA"), times = 5),
    viz_type_text        = "Line"
  )

  # Group by country and compute 3-period trailing average per series
  out <- df |>
    dplyr::group_by(.data$geo_entity_text) |>
    econanalyzr::econ_calc_trail_avg(3L)

  # Extract trailing portion (second half of rows)
  n <- nrow(df)
  tr <- dplyr::slice(out, (n + 1L):(2L * n))

  # Split by group and verify each series independently
  tr_us <- tr[tr$geo_entity_text == "US", , drop = FALSE]
  tr_ca <- tr[tr$geo_entity_text == "CA", , drop = FALSE]

  # US values are 1,2,3,4,5 -> trailing(3): NA,NA,2,3,4
  testthat::expect_equal(tr_us$value, c(NA_real_, NA_real_, 2, 3, 4))

  # CA values are 10,20,30,40,50 -> trailing(3): NA,NA,20,30,40
  testthat::expect_equal(tr_ca$value, c(NA_real_, NA_real_, 20, 30, 40))

  # Suffix applied
  testthat::expect_true(all(grepl("; Trail 3$", tr_us$data_transform_text)))
  testthat::expect_true(all(grepl("; Trail 3$", tr_ca$data_transform_text)))
})

testthat::test_that("NA in any window yields NA; incomplete windows are NA", {
  # Values: 10, NA, 30, 40; trailing(2)
  df <- tibble::tibble(
    date                 = as.Date("2024-01-01") + 0:3,
    date_period_text     = rep("Daily", 4),
    value                = c(10, NA, 30, 40),    # double with NA
    data_element_text    = "Quits Rate",
    data_measure_text    = "Percent",
    date_measure_text    = "Daily",
    data_transform_text  = "NSA",
    geo_entity_type_text = "Nation",
    geo_entity_text      = "US",
    viz_type_text        = "Line"
  )

  out <- econanalyzr::econ_calc_trail_avg(df, 2L)
  n <- nrow(df)
  trailing_vals <- out$value[(n + 1L):(2L * n)]

  # right-aligned, .complete = TRUE:
  # pos1 NA (incomplete), pos2 mean(10,NA)=NA, pos3 mean(NA,30)=NA, pos4 mean(30,40)=35
  testthat::expect_equal(trailing_vals, c(NA_real_, NA_real_, NA_real_, 35))
})

testthat::test_that("returns an ungrouped tibble even if input was grouped", {
  df <- .valid_econ_df(4L) |>
    dplyr::group_by(.data$data_element_text)

  out <- econanalyzr::econ_calc_trail_avg(df, 2L)
  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_false(inherits(out, "grouped_df"))
})

testthat::test_that("bad trail_amount triggers classed error", {
  df <- .valid_econ_df(3L)
  testthat::expect_error(econanalyzr::econ_calc_trail_avg(df, 0),   class = "econ_calc_trail_avg_bad_trail_amount")
  testthat::expect_error(econanalyzr::econ_calc_trail_avg(df, -1),  class = "econ_calc_trail_avg_bad_trail_amount")
  testthat::expect_error(econanalyzr::econ_calc_trail_avg(df, 2.5), class = "econ_calc_trail_avg_bad_trail_amount")
  testthat::expect_error(econanalyzr::econ_calc_trail_avg(df, NA),  class = "econ_calc_trail_avg_bad_trail_amount")
})

testthat::test_that("invalid econ schema surfaces a validation error", {
  # Missing required columns (only date & value)
  bad <- tibble::tibble(date = as.Date("2024-01-01"), value = 1)
  testthat::expect_error(
    econanalyzr::econ_calc_trail_avg(bad, 3L),
    class = "econanalyzr_validation_error"
  )
})

testthat::test_that("original rows unchanged; trailing rows appended with suffix only", {
  df  <- .valid_econ_df(3L)
  out <- econanalyzr::econ_calc_trail_avg(df, 2L)

  n <- nrow(df)
  # Originals: exact match & no suffix added
  testthat::expect_identical(out$data_transform_text[seq_len(n)], df$data_transform_text)
  # Trailing rows: suffix added
  testthat::expect_true(all(grepl("; Trail 2$", out$data_transform_text[(n + 1L):(2L * n)])))
})
