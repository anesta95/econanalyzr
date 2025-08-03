test_that("valid econanalyzr tibble passes validation", {
  valid_df <- tibble::tibble(
    date = as.Date("2024-01-01"),
    date_period_text = "January 2024",
    value = 100.5,
    data_element_text = "GDP",
    data_measure_text = "Nominal",
    date_measure_text = "Month",
    data_transform_text = "Level",
    geo_entity_type_text = "Country",
    geo_entity_text = "USA",
    optional_col_1 = "extra",
    optional_col_2 = 999,
    viz_type_text = "bar"
  )

  expect_identical(check_econanalyzr_df(valid_df), valid_df)
})

test_that("non-data.frame or non-tibble input fails", {
  expect_error(
    check_econanalyzr_df("not a data frame"),
    class = "econanalyzr_validation_error"
  )

  expect_error(
    check_econanalyzr_df(list(a = 1, b = 2)),
    class = "econanalyzr_validation_error"
  )

  expect_error(
    check_econanalyzr_df(123),
    class = "econanalyzr_validation_error"
  )

  expect_error(
    check_econanalyzr_df(TRUE),
    class = "econanalyzr_validation_error"
  )
})


test_that("missing required column fails", {
  invalid_df <- tibble::tibble(
    date = as.Date("2024-01-01"),
    date_period_text = "January 2024",
    value = 100.5,
    data_element_text = "GDP",
    data_measure_text = "Nominal",
    date_measure_text = "Month",
    data_transform_text = "Level",
    geo_entity_type_text = "Country"
    # Missing geo_entity_text
  )

  expect_error(
    check_econanalyzr_df(invalid_df),
    class = "econanalyzr_validation_error"
  )
})

test_that("wrong column names or order in first 9 fails", {
  reordered_df <- tibble::tibble(
    date = as.Date("2024-01-01"),
    value = 100.5,  # <- wrong order
    date_period_text = "January 2024",
    data_element_text = "GDP",
    data_measure_text = "Nominal",
    date_measure_text = "Month",
    data_transform_text = "Level",
    geo_entity_type_text = "Country",
    geo_entity_text = "USA",
    viz_type_text = "bar"
  )

  expect_error(
    check_econanalyzr_df(reordered_df),
    class = "econanalyzr_validation_error"
  )
})

test_that("wrong type/class in required column fails", {
  bad_type_df <- tibble::tibble(
    date = "2024-01-01",  # should be Date
    date_period_text = "January 2024",
    value = 100.5,
    data_element_text = "GDP",
    data_measure_text = "Nominal",
    date_measure_text = "Month",
    data_transform_text = "Level",
    geo_entity_type_text = "Country",
    geo_entity_text = "USA",
    viz_type_text = "bar"
  )

  expect_error(
    check_econanalyzr_df(bad_type_df),
    class = "econanalyzr_validation_error"
  )
})

test_that("missing final column 'viz_type_text' fails", {
  df_missing_final <- tibble::tibble(
    date = as.Date("2024-01-01"),
    date_period_text = "January 2024",
    value = 100.5,
    data_element_text = "GDP",
    data_measure_text = "Nominal",
    date_measure_text = "Month",
    data_transform_text = "Level",
    geo_entity_type_text = "Country",
    geo_entity_text = "USA"
    # no viz_type_text
  )

  expect_error(
    check_econanalyzr_df(df_missing_final),
    class = "econanalyzr_validation_error"
  )
})

test_that("wrong type/class for final column 'viz_type_text' fails", {
  df_wrong_final <- tibble::tibble(
    date = as.Date("2024-01-01"),
    date_period_text = "January 2024",
    value = 100.5,
    data_element_text = "GDP",
    data_measure_text = "Nominal",
    date_measure_text = "Month",
    data_transform_text = "Level",
    geo_entity_type_text = "Country",
    geo_entity_text = "USA",
    viz_type_text = factor("bar")  # wrong class
  )

  expect_error(
    check_econanalyzr_df(df_wrong_final),
    class = "econanalyzr_validation_error"
  )
})
