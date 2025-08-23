# tests/testthat/test-econ_csv_write_out.R
#
# Tests for econ_csv_write_out():
# - Valid DF -> writes file, returns path, message appears by default
# - Filename stem from single date vs date range
# - Metadata tokenization from *_text columns (single value vs multi-value -> n_<aspect>)
# - All-NA metadata column -> warns with class and uses 'na' token
# - Overwrite protection (error when exists unless overwrite = TRUE)
# - Bad folder input error
# - Validator integration: bubbles classed errors from check_econanalyzr_df()
#

# Helper: minimal valid tibble (already correct order)
valid_df <- function(n = 1L) {
  tibble::tibble(
    date                  = as.Date("2024-01-01") + seq_len(n) - 1L,
    date_period_text      = rep("2024-01", n),
    value                 = as.double(seq_len(n)),
    data_element_text     = rep("Quits Rate", n),
    data_measure_text     = rep("Percent", n),
    date_measure_text     = rep("Monthly", n),
    data_transform_text   = rep("Seasonally Adjusted", n),
    geo_entity_type_text  = rep("Nation", n),
    geo_entity_text       = rep("US", n),
    viz_type_text         = rep("Line", n)
  )
}

# Temporary folder helper
tmp_outdir <- function() {
  file.path(tempdir(), paste0("econ_csv_write_out-", as.integer(runif(1, 1e6, 9e6))))
}

test_that("valid input writes CSV, returns path, and emits message by default", {
  dir <- tmp_outdir()
  on.exit(unlink(dir, recursive = TRUE, force = TRUE), add = TRUE)

  df <- valid_df(1L)
  # Expect an informative message; capture so tests are quiet
  expect_message(
    path <- econ_csv_write_out(df, folder = dir, overwrite = TRUE, quiet = FALSE),
    regexp = "Wrote CSV:"
  )
  expect_true(file.exists(path))

  # Filename should include date stem and metadata tokens (lowercased, underscores)
  fname <- basename(path)
  expect_true(grepl("^2024-01-01-", fname))
  # Tokens include all *_text columns in canonical order after validation
  expect_true(grepl("-2024-01-", fname))                    # date_period_text
  expect_true(grepl("-quits_rate-", fname))                 # data_element_text (space -> _; lowercased)
  expect_true(grepl("-percent-", fname))                    # data_measure_text
  expect_true(grepl("-monthly-", fname))                    # date_measure_text
  expect_true(grepl("-seasonally_adjusted-", fname))        # data_transform_text
  expect_true(grepl("-nation-", fname))                     # geo_entity_type_text
  expect_true(grepl("-us-", fname))                         # geo_entity_text
  expect_true(grepl("-line\\.csv$", fname))                 # viz_type_text + extension
})

test_that("date stem reflects single date vs date range", {
  dir <- tmp_outdir()
  on.exit(unlink(dir, recursive = TRUE, force = TRUE), add = TRUE)

  # single date
  df1 <- valid_df(1L)
  p1 <- econ_csv_write_out(df1, folder = dir, overwrite = TRUE, quiet = TRUE)
  expect_true(grepl("^2024-01-01-", basename(p1)))

  # date range (max first, then min)
  df2 <- valid_df(3L)
  df2$date <- as.Date(c("2024-01-01", "2024-01-03", "2024-01-02"))  # max=03, min=01
  p2 <- econ_csv_write_out(df2, folder = dir, overwrite = TRUE, quiet = TRUE)
  expect_true(grepl("^2024-01-03_2024-01-01-", basename(p2)))
})

test_that("multi-valued metadata columns produce n_<aspect> tokens", {
  dir <- tmp_outdir()
  on.exit(unlink(dir, recursive = TRUE, force = TRUE), add = TRUE)

  df <- valid_df(3L)
  # Make geo_entity_text have two unique values -> "2_geo_entity"
  df$geo_entity_text <- c("US", "US", "Canada")

  p <- econ_csv_write_out(df, folder = dir, overwrite = TRUE, quiet = TRUE)
  stem <- sub("\\.csv$", "", basename(p))
  expect_true(grepl("-2_geo_entity-", stem))
})

test_that("all-NA metadata columns warn and get 'na' token", {
  dir <- tmp_outdir()
  on.exit(unlink(dir, recursive = TRUE, force = TRUE), add = TRUE)

  df <- valid_df(2L)
  # Make one *_text column entirely NA
  df$data_transform_text <- NA_character_

  expect_warning(
    p <- econ_csv_write_out(df, folder = dir, overwrite = TRUE, quiet = TRUE),
    class = "econ_csv_meta_all_na"
  )
  stem <- sub("\\.csv$", "", basename(p))
  expect_true(grepl("-na-", stem))
})

test_that("overwrite protection: errors when file exists unless overwrite=TRUE", {
  dir <- tmp_outdir()
  on.exit(unlink(dir, recursive = TRUE, force = TRUE), add = TRUE)

  df <- valid_df(1L)

  # First write
  p1 <- econ_csv_write_out(df, folder = dir, overwrite = TRUE, quiet = TRUE)
  expect_true(file.exists(p1))

  # Second write with overwrite = FALSE -> error
  expect_error(
    econ_csv_write_out(df, folder = dir, overwrite = FALSE, quiet = TRUE),
    class = "econ_csv_write_out_exists"
  )

  # overwrite = TRUE -> success
  p2 <- econ_csv_write_out(df, folder = dir, overwrite = TRUE, quiet = TRUE)
  expect_true(file.exists(p2))
  expect_identical(p1, p2)  # same filename
})

test_that("bad folder input triggers classed error", {
  df <- valid_df()
  expect_error(econ_csv_write_out(df, folder = NA_character_), class = "econ_csv_write_out_bad_folder")
  expect_error(econ_csv_write_out(df, folder = ""),            class = "econ_csv_write_out_bad_folder")
  expect_error(econ_csv_write_out(df, folder = c("a", "b")),   class = "econ_csv_write_out_bad_folder")
})

test_that("validator integration: bubbles classed errors from check_econanalyzr_df()", {
  dir <- tmp_outdir()
  on.exit(unlink(dir, recursive = TRUE, force = TRUE), add = TRUE)

  df <- valid_df()
  # Remove viz_type_text but keep width >= 10 so we pass min-cols and hit the specific class
  df$viz_type_text <- NULL
  df$optional_col  <- "x"

  expect_error(
    econ_csv_write_out(df, folder = dir, overwrite = TRUE, quiet = TRUE),
    class = "econ_df_missing_viz"
  )
})

