# tests/testthat/test-save_econanalyzr_chart.R
#
# PURPOSE
# -------
# Integration tests for the exported save_econanalyzr_chart():
#   - ggsave backend writes a raster with the chosen extension
#   - grDevices backend writes a raster with the chosen format
#   - overwrite protection works
#   - bad inputs produce classed errors
#   - engine backends enforce availability and reject conflicting args
#
# DESIGN NOTES
# ------------
# We construct a minimal econanalyzr-valid tibble, validated by check_econanalyzr_df().
# We don't assert the full filename stem (which includes data-derived tokens)
# to avoid brittle tests; we assert extension + existence + size > 0.
# WHY skip_on_cran()?
# -------------------
# Only the *file-writing* tests are skipped on CRAN. The validation/error
# tests still run everywhere.

.valid_econ_df <- function(n = 3L) {
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

.make_plot <- function(df) {
  ggplot2::ggplot(df, ggplot2::aes(x = date, y = value)) + ggplot2::geom_line()
}

test_that("save_econanalyzr_chart (ggsave) writes a PNG to disk", {
  testthat::skip_on_cran()
  df <- .valid_econ_df(5L)
  p  <- .make_plot(df)

  outdir <- file.path(tempdir(), paste0("save-ggsave-", as.integer(runif(1,1e6,9e6))))
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(outdir, recursive = TRUE, force = TRUE), add = TRUE)

  path <- NULL
  expect_message(
    { path <- econanalyzr::save_econanalyzr_chart(
      plt = p, folder = outdir, data = df,
      engine = "ggsave", device = "png",
      width = 800, height = 400, units = "px", dpi = 150,
      quiet = FALSE, overwrite = TRUE
    )
    },
    regexp = "Saving chart via ggsave"
  )

  expect_true(is.character(path) && length(path) == 1L)
  expect_true(file.exists(path))
  expect_match(basename(path), "\\.png$")
  expect_gt(file.info(path)$size, 0)
})


test_that("save_econanalyzr_chart (grDevices) writes a PNG to disk", {
  testthat::skip_on_cran()
  df <- .valid_econ_df(3L)
  p  <- .make_plot(df)

  outdir <- file.path(tempdir(), paste0("save-grdev-", as.integer(runif(1,1e6,9e6))))
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(outdir, recursive = TRUE, force = TRUE), add = TRUE)

  path <- econanalyzr::save_econanalyzr_chart(
    plt = p, folder = outdir, data = df,
    engine = "grDevices", format = "png",
    width = 600, height = 300, units = "px", dpi = 150,
    quiet = TRUE, overwrite = TRUE
  )
  expect_true(file.exists(path))
  expect_match(basename(path), "\\.png$")
  expect_gt(file.info(path)$size, 0)
})

test_that("overwrite protection prevents clobbering without overwrite=TRUE", {
  testthat::skip_on_cran()
  df <- .valid_econ_df(2L)
  p  <- .make_plot(df)

  outdir <- file.path(tempdir(), paste0("save-overwrite-", as.integer(runif(1,1e6,9e6))))
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(outdir, recursive = TRUE, force = TRUE), add = TRUE)

  # First write creates the file
  path1 <- econanalyzr::save_econanalyzr_chart(
    plt = p, folder = outdir, data = df,
    engine = "ggsave", device = "png",
    width = 400, height = 200, units = "px", dpi = 100,
    quiet = TRUE, overwrite = TRUE
  )
  expect_true(file.exists(path1))

  # Second write (same stem) without overwrite must error
  expect_error(
    econanalyzr::save_econanalyzr_chart(
      plt = p, folder = outdir, data = df,
      engine = "ggsave", device = "png",
      width = 400, height = 200, units = "px", dpi = 100,
      quiet = TRUE, overwrite = FALSE
    ),
    class = "save_econanalyzr_chart_exists"
  )
})

test_that("bad inputs surface classed errors", {
  df <- .valid_econ_df(1L)
  p  <- .make_plot(df)
  bad_plot <- list(a = 1)

  # plt must be a ggplot
  expect_error(
    econanalyzr::save_econanalyzr_chart(
      plt = bad_plot, folder = tempdir(), data = df
    ),
    class = "save_econanalyzr_chart_bad_plot"
  )

  # folder must be a valid string scalar (non-NA)
  expect_error(
    econanalyzr::save_econanalyzr_chart(
      plt = p, folder = NA_character_, data = df
    ),
    class = "save_econanalyzr_chart_bad_folder"
  )

  # Missing data: if plt$data is empty and data= not supplied, we cannot build a filename
  p_nodata <- ggplot2::ggplot() + ggplot2::geom_point()
  expect_error(
    econanalyzr::save_econanalyzr_chart(
      plt = p_nodata, folder = tempdir()
    ),
    class = "save_econanalyzr_chart_missing_data"
  )

  # ggsave path prohibits passing conflicting args in ...
  expect_error(
    econanalyzr::save_econanalyzr_chart(
      plt = p, folder = tempdir(), data = df,
      engine = "ggsave", device = "png", filename = "x.png"
    ),
    class = "save_econanalyzr_chart_conflicting_args"
  )
})

# ---------------------------
# ragg: MISSING -> must error
# ---------------------------
test_that("ragg missing -> classed error", {
  # Run this test ONLY if ragg is NOT installed
  testthat::skip_if(rlang::is_installed("ragg"), "ragg installed; this test covers the missing-backend branch.")

  df <- .valid_econ_df(1L)
  p  <- .make_plot(df)

  expect_error(
    econanalyzr::save_econanalyzr_chart(
      plt = p, folder = tempdir(), data = df,
      engine = "ragg", format = "png",
      width = 320, height = 200, units = "px", dpi = 96,
      quiet = TRUE, overwrite = TRUE
    ),
    class = "save_econanalyzr_chart_missing_backend"
  )
})

# --------------------------
# ragg: PRESENT -> write ok
# --------------------------
test_that("ragg present -> writes a PNG", {
  # Run this test ONLY if ragg IS installed
  testthat::skip_on_cran()
  testthat::skip_if(!rlang::is_installed("ragg"), "ragg not installed; skipping present-backend branch.")

  df <- .valid_econ_df(2L)
  p  <- .make_plot(df)

  outdir <- file.path(tempdir(), paste0("ragg-present-", as.integer(runif(1,1e6,9e6))))
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(outdir, recursive = TRUE, force = TRUE), add = TRUE)

  path <- econanalyzr::save_econanalyzr_chart(
    plt = p, folder = outdir, data = df,
    engine = "ragg", format = "png",
    width = 320, height = 200, units = "px", dpi = 96,
    quiet = TRUE, overwrite = TRUE
  )
  expect_true(file.exists(path))
  expect_match(basename(path), "\\.png$")
  expect_gt(file.info(path)$size, 0)
})

# ------------------------------
# svglite: MISSING -> must error
# ------------------------------
test_that("svglite missing -> classed error", {
  # Run this test ONLY if svglite is NOT installed
  testthat::skip_if(rlang::is_installed("svglite"), "svglite installed; this test covers the missing-backend branch.")

  df <- .valid_econ_df(1L)
  p  <- .make_plot(df)

  expect_error(
    econanalyzr::save_econanalyzr_chart(
      plt = p, folder = tempdir(), data = df,
      engine = "svglite", format = "svg",
      width = 320, height = 200, units = "px", dpi = 96,
      quiet = TRUE, overwrite = TRUE
    ),
    class = "save_econanalyzr_chart_missing_backend"
  )
})

# -----------------------------
# svglite: PRESENT -> write ok
# -----------------------------
test_that("svglite present -> writes an SVG", {
  # Run this test ONLY if svglite IS installed
  testthat::skip_on_cran()
  testthat::skip_if(!rlang::is_installed("svglite"), "svglite not installed; skipping present-backend branch.")

  df <- .valid_econ_df(2L)
  p  <- .make_plot(df)

  outdir <- file.path(tempdir(), paste0("svglite-present-", as.integer(runif(1,1e6,9e6))))
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(outdir, recursive = TRUE, force = TRUE), add = TRUE)

  path <- econanalyzr::save_econanalyzr_chart(
    plt = p, folder = outdir, data = df,
    engine = "svglite", format = "svg",
    width = 320, height = 200, units = "px", dpi = 96,
    quiet = TRUE, overwrite = TRUE
  )
  expect_true(file.exists(path))
  expect_match(basename(path), "\\.svg$")
  expect_gt(file.info(path)$size, 0)
})

