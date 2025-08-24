# tests/testthat/test-utils-econ_viz_save_svglite.R
#
# PURPOSE
# -------
# Validate that econ_viz_save_svglite():
#   - gracefully skips if 'svglite' isn't installed
#   - writes an SVG when installed
#   - errors with a classed message if extension != 'svg'
#
# DESIGN NOTES
# ------------
# This keeps the vector path isolated and simple.
# WHY skip_on_cran()?
# -------------------
# The file-writing test is skipped on CRAN (device/I-O heavy).
# The error-path test is small/fast and runs everywhere.

test_that("econ_viz_save_svglite saves svg when svglite is installed", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("svglite")

  dir <- file.path(tempdir(), paste0("svglite-", as.integer(runif(1,1e6,9e6))))
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(dir, recursive = TRUE, force = TRUE), add = TRUE)

  p <- {
    df <- data.frame(x = 1:5, y = c(2,1,3,2,4))
    ggplot2::ggplot(df, ggplot2::aes(x, y)) + ggplot2::geom_line()
  }
  dims <- econanalyzr:::econ_viz_dims(1000, 600, "px", 200)

  out <- file.path(dir, "plot.svg")
  econanalyzr:::econ_viz_save_svglite(p, out, "svg", dims)
  expect_true(file.exists(out))
  expect_gt(file.info(out)$size, 0)
})

test_that("econ_viz_save_svglite errors if ext is not svg", {
  if (rlang::is_installed("svglite")) {
    p <- {
      df <- data.frame(x = 1:3, y = 1:3)
      ggplot2::ggplot(df, ggplot2::aes(x, y)) + ggplot2::geom_point()
    }
    dims <- econanalyzr:::econ_viz_dims(800, 400, "px", 200)
    expect_error(
      econanalyzr:::econ_viz_save_svglite(p, tempfile(fileext = ".png"), "png", dims),
      class = "save_econanalyzr_chart_bad_format"
    )
  } else {
    testthat::skip("svglite not installed; skipping ext!=svg error test.")
  }
})

