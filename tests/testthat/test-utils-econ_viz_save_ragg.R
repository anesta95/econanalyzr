# tests/testthat/test-utils-econ_viz_save_ragg.R
#
# PURPOSE
# -------
# Validate that econ_viz_save_ragg():
#   - gracefully skips if 'ragg' isn't installed (CRAN-friendliness)
#   - writes a PNG when installed
#   - errors with a classed message on unsupported format
#
# DESIGN NOTES
# ------------
# We use skip_if_not_installed("ragg") to avoid hard dependency in tests.
# WHY skip_on_cran()?
# -------------------
# The first test writes files with a device (ragg) that can vary across
# platforms; we skip that *on CRAN only* to keep checks fast/robust.
# The unsupported-format test doesn't write files and runs everywhere.

test_that("econ_viz_save_ragg saves png when ragg is installed", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("ragg")

  dir <- file.path(tempdir(), paste0("ragg-", as.integer(runif(1,1e6,9e6))))
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(dir, recursive = TRUE, force = TRUE), add = TRUE)

  p <- {
    df <- data.frame(x = 1:10, y = 1:10)
    ggplot2::ggplot(df, ggplot2::aes(x, y)) + ggplot2::geom_point()
  }
  dims <- econanalyzr:::econ_viz_dims(1000, 600, units = "px", dpi = 200)

  out <- file.path(dir, "ragg.png")
  econanalyzr:::econ_viz_save_ragg(p, out, "png", dims, bg = "white", dpi = 200)
  expect_true(file.exists(out))
  expect_gt(file.info(out)$size, 0)
})

test_that("econ_viz_save_ragg errors on unsupported format", {
  if (rlang::is_installed("ragg")) {
    p <- {
      df <- data.frame(x = 1:3, y = 1:3)
      ggplot2::ggplot(df, ggplot2::aes(x, y)) + ggplot2::geom_point()
    }
    dims <- econanalyzr:::econ_viz_dims(800, 400, "px", 200)
    expect_error(
      econanalyzr:::econ_viz_save_ragg(p, tempfile(fileext = ".pdf"), "pdf", dims, "white", 200),
      class = "save_econanalyzr_chart_bad_format"
    )
  } else {
    testthat::skip("ragg not installed; skipping unsupported-format test.")
  }
})

