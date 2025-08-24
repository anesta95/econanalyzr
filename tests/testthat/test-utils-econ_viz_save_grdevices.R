# tests/testthat/test-utils-econ_viz_save_grdevices.R
#
# PURPOSE
# -------
# Validate that econ_viz_save_grdevices():
#   - writes raster (PNG) and vector (PDF) files using base grDevices
#   - throws a classed error for unsupported formats
#
# DESIGN NOTES
# ------------
# We don't inspect pixels or PDF content; we just assert the file exists and is
# non-empty. We write to a temp dir and clean up afterwards.
# WHY skip_on_cran()?
# -------------------
# Image device behavior and file I/O can be slow or differ on CRAN's
# headless check machines. We only skip the *file-writing* test; the
# error-path test still runs on CRAN.

.make_plot <- function() {
  df <- data.frame(x = 1:3, y = c(1, 2, 1))
  ggplot2::ggplot(df, ggplot2::aes(x, y)) + ggplot2::geom_line()
}
.make_dims <- function() {
  econanalyzr:::econ_viz_dims(width = 800, height = 400, units = "px", dpi = 200)
}

test_that("econ_viz_save_grdevices writes PNG and PDF files", {
  testthat::skip_on_cran()  # only the file-writing test is skipped on CRAN
  dir <- file.path(tempdir(), paste0("grdevices-", as.integer(runif(1,1e6,9e6))))
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(dir, recursive = TRUE, force = TRUE), add = TRUE)

  p <- .make_plot()
  dims <- .make_dims()

  # PNG (raster path)
  p_png <- file.path(dir, "p.png")
  econanalyzr:::econ_viz_save_grdevices(p, p_png, "png", dims, bg = "white", dpi = 200)
  expect_true(file.exists(p_png))
  expect_gt(file.info(p_png)$size, 0)

  # PDF (vector path)
  p_pdf <- file.path(dir, "p.pdf")
  econanalyzr:::econ_viz_save_grdevices(p, p_pdf, "pdf", dims, bg = "white", dpi = 200)
  expect_true(file.exists(p_pdf))
  expect_gt(file.info(p_pdf)$size, 0)
})

test_that("econ_viz_save_grdevices errors on unsupported extension", {
  p <- .make_plot()
  dims <- .make_dims()
  expect_error(
    econanalyzr:::econ_viz_save_grdevices(p, tempfile(fileext = ".xyz"), "xyz", dims, "white", 96),
    class = "save_econanalyzr_chart_bad_format"
  )
})
