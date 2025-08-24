# tests/testthat/test-utils-econ_viz_dims.R
#
# PURPOSE
# -------
# Validate that econ_viz_dims():
#   - converts sizes correctly across units ("px", "in", "cm", "mm")
#   - returns both inch and pixel dimensions for downstream devices
#   - produces classed errors for bad inputs (size, dpi, units)
#
# DESIGN NOTES
# ------------
# We keep calculations simple and exact where possible. For example, we test
# 2.54 cm == 1 inch exactly (by definition), so conversions should be exact.
# We also assert integer pixel sizes because raster devices expect integer px.

test_that("econ_viz_dims converts px to inches/pixels correctly", {
  d <- econanalyzr:::econ_viz_dims(width = 2000, height = 1000, units = "px", dpi = 250)

  # Types: inches should be double; pixels should be integer
  expect_type(d$width_in, "double")
  expect_type(d$width_px, "integer")

  # px -> inches via 1/dpi
  expect_equal(d$width_in, 2000 / 250)
  expect_equal(d$height_in, 1000 / 250)

  # px preserved as integers
  expect_equal(d$width_px, 2000L)
  expect_equal(d$height_px, 1000L)
})

test_that("econ_viz_dims converts inches and cm/mm correctly", {
  # Inches -> pixels via dpi
  d_in <- econanalyzr:::econ_viz_dims(width = 8, height = 4, units = "in", dpi = 300)
  expect_equal(d_in$width_px, 2400L)
  expect_equal(d_in$height_px, 1200L)
  expect_equal(d_in$width_in, 8)
  expect_equal(d_in$height_in, 4)

  # 2.54 cm = 1 in, 5.08 cm = 2 in
  d_cm <- econanalyzr:::econ_viz_dims(width = 2.54, height = 5.08, units = "cm", dpi = 254)
  expect_equal(d_cm$width_in, 1)
  expect_equal(d_cm$height_in, 2)
  expect_equal(d_cm$width_px, 254L)
  expect_equal(d_cm$height_px, 508L)

  # 25.4 mm = 1 in, 50.8 mm = 2 in
  d_mm <- econanalyzr:::econ_viz_dims(width = 25.4, height = 50.8, units = "mm", dpi = 254)
  expect_equal(d_mm$width_in, 1)
  expect_equal(d_mm$height_in, 2)
  expect_equal(d_mm$width_px, 254L)
  expect_equal(d_mm$height_px, 508L)
})

test_that("econ_viz_dims surfaces classed errors for invalid inputs", {
  # Negative size not allowed
  expect_error(
    econanalyzr:::econ_viz_dims(-1, 100, "px", 300),
    class = "save_econanalyzr_chart_bad_size"
  )
  # Non-positive dpi not allowed
  expect_error(
    econanalyzr:::econ_viz_dims(100, 100, "px", 0),
    class = "save_econanalyzr_chart_bad_dpi"
  )
  # Unknown units must error
  expect_error(
    econanalyzr:::econ_viz_dims(100, 100, "pt", 300),
    class = "save_econanalyzr_chart_bad_units"
  )
})
