#' Compute pixel and inch dimensions for visualization devices
#'
#' Converts user-supplied `width`, `height`, `units`, and `dpi` into:
#' - integer pixel sizes (for raster devices, e.g., PNG/JPEG/TIFF), and
#' - floating-point inch sizes (for vector devices, e.g., PDF/SVG).
#'
#' This is used by the various econanalyzr visualization savers to normalize sizing
#' regardless of which graphics backend is chosen.
#'
#' @param width,height Numeric scalars: requested size.
#' @param units Character scalar: one of `"px"`, `"in"`, `"cm"`, `"mm"`.
#' @param dpi Numeric scalar dots-per-inch (used to convert between px and inches).
#'
#' @return A list with:
#'   - `width_in`, `height_in` (numeric, inches)
#'   - `width_px`, `height_px` (integer, pixels)
#'
#' @keywords internal
#' @rdname econanalyzr-viz-helpers
econ_viz_dims <- function(width, height, units, dpi) {
  # Light validation (exported caller validates earlier)
  if (!is.numeric(width)  || !is.numeric(height) || any(width <= 0 | height <= 0)) {
    rlang::abort("`width` and `height` must be positive numerics.",
                 class = "save_econanalyzr_chart_bad_size")
  }
  if (!is.numeric(dpi) || dpi <= 0) {
    rlang::abort("`dpi` must be a positive numeric scalar.",
                 class = "save_econanalyzr_chart_bad_dpi")
  }

  # Convert requested size to inches (used by vector devices)
  to_in <- switch(units,
                  "px" = 1 / dpi,
                  "in" = 1,
                  "cm" = 1 / 2.54,
                  "mm" = 1 / 25.4,
                  rlang::abort("`units` must be one of 'px', 'in', 'cm', or 'mm'.",
                               class = "save_econanalyzr_chart_bad_units")
  )
  width_in  <- width  * to_in
  height_in <- height * to_in

  # Convert requested size to pixels (used by raster devices)
  to_px <- switch(units,
                  "px" = 1,
                  "in" = dpi,
                  "cm" = dpi / 2.54,
                  "mm" = dpi / 25.4
  )
  width_px  <- as.integer(round(width  * to_px))
  height_px <- as.integer(round(height * to_px))

  list(
    width_in  = width_in,
    height_in = height_in,
    width_px  = width_px,
    height_px = height_px
  )
}
