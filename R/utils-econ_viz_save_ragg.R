#' Save a ggplot via ragg raster devices
#'
#' Uses `ragg::agg_png()`, `ragg::agg_jpeg()`, or `ragg::agg_tiff()` for
#' high-quality raster output. Requires **ragg** in Suggests.
#' This helper checks availability with `rlang::is_installed()` and never attaches ragg.
#'
#' @param plt A ggplot object to print.
#' @param outpath File path (including extension) to write to.
#' @param ext Lowercased extension: one of "png", "jpeg"/"jpg", "tiff".
#' @param dims List from [econ_viz_dims()]: `width_px`, `height_px` (pixels).
#' @param bg Background color (e.g., "white", "transparent").
#' @param dpi Resolution (passed as `res`).
#'
#' @keywords internal
#' @rdname econanalyzr-viz-helpers
econ_viz_save_ragg <- function(plt, outpath, ext, dims, bg, dpi) {
  if (!rlang::is_installed("ragg")) {
    rlang::abort(
      "Engine 'ragg' requires the 'ragg' package. Install it or choose another engine.",
      class = "save_econanalyzr_chart_missing_backend"
    )
  }

  e <- tolower(ext)
  fun <- switch(e,
                "png"  = ragg::agg_png,
                "jpeg" = ragg::agg_jpeg,
                "jpg"  = ragg::agg_jpeg,
                "tiff" = ragg::agg_tiff,
                NULL
  )
  if (is.null(fun)) {
    rlang::abort(glue::glue("Unsupported format for ragg engine: '{ext}' (use png/jpeg/tiff)."),
                 class = "save_econanalyzr_chart_bad_format")
  }

  # ragg accepts units = "px" for exact raster sizing
  fun(
    filename   = outpath,
    width      = dims$width_px,
    height     = dims$height_px,
    units      = "px",
    res        = dpi,
    background = bg
  )
  on.exit(grDevices::dev.off(), add = TRUE)
  print(plt)
  invisible()
}
