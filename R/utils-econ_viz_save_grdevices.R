#' Save a ggplot via base grDevices
#'
#' Opens a base graphics device (png/jpeg/tiff/bmp/pdf/svg), prints the ggplot,
#' and closes the device. Raster devices use pixels + `res`; vector devices use inches.
#'
#' @param plt A ggplot object to print.
#' @param outpath File path (including extension) to write to.
#' @param ext Lowercased extension (e.g., "png","pdf","svg","jpeg","tiff","bmp").
#' @param dims List from [econ_viz_dims()]: `width_in`, `height_in`, `width_px`, `height_px`.
#' @param bg Background color for raster devices (e.g., "white", "transparent").
#' @param dpi Resolution (passed as `res` for raster devices).
#'
#' @keywords internal
#' @rdname econanalyzr-viz-helpers
econ_viz_save_grdevices <- function(plt, outpath, ext, dims, bg, dpi) {
  e <- tolower(ext)

  # Raster devices: PNG/JPEG/TIFF/BMP (pixel sizes + resolution)
  if (e %in% c("png", "jpeg", "jpg", "tiff", "bmp")) {
    fun <- switch(e,
                  "png"  = grDevices::png,
                  "jpeg" = grDevices::jpeg,
                  "jpg"  = grDevices::jpeg,
                  "tiff" = grDevices::tiff,
                  "bmp"  = grDevices::bmp
    )
    fun(filename = outpath, width = dims$width_px, height = dims$height_px, res = dpi, bg = bg)
    on.exit(grDevices::dev.off(), add = TRUE)
    print(plt)
    return(invisible())
  }

  # Vector devices: PDF/SVG (inch sizes)
  if (e == "pdf") {
    grDevices::pdf(file = outpath, width = dims$width_in, height = dims$height_in, onefile = FALSE)
    on.exit(grDevices::dev.off(), add = TRUE)
    print(plt)
    return(invisible())
  }

  if (e == "svg") {
    grDevices::svg(filename = outpath, width = dims$width_in, height = dims$height_in)
    on.exit(grDevices::dev.off(), add = TRUE)
    print(plt)
    return(invisible())
  }

  rlang::abort(glue::glue("Unsupported format for grDevices engine: '{ext}'."),
               class = "save_econanalyzr_chart_bad_format")
}
