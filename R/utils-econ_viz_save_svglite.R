#' Save a ggplot via svglite (vector SVG)
#'
#' Uses `svglite::svglite()` for clean SVG output. Requires **svglite** in Suggests.
#' Checks availability with `rlang::is_installed()` and never attaches svglite.
#'
#' @param plt A ggplot object to print.
#' @param outpath File path (including extension) to write to.
#' @param ext Lowercased extension; must be "svg".
#' @param dims List from [econ_viz_dims()]: `width_in`, `height_in` (inches).
#'
#' @keywords internal
#' @rdname econanalyzr-viz-helpers
econ_viz_save_svglite <- function(plt, outpath, ext, dims) {
  if (tolower(ext) != "svg") {
    rlang::abort("Engine 'svglite' only supports 'svg' format.",
                 class = "save_econanalyzr_chart_bad_format")
  }
  if (!rlang::is_installed("svglite")) {
    rlang::abort(
      "Engine 'svglite' requires the 'svglite' package. Install it or choose another engine.",
      class = "save_econanalyzr_chart_missing_backend"
    )
  }

  svglite::svglite(file = outpath, width = dims$width_in, height = dims$height_in)
  on.exit(grDevices::dev.off(), add = TRUE)
  print(plt)
  invisible()
}
