#' Save a ggplot chart with an econanalyzr-style filename
#'
#' Builds a descriptive filename from the plot's data (or a supplied `data` frame):
#' `<date_or_range>-<meta1>-<meta2>-...-<metaK>.<ext>`.
#'
#' Filename construction uses:
#' - [make_file_name_date()] for the date stem (single `YYYY-MM-DD` or `max_min` range),
#' - [make_metadata_vec()] for tokens derived from all `*_text` columns in order,
#' - [sanitize_filename()] to make a safe, lowercase slug.
#'
#' Saving backends:
#' - `engine = "ggsave"` (default): calls [ggplot2::ggsave()], honoring `device`, `width`, `height`,
#'   `units`, `dpi`, `scale`, `bg`, and any extra `...` it supports.
#' - `engine = "grDevices"`: opens a graphics device from base R (e.g., `png()`, `pdf()`, `svg()`,
#'   `jpeg()`, `tiff()`, `bmp()`), prints the plot, and `dev.off()`.
#' - `engine = "ragg"`: uses `ragg::agg_png()` / `agg_jpeg()` / `agg_tiff()` if **ragg** is installed.
#' - `engine = "svglite"`: uses `svglite::svglite()` for SVG if **svglite** is installed.
#'
#' @param plt A `ggplot` object.
#' @param folder Output directory path. Created if it doesn't exist.
#' @param data Optional data frame for filename construction.
#'   If `NULL`, uses `plt$data`. Must satisfy [check_econanalyzr_df()].
#' @param engine Saving backend: one of `"ggsave"` (default), `"grDevices"`, `"ragg"`, `"svglite"`.
#' @param device For `engine = "ggsave"`, a device passed to [ggplot2::ggsave()]
#'   (e.g., `"png"`, `"pdf"`, `"svg"`, `"jpeg"`, `"tiff"`, or a device function).
#'   Ignored for other engines.
#' @param format Output format/extension for non-ggsave engines.
#'   One of `"png"`, `"pdf"`, `"svg"`, `"jpeg"`, `"jpg"`, `"tiff"`, `"bmp"`. Default `"png"`.
#' @param width,height Plot dimensions. Defaults target a crisp social-card:
#'   `width = 3600`, `height = 2010`.
#' @param units Units for `width`/`height`: `"px"` (default), `"in"`, `"cm"`, `"mm"`.
#' @param dpi Resolution in dots per inch (used when needed; ignored for vector formats).
#' @param scale A scaling factor (multiplies `width`/`height`) passed to `ggsave()`.
#' @param bg Background passed to `ggsave()` or used for raster devices (e.g., `"white"`, `"transparent"`).
#' @param overwrite If `FALSE` (default) and the target file exists, abort with a classed error.
#' @param quiet If `FALSE` (default) emit an informational message on success.
#' @param filename_stem Optional override for the filename stem (without extension). Will be sanitized.
#'   If `NULL`, the stem is built from `data`.
#' @param ... Additional arguments forwarded to [ggplot2::ggsave()] when `engine = "ggsave"`.
#'
#' @return (Invisibly) the full file path written.
#'
#' @export
#' @importFrom rlang abort inform is_string warn
#' @importFrom glue glue
#' @importFrom ggplot2 ggsave
save_econanalyzr_chart <- function(
    plt,
    folder,
    data = NULL,
    engine = c("ggsave", "grDevices", "ragg", "svglite"),
    device = "png",
    format = "png",
    width = 1200,
    height = 627,
    units = "px",
    dpi = 320,
    scale = 1,
    bg = "white",
    overwrite = FALSE,
    quiet = FALSE,
    filename_stem = NULL,
    ...
) {
  ## ----- Validate inputs -------------------------------------------------------
  engine <- match.arg(engine)

  if (!inherits(plt, "ggplot")) {
    rlang::abort("Argument `plt` must be a ggplot object.", class = "save_econanalyzr_chart_bad_plot")
  }
  if (!rlang::is_string(folder) || is.na(folder) || nchar(folder) == 0L) {
    rlang::abort("`folder` must be a non-missing string scalar.", class = "save_econanalyzr_chart_bad_folder")
  }

  # Make sure the output directory exists
  if (!dir.exists(folder)) {
    ok <- dir.create(folder, recursive = TRUE, showWarnings = FALSE)
    if (!ok) {
      rlang::abort(glue::glue("Failed to create output directory: '{folder}'"),
                   class = "save_econanalyzr_chart_dir_create_failed")
    }
  }

  # Data for filename construction: prefer explicit `data`, else plt$data
  df <- if (!is.null(data)) data else plt$data
  if (!is.data.frame(df)) {
    rlang::abort(
      "Could not find a usable data frame for filename construction. Supply via `data = ...`.",
      class = "save_econanalyzr_chart_missing_data"
    )
  }
  df <- check_econanalyzr_df(df)  # may reorder/coerce; returns tibble

  ## ----- Build filename stem (date + *_text tokens) ---------------------------
  date_stem <- make_file_name_date(df[["date"]])

  text_cols   <- names(df)[endsWith(names(df), "_text")]
  meta_tokens <- make_metadata_vec(df[, text_cols, drop = FALSE])  # character(0) if no *_text

  stem <- if (is.null(filename_stem)) {
    paste(c(date_stem, unname(meta_tokens)), collapse = "-")
  } else {
    filename_stem
  }
  safe_stem <- sanitize_filename(stem)

  ## ----- Determine extension and outpath --------------------------------------
  # ggsave: infer extension from `device` if character; else default "png"
  # others: use `format` argument (default "png")
  ext <- if (engine == "ggsave") {
    if (is.character(device) && length(device) == 1L) tolower(device) else "png"
  } else {
    tolower(format %||% "png")
  }

  filename <- paste0(safe_stem, ".", ext)
  outpath  <- file.path(folder, filename)

  # Overwrite protection
  if (!overwrite && file.exists(outpath)) {
    rlang::abort(
      glue::glue("File already exists: '{outpath}'. Set `overwrite = TRUE` to replace."),
      class = "save_econanalyzr_chart_exists"
    )
  }

  ## ----- Save using selected engine -------------------------------------------
  if (identical(engine, "ggsave")) {
    # Guard against conflicting args in ...
    dots <- list(...)
    if (length(dots)) {
      forbidden <- intersect(names(dots), c("filename", "path", "plot"))
      if (length(forbidden)) {
        rlang::abort(
          glue::glue("Do not pass {paste(forbidden, collapse = ', ')} in `...`; they are set by `save_econanalyzr_chart()`."),
          class = "save_econanalyzr_chart_conflicting_args"
        )
      }
    }

    if (!quiet) rlang::inform(glue::glue("Saving chart via ggsave(): {outpath}"), class = "save_econanalyzr_chart_ok")

    ggplot2::ggsave(
      filename = filename,
      plot     = plt,
      device   = device,
      path     = folder,
      width    = width,
      height   = height,
      units    = units,
      dpi      = dpi,
      scale    = scale,
      bg       = bg,
      ...
    )

    return(invisible(outpath))
  }

  # For device-based engines, we open a device -> print(plt) -> dev.off()
  # Compute pixel and inch dimensions; different devices expect different units.
  dims <- .econ_dims(width, height, units, dpi)  # list: width_px, height_px, width_in, height_in

  if (!quiet) rlang::inform(glue::glue("Saving chart via {engine}: {outpath}"), class = "save_econanalyzr_chart_ok")

  switch(engine,
         "grDevices" = .econ_save_grdevices(plt, outpath, ext, dims, bg, dpi),
         "ragg"      = .econ_save_ragg(plt, outpath, ext, dims, bg, dpi),
         "svglite"   = .econ_save_svglite(plt, outpath, ext, dims),
         rlang::abort("Unknown engine.", class = "save_econanalyzr_chart_bad_engine")
  )

  invisible(outpath)
}

# Null-coalescing helper
`%||%` <- function(a, b) if (is.null(a)) b else a

# Compute pixel and inch dimensions given width/height/units/dpi
.econ_dims <- function(width, height, units, dpi) {
  # Convert to inches
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

  # Convert to pixels (rounded for device calls that require integers)
  to_px <- switch(units,
                  "px" = 1,
                  "in" = dpi,
                  "cm" = dpi / 2.54,
                  "mm" = dpi / 25.4
  )
  width_px  <- as.integer(round(width  * to_px))
  height_px <- as.integer(round(height * to_px))

  list(width_in = width_in, height_in = height_in, width_px = width_px, height_px = height_px)
}

# Engine: base grDevices (png/jpeg/tiff/bmp are raster; pdf/svg are vector)
.econ_save_grdevices <- function(plt, outpath, ext, dims, bg, dpi) {
  # Normalize extension
  e <- tolower(ext)
  # Open device appropriately, print plot, close
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
  if (e == "pdf") {
    grDevices::pdf(file = outpath, width = dims$width_in, height = dims$height_in, onefile = FALSE)
    on.exit(grDevices::dev.off(), add = TRUE)
    print(plt)
    return(invisible())
  }
  if (e == "svg") {
    # Use base svg device if svglite engine wasn't chosen
    grDevices::svg(filename = outpath, width = dims$width_in, height = dims$height_in)
    on.exit(grDevices::dev.off(), add = TRUE)
    print(plt)
    return(invisible())
  }
  rlang::abort(glue::glue("Unsupported format for grDevices engine: '{ext}'"),
               class = "save_econanalyzr_chart_bad_format")
}

# Engine: ragg (raster only); requires Suggests: ragg
.econ_save_ragg <- function(plt, outpath, ext, dims, bg, dpi) {
  if (!requireNamespace("ragg", quietly = TRUE)) {
    rlang::abort("Engine 'ragg' requires the 'ragg' package. Install it or choose another engine.",
                 class = "save_econanalyzr_chart_missing_backend")
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
  # ragg devices accept width/height in pixels if units = "px" (commonly supported).
  # Fall back to inches if needed, but "px" is preferred for crisp raster sizing.
  fun(filename = outpath, width = dims$width_px, height = dims$height_px, units = "px", res = dpi, background = bg)
  on.exit(grDevices::dev.off(), add = TRUE)
  print(plt)
  invisible()
}

# Engine: svglite (vector SVG only); requires Suggests: svglite
.econ_save_svglite <- function(plt, outpath, ext, dims) {
  if (tolower(ext) != "svg") {
    rlang::abort("Engine 'svglite' only supports 'svg' format.",
                 class = "save_econanalyzr_chart_bad_format")
  }
  if (!requireNamespace("svglite", quietly = TRUE)) {
    rlang::abort("Engine 'svglite' requires the 'svglite' package. Install it or choose another engine.",
                 class = "save_econanalyzr_chart_missing_backend")
  }
  svglite::svglite(file = outpath, width = dims$width_in, height = dims$height_in)
  on.exit(grDevices::dev.off(), add = TRUE)
  print(plt)
  invisible()
}
