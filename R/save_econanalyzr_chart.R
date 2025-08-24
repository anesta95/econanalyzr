#' Save a ggplot chart with an econanalyzr-style filename
#'
#' Builds a descriptive filename from the plot's data (or a supplied `data` frame):
#' `<date_or_range>-<meta1>-<meta2>-...-<metaK>.<ext>`.
#'
#' Filename construction reuses package helpers:
#' - [make_file_name_date()] for the date stem (single `YYYY-MM-DD` or `max_min` range),
#' - [make_metadata_vec()] for tokens derived from all `*_text` columns in order,
#' - [sanitize_filename()] to make a safe, lowercase slug.
#'
#' Saving backends:
#' - `engine = "ggsave"` (default): calls [ggplot2::ggsave()], honoring `device`, `width`, `height`,
#'   `units`, `dpi`, `scale`, `bg`, and any extra `...` that `ggsave()` supports.
#' - `engine = "grDevices"`: opens base devices (png/jpeg/tiff/bmp/pdf/svg) via [econ_viz_save_grdevices()].
#' - `engine = "ragg"`: high-quality raster (png/jpeg/tiff) via [econ_viz_save_ragg()] (Suggests: ragg).
#' - `engine = "svglite"`: vector SVG via [econ_viz_save_svglite()] (Suggests: svglite).
#'
#' @param plt A `ggplot` object.
#' @param folder Output directory path. Created if it doesn't exist.
#' @param data Optional data frame for filename construction. If `NULL`, uses `plt$data`.
#'   Must satisfy [check_econanalyzr_df()] or a classed error is raised.
#' @param engine Saving backend: one of `"ggsave"` (default), `"grDevices"`, `"ragg"`, `"svglite"`.
#' @param device For `engine = "ggsave"`, a device passed to [ggplot2::ggsave()]
#'   (e.g., `"png"`, `"pdf"`, `"svg"`, `"jpeg"`, `"tiff"`, or a device function). Ignored otherwise.
#' @param format Output format/extension for non-ggsave engines. One of `"png"`, `"pdf"`, `"svg"`,
#'   `"jpeg"`, `"jpg"`, `"tiff"`, `"bmp"`. Default `"png"`.
#' @param width,height Plot size (defaults target a crisp social card): `width = 3600`, `height = 2010`.
#' @param units Units for `width`/`height`: `"px"` (default), `"in"`, `"cm"`, `"mm"`.
#' @param dpi Resolution in dots per inch (used for raster devices or when converting from inches).
#' @param scale Scaling factor (multiplies `width`/`height`) passed to `ggsave()` only.
#' @param bg Background: `"white"` (default), `"transparent"`, etc. Passed to raster devices / `ggsave()`.
#' @param overwrite If `FALSE` (default) and the target file exists, abort with a classed error.
#' @param quiet If `FALSE` (default) emit an informational message on success.
#' @param name_stem Optional override for the filename stem (no extension). Will be sanitized.
#'   If `NULL`, the stem is built from `data`.
#' @param ... Additional arguments forwarded to [ggplot2::ggsave()] when `engine = "ggsave"`.
#'
#' @return (Invisibly) the full file path written.
#'
#' @export
#' @importFrom rlang abort inform is_string warn is_installed
#' @importFrom glue glue
#' @importFrom ggplot2 ggsave
save_econanalyzr_chart <- function(
    plt,
    folder,
    data = NULL,
    engine = c("ggsave", "grDevices", "ragg", "svglite"),
    device = "png",
    format = "png",
    width = 3600,
    height = 2010,
    units = "px",
    dpi = 320,
    scale = 1,
    bg = "white",
    overwrite = FALSE,
    quiet = FALSE,
    name_stem = NULL,
    ...
) {
  # ---- Validate high-level inputs ---------------------------------------------
  engine <- match.arg(engine)

  if (!inherits(plt, "ggplot")) {
    rlang::abort("Argument `plt` must be a ggplot object.",
                 class = "save_econanalyzr_chart_bad_plot")
  }
  if (!rlang::is_string(folder) || is.na(folder) || nchar(folder) == 0L) {
    rlang::abort("`folder` must be a non-missing string scalar.",
                 class = "save_econanalyzr_chart_bad_folder")
  }

  # Ensure output directory exists (create recursively if needed)
  if (!dir.exists(folder)) {
    ok <- dir.create(folder, recursive = TRUE, showWarnings = FALSE)
    if (!ok) {
      rlang::abort(glue::glue("Failed to create output directory: '{folder}'"),
                   class = "save_econanalyzr_chart_dir_create_failed")
    }
  }

  # Data used to construct the filename: prefer explicit `data`; fallback to plt$data.
  # We don't try to scrape layer data automatically.
  df <- if (!is.null(data)) data else plt$data
  if (!is.data.frame(df)) {
    rlang::abort(
      "Could not find a usable data frame for filename construction. Supply via `data = ...`.",
      class = "save_econanalyzr_chart_missing_data"
    )
  }

  # Validate & normalize the econanalyzr schema (may reorder/coerce columns)
  df <- check_econanalyzr_df(df)

  # ---- Build the filename stem (date + *_text tokens) -------------------------
  date_stem   <- make_file_name_date(df[["date"]])
  text_cols   <- names(df)[endsWith(names(df), "_text")]
  # make_metadata_vec() should return character(0) for 0-column input
  meta_tokens <- make_metadata_vec(df[, text_cols, drop = FALSE])

  stem <- if (is.null(name_stem)) {
    paste(c(date_stem, unname(meta_tokens)), collapse = "-")
  } else {
    name_stem
  }
  safe_stem <- sanitize_filename(stem)

  # Choose the extension for the output file:
  # - ggsave: derive from `device` if character; else default "png"
  # - other engines: use `format` parameter
  ext <- if (engine == "ggsave") {
    if (is.character(device) && length(device) == 1L) tolower(device) else "png"
  } else {
    tolower(format)
  }

  filename <- paste0(safe_stem, ".", ext)
  outpath  <- file.path(folder, filename)

  # Overwrite protection: fail early if file exists and overwrite = FALSE
  if (!overwrite && file.exists(outpath)) {
    rlang::abort(
      glue::glue("File already exists: '{outpath}'. Set `overwrite = TRUE` to replace."),
      class = "save_econanalyzr_chart_exists"
    )
  }

  # Compute dimensions in both px and inches for whichever device path is used
  dims <- econ_viz_dims(width, height, units, dpi)

  # ---- Save using the selected engine -----------------------------------------
  if (identical(engine, "ggsave")) {
    # Guard against conflicting arguments inside `...` that would fight ours
    dots <- list(...)
    forbidden <- intersect(names(dots), c("filename", "path", "plot"))
    if (length(forbidden)) {
      rlang::abort(
        glue::glue("Do not pass {paste(forbidden, collapse = ', ')} in `...`; they are set by `save_econanalyzr_chart()`."),
        class = "save_econanalyzr_chart_conflicting_args"
      )
    }

    if (!quiet) rlang::inform(glue::glue("Saving chart via ggsave(): {outpath}"),
                              class = "save_econanalyzr_chart_ok")

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

  # Device-based engines: open -> print -> dev.off()
  if (!quiet) rlang::inform(glue::glue("Saving chart via {engine}: {outpath}"),
                            class = "save_econanalyzr_chart_ok")

  switch(engine,
         "grDevices" = econ_viz_save_grdevices(plt, outpath, ext, dims, bg, dpi),
         "ragg"      = econ_viz_save_ragg(plt, outpath, ext, dims, bg, dpi),
         "svglite"   = econ_viz_save_svglite(plt, outpath, ext, dims),
         rlang::abort("Unknown engine.", class = "save_econanalyzr_chart_bad_engine")
  )

  invisible(outpath)
}
