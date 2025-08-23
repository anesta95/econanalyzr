#' Write a validated econanalyzr tibble to CSV with a descriptive filename
#'
#' See details in prior message; unchanged contract.
#'
#' @param df A data frame/tibble that should satisfy check_econanalyzr_df().
#' @param folder Output directory.
#' @param overwrite If FALSE and file exists, abort (default FALSE).
#' @param quiet If FALSE, emit an info message (default FALSE).
#' @return Invisibly, the file path written.
#' @export
#' @importFrom readr write_csv
#' @importFrom rlang abort inform is_string
#' @importFrom glue glue
econ_csv_write_out <- function(df, folder, overwrite = FALSE, quiet = FALSE) {
  # Validate folder
  if (!rlang::is_string(folder) || is.na(folder) || nchar(folder) == 0L) {
    rlang::abort("`folder` must be a non-missing string scalar.",
                 class = "econ_csv_write_out_bad_folder")
  }

  # Validate & normalize schema (may reorder/coerce)
  df <- check_econanalyzr_df(df)

  # Ensure folder exists
  if (!dir.exists(folder)) {
    ok <- dir.create(folder, recursive = TRUE, showWarnings = FALSE)
    if (!ok) {
      rlang::abort(glue::glue("Failed to create output directory: '{folder}'"),
                   class = "econ_csv_write_out_dir_create_failed")
    }
  }

  # Filename parts
  date_stem <- make_file_name_date(df[["date"]])

  text_cols <- names(df)[grepl("_text$", names(df), perl = TRUE)]
  meta_tokens <- if (length(text_cols)) {
    make_metadata_vec(df[, text_cols, drop = FALSE])  # warns on all-NA columns
  } else {
    character(0)
  }

  stem <- paste(c(date_stem, unname(meta_tokens)), collapse = "-")
  safe_stem <- sanitize_filename(stem)
  filename <- file.path(folder, paste0(safe_stem, ".csv"))

  # Overwrite protection
  if (!overwrite && file.exists(filename)) {
    rlang::abort(
      glue::glue("File already exists: '{filename}'. Set `overwrite = TRUE` to replace."),
      class = "econ_csv_write_out_exists"
    )
  }

  # Write
  readr::write_csv(df, file = filename)

  if (!quiet) {
    rlang::inform(glue::glue("Wrote CSV: {filename}"), class = "econ_csv_write_out_ok")
  }

  invisible(filename)
}
