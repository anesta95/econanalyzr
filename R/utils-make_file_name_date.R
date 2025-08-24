#' Build the date portion of a filename from a Date vector
#'
#' If there is exactly one unique non-NA date, return it as `YYYY-MM-DD`.
#' If there are multiple, return `maxDate_minDate` (both formatted `YYYY-MM-DD`).
#' Errors if there are no non-NA dates or if `date_vec` is not Date/POSIXt.
#'
#' @param date_vec A Date vector (POSIXt allowed; coerced to Date).
#' @return A character stem like `"2025-05-01"` or `"2025-05-01_2021-05-01"`.
#' @keywords internal
#' @rdname econanalyzr-viz-helpers
make_file_name_date <- function(date_vec) {
  # Accept Date; allow POSIXt but coerce to Date (drop time)
  if (inherits(date_vec, "POSIXct") || inherits(date_vec, "POSIXlt")) {
    date_vec <- as.Date(date_vec)
  } else if (!inherits(date_vec, "Date")) {
    rlang::abort("`date_vec` must be Date or POSIXt.", class = "make_file_name_date_bad_type")
  }

  non_na <- date_vec[!is.na(date_vec)]
  if (length(non_na) == 0L) {
    rlang::abort("`date_vec` contains no non-NA dates.", class = "make_file_name_date_all_na")
  }

  u <- unique(non_na)
  if (length(u) == 1L) {
    return(format(u, "%Y-%m-%d"))
  } else {
    mx <- max(non_na)
    mn <- min(non_na)
    return(paste(format(mx, "%Y-%m-%d"), format(mn, "%Y-%m-%d"), sep = "_"))
  }
}
