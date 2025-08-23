#' Sanitize a string for use as a filename stem
#'
#' Produces a conservative, filesystem-friendly slug:
#' - Lowercases the string.
#' - Replaces any whitespace (one or more) with underscores.
#' - Replaces any character not in `[a-z0-9._-]` with underscores.
#' - Collapses multiple underscores to a single underscore.
#' - Trims leading/trailing underscores and dots.
#'
#' This does **not** add an extension; you can append e.g. `".csv"` afterward.
#' `NA` inputs are returned as `NA`.
#'
#' @param x A character vector (will be coerced via `as.character()`).
#'
#' @return A character vector of sanitized filename stems, same length as `x`.
#'
#' @keywords internal
#' @noRd
sanitize_filename <- function(x) {
  x_na <- is.na(x)
  y <- tolower(as.character(x))
  y[!x_na] <- gsub("\\s+", "_", y[!x_na], perl = TRUE)           # whitespace → _
  y[!x_na] <- gsub("[^a-z0-9._-]+", "_", y[!x_na], perl = TRUE)  # non-allowed → _
  y[!x_na] <- gsub("_+", "_", y[!x_na], perl = TRUE)             # collapse __ → _
  y[!x_na] <- sub("^[_\\.]+", "", y[!x_na], perl = TRUE)         # trim leading _/.
  y[!x_na] <- sub("[_\\.]+$", "", y[!x_na], perl = TRUE)         # trim trailing _/.
  y
}
