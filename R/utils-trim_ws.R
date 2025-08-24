#' Trim leading and trailing whitespace
#'
#' Removes ASCII whitespace from the start and end of each string.
#' - Leaves internal whitespace untouched.
#' - Preserves `NA` values as `NA`.
#'
#' @param x A character vector (will be coerced via `as.character()`).
#'
#' @return A character vector the same length as `x` with leading/trailing
#'   whitespace removed.
#'
#' @keywords internal
#' @noRd
trim_ws <- function(x) {
  # Coerce to character while preserving NA; avoid errors on non-character inputs
  x <- as.character(x)
  # Remove leading then trailing whitespace with PCRE
  x <- sub("^\\s+", "", x, perl = TRUE)
  x <- sub("\\s+$", "", x, perl = TRUE)
  x
}
