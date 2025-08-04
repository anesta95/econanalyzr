#' Calculate Percent Change Between Two Values
#'
#' Computes the percent change from a starting value to an ending value.
#'
#' This function is vectorized and designed for use in tidyverse workflows
#' such as `dplyr::mutate()`.
#'
#' @param start_value A numeric scalar or vector of starting values.
#' @param end_value A numeric scalar or vector of ending values.
#'
#' @return A numeric vector of percent changes.
#'
#' @importFrom vctrs vec_recycle_common
#'
#' @examples
#' percent_change(100, 120)      # 0.20 (20% increase)
#' percent_change(c(100, NA), c(110, 120))  # returns c(0.10, NA) with warning
#'
#' @export
percent_change <- function(start_value, end_value) {
  # Null check
  if (is.null(start_value) || is.null(end_value)) {
    stop("'start_value' and 'end_value' cannot be NULL.")
  }

  # Recycle input to common length
  args <- vctrs::vec_recycle_common(start_value, end_value)
  start_value <- args[[1]]
  end_value <- args[[2]]

  # Type check
  if (!is.numeric(start_value) || !is.numeric(end_value)) {
    stop("'start_value' and 'end_value' must be numeric.")
  }

  # Check for non-finite values (Inf, -Inf, NaN)
  nonfinite_mask <- is.infinite(start_value) | is.infinite(end_value) |
    is.nan(start_value) | is.nan(end_value)
  if (any(nonfinite_mask, na.rm = TRUE)) {
    stop("Both 'start_value' and 'end_value' must be finite (non-Inf and non-NaN).")
  }

  # Warn on NA values
  if (any(is.na(start_value) | is.na(end_value))) {
    warning("One or more input values are NA; returning NA for those cases.")
  }

  # Warn on divide-by-zero
  zero_mask <- !is.na(start_value) & start_value == 0
  if (any(zero_mask)) {
    warning("Division by zero detected in 'start_value'; output may contain Inf or NaN.")
  }

  # Calculation
  result <- rep(NA_real_, length(start_value))
  mask <- !is.na(start_value) & !is.na(end_value)
  result[mask] <- (end_value[mask] - start_value[mask]) / start_value[mask]
  return(result)
}
