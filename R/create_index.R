#' Create Indexed Values from a Numeric Vector
#'
#' Computes an index column from a numeric vector based on a specified base position and index type.
#'
#' This function is useful for creating indexed time series or normalizing numeric vectors for comparison.
#'
#' @param num_vec A numeric vector of length greater than 1.
#' @param idx_pos A numeric scalar indicating the index position to use as the base (default is 1).
#' @param idx_type A numeric scalar; either `100` (index relative to 100) or `0` (relative percent change). Default is `100`.
#'
#' @return A numeric vector of indexed values.
#'
#' @examples
#' # Basic usage
#' create_index(c(50, 100, 150))
#'
#' # Index centered at 0%
#' create_index(c(50, 100, 150), idx_type = 0)
#'
#' # Using the second value as base
#' create_index(c(90, 100, 120), idx_pos = 2)
#'
#'
#' @export
create_index <- function(num_vec, idx_pos = 1, idx_type = 100) {
  # Validate num_vec
  if (!is.numeric(num_vec) || length(num_vec) < 2) {
    stop("'num_vec' must be a numeric vector of length greater than 1.")
  }

  # Validate idx_pos
  if (!is.numeric(idx_pos) || length(idx_pos) != 1 || !(idx_pos %in% seq_along(num_vec))) {
    stop("'idx_pos' must be a single numeric index within the bounds of 'num_vec'.")
  }

  # Validate idx_type
  if (!idx_type %in% c(100, 0)) {
    stop("'idx_type' must be either 100 or 0.")
  }

  base_value <- num_vec[idx_pos]

  # Throw error if base_value is invalid for division
  if (is.na(base_value) || is.nan(base_value) || is.infinite(base_value) || base_value == 0) {
    stop(
      "'base_value' at the index position supplied is invalid (0, NA, NaN, or Inf) and cannot be used to compute an index."
    )
  }

  # Warn if other values in num_vec are problematic (excluding base)
  check_mask <- seq_along(num_vec) != idx_pos
  if (any(is.na(num_vec[check_mask]) |
          is.nan(num_vec[check_mask]) |
          is.infinite(num_vec[check_mask]))) {
    warning("Some values in 'num_vec' (excluding base) are NA, NaN, or Inf; resulting index may contain NA or NaN.")
  }

  # Compute index
  if (idx_type == 100) {
    index <- (num_vec / base_value) * 100
  } else {
    index <- ((num_vec / base_value) - 1) * 100
  }

  return(index)
}
