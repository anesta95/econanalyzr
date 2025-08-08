#' Calculate a Diffusion Index
#'
#' Computes a diffusion index using one of several popular methodologies.
#'
#' Supported index types include:
#' - `"Federal Reserve"`: Difference between percent increased and decreased, multiplied by 100.
#' - `"IHS-PMI"`: Sum of percent increased and half the percent unchanged.
#' - `"Conference Board"`: Encodes percent change into categorical scores (1, 0.5, 0) and returns the mean.
#'
#' @param pct_increased Numeric scalar or vector: percent of items that increased.
#' @param pct_decreased Numeric scalar or vector: percent of items that decreased.
#' @param pct_unchanged Numeric scalar or vector: percent of items unchanged.
#' @param pct_change Numeric scalar or vector: percent change for Conference Board method.
#' @param idx_type Character string: one of "Federal Reserve" (default), "IHS-PMI", or "Conference Board".
#'
#' @return A numeric vector representing the diffusion index.
#'
#' @importFrom glue glue
#'
#' @examples
#' create_diffusion_index(pct_increased = c(0.6, 0.5), pct_decreased = c(0.2, 0.3))
#' create_diffusion_index(pct_increased = 0.55, pct_unchanged = 0.30, idx_type = "IHS-PMI")
#' create_diffusion_index(pct_change = c(0.001, -0.001, 0.0001), idx_type = "Conference Board")
#'
#' @export
create_diffusion_index <- function(pct_increased = NULL,
                                   pct_decreased = NULL,
                                   pct_unchanged = NULL,
                                   pct_change = NULL,
                                   idx_type = c("Federal Reserve", "IHS-PMI", "Conference Board")) {

  idx_type <- match.arg(idx_type)

  # Helper for validating input content
  validate_input <- function(x, name) {
    if (!is.numeric(x)) stop(glue::glue("'{name}' must be numeric."))
    if (!all(is.finite(x), na.rm = TRUE)) stop(glue::glue("'{name}' must contain only finite values (no Inf, -Inf, or NaN)."))
  }

  # Warn if input has NAs
  warn_if_na <- function(x, name) {
    if (anyNA(x)) warning(glue::glue("'{name}' contains NA; resulting index values may include NA."))
  }

  if (idx_type == "Federal Reserve") {
    if (is.null(pct_increased) || is.null(pct_decreased)) {
      stop("Both 'pct_increased' and 'pct_decreased' must be provided for 'Federal Reserve' index type.")
    }
    if (!is.null(pct_unchanged) || !is.null(pct_change)) {
      warning("Ignoring 'pct_unchanged' and 'pct_change' for 'Federal Reserve' method.")
    }

    validate_input(pct_increased, "pct_increased")
    validate_input(pct_decreased, "pct_decreased")
    warn_if_na(pct_increased, "pct_increased")
    warn_if_na(pct_decreased, "pct_decreased")

    if (length(pct_increased) != length(pct_decreased)) {
      stop("'pct_increased' and 'pct_decreased' must be the same length.")
    }

    return((pct_increased - pct_decreased) * 100)
  }

  if (idx_type == "IHS-PMI") {
    if (is.null(pct_increased) || is.null(pct_unchanged)) {
      stop("Both 'pct_increased' and 'pct_unchanged' must be provided for 'IHS-PMI' index type.")
    }
    if (!is.null(pct_decreased) || !is.null(pct_change)) {
      warning("Ignoring 'pct_decreased' and 'pct_change' for 'IHS-PMI' method.")
    }

    validate_input(pct_increased, "pct_increased")
    validate_input(pct_unchanged, "pct_unchanged")
    warn_if_na(pct_increased, "pct_increased")
    warn_if_na(pct_unchanged, "pct_unchanged")

    if (length(pct_increased) != length(pct_unchanged)) {
      stop("'pct_increased' and 'pct_unchanged' must be the same length.")
    }

    return(pct_increased + 0.5 * pct_unchanged)
  }

  if (idx_type == "Conference Board") {
    if (is.null(pct_change)) {
      stop("'pct_change' must be provided for 'Conference Board' index type.")
    }
    if (!is.null(pct_increased) || !is.null(pct_decreased) || !is.null(pct_unchanged)) {
      warning("Ignoring 'pct_increased', 'pct_decreased', and 'pct_unchanged' for 'Conference Board' method.")
    }

    validate_input(pct_change, "pct_change")

    if (anyNA(pct_change)) {
      stop("'pct_change' contains NA; this is not permitted for 'Conference Board' method.")
    }

    encoded <- dplyr::case_when(
      pct_change > 0.0005 ~ 1,
      pct_change < -0.0005 ~ 0,
      abs(pct_change) <= 0.0005 ~ 0.5,
      TRUE ~ NA_real_
    )

    return(mean(encoded, na.rm = TRUE))
  }

  stop("Unhandled 'idx_type'. This should not occur.")
}
