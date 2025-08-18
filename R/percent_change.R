#' Calculate percent change between two values
#'
#' Computes the percent change from `start_value` to `end_value`:
#' \deqn{(end - start) / start}
#'
#' Vectorized for tidyverse workflows (e.g., `dplyr::mutate()`).
#'
#' @param start_value Numeric **scalar or vector** of starting values.
#' @param end_value   Numeric **scalar or vector** of ending values.
#'
#' @return A numeric scalar or vector of percent changes (same length as recycled inputs).
#' @export
#' @importFrom vctrs vec_recycle_common
#' @importFrom rlang abort warn
#' @importFrom glue glue
#'
#' @examples
#' percent_change(100, 120)                      # 0.20 (20% increase)
#' percent_change(c(100, 80), c(110, 72))        # c(0.10, -0.10)
#' percent_change(c(100, NA), c(110, 120))       # c(0.10, NA)
#' percent_change(0, 5)                          # NA with a warning (division by zero)
percent_change <- function(start_value, end_value) {
  # Validate Non-NULL ----------------------------------------------------------
  if (is.null(start_value) || is.null(end_value)) {
    rlang::abort(
      class = "percent_change_null_input",
      message = "`start_value` and `end_value` cannot be NULL."
    )
  }

  # Recycle Inputs To Common Size ---------------------------------------------
  args <- tryCatch(
    vctrs::vec_recycle_common(start_value = start_value, end_value = end_value),
    error = function(cnd) {
      rlang::abort(
        class   = "percent_change_bad_length_mismatch",
        message = glue::glue(
          "Lengths must match (unless scalar). Got start_value = {length(start_value)}, end_value = {length(end_value)}."
        ),
        parent  = cnd
      )
    }
  )
  start_value <- args$start_value
  end_value   <- args$end_value

  # Validate Types -------------------------------------------------------------
  if (!is.numeric(start_value) || !is.numeric(end_value)) {
    rlang::abort(
      class = "percent_change_bad_type",
      message = "`start_value` and `end_value` must both be numeric."
    )
  }

  # Validate Finiteness (Allow NA, Error on Inf/-Inf/NaN) ---------------------
  bad_start <- is.nan(start_value) | (is.infinite(start_value) & !is.na(start_value))
  bad_end   <- is.nan(end_value)   | (is.infinite(end_value)   & !is.na(end_value))
  if (any(bad_start | bad_end)) {
    rlang::abort(
      class   = "percent_change_nonfinite_values",
      message = "`start_value` and `end_value` must be finite (no Inf/-Inf/NaN). NA is allowed."
    )
  }

  # Warn Once On NA Inputs (Optional) -----------------------------------------
  has_na <- any(is.na(start_value) | is.na(end_value))
  if (has_na) {
    rlang::warn(
      class = "percent_change_na_input",
      message = "NA values detected; returning NA for those positions."
    )
  }

  # Handle Zero Denominator (Division By Zero) --------------------------------
  # Policy: return NA for start == 0 (undefined percent change), warn once.
  zero_den <- !is.na(start_value) & start_value == 0
  if (any(zero_den)) {
    rlang::warn(
      class = "percent_change_zero_denominator",
      message = "Division by zero in `start_value`; returning NA for those positions."
    )
  }

  # Compute On Valid Positions Only -------------------------------------------
  result <- rep_len(NA_real_, length(start_value))
  valid  <- !is.na(start_value) & !is.na(end_value) & start_value != 0
  # percent change = (end - start) / start
  result[valid] <- (end_value[valid] - start_value[valid]) / start_value[valid]

  # Return ---------------------------------------------------------------------
  return(result)
}
