#' Calculate annualized rate of change
#'
#' Computes the annualized rate of change from paired start and end values
#' over a given time interval, annualizing according to the specified unit.
#'
#' @param start_values Numeric scalar or vector of positive start values.
#' @param end_values   Numeric scalar or vector of positive end values.
#'                     If any of `start_values`, `end_values`, or `time_elapsed` is
#'                     a scalar, it is recycled to the common length; otherwise,
#'                     lengths must be compatible.
#' @param time_elapsed Numeric scalar or vector of elapsed time in `time_unit`
#'                     between start and end. If scalar, it is recycled.
#' @param time_unit    One of "daily", "weekly", "monthly", "quarterly", "annually".
#' @param year_length  Days per year used when `time_unit` is "daily" (and to derive
#'                     weeks/year when `time_unit` is "weekly"). Defaults to 365.2425
#'                     (Gregorian average year).
#'
#' @return A numeric scalar or vector of annualized rates, same length as the recycled inputs.
#'
#' @importFrom rlang abort
#' @importFrom glue glue
#' @importFrom vctrs vec_recycle_common
#'
#' @examples
#' # Scalar (monthly): from 100 to 103 over 1 month -> annualized rate
#' annualize_change(100, 103, time_elapsed = 1, time_unit = "monthly")
#'
#' # Vectorized (quarterly): scalar start recycled to match end/time vectors
#' annualize_change(
#'   start_values = 100,
#'   end_values   = c(101, 98, 120),
#'   time_elapsed = c(1, 2, 3),
#'   time_unit    = "quarterly"
#' )
#'
#' # Weekly with exact 52-week convention (52 * 7 = 364 days)
#' annualize_change(
#'   start_values = 100,
#'   end_values   = 102,
#'   time_elapsed = 4,
#'   time_unit    = "weekly",
#'   year_length  = 364
#' )
#' @export
annualize_change <- function(
    start_values,
    end_values,
    time_elapsed,
    time_unit   = c("daily", "weekly", "monthly", "quarterly", "annually"),
    year_length = 365.2425
) {
  # Match Scalar Choice --------------------------------------------------------
  time_unit <- match.arg(time_unit)

  # Validate Basic Types -------------------------------------------------------
  if (!is.numeric(start_values) || !is.numeric(end_values)) {
    rlang::abort(
      class = "annualize_change_bad_type",
      message = "`start_values` and `end_values` must both be numeric."
    )
  }
  if (!is.numeric(time_elapsed) || length(time_elapsed) < 1L) {
    rlang::abort(
      class = "annualize_change_bad_time_elapsed_type",
      message = "`time_elapsed` must be a numeric scalar or numeric vector."
    )
  }
  if (!is.numeric(year_length) || length(year_length) != 1L || !is.finite(year_length) || year_length <= 0) {
    rlang::abort(
      class = "annualize_change_bad_year_length",
      message = "`year_length` must be a positive, finite numeric scalar."
    )
  }

  # Recycle All Inputs To A Common Size ---------------------------------------
  # Allows any of the three inputs to be length-1 scalars; otherwise lengths must be equal.
  recycled <- tryCatch(
    vctrs::vec_recycle_common(
      start_values = start_values,
      end_values   = end_values,
      time_elapsed = time_elapsed
    ),
    error = function(cnd) {
      # Map vctrs size errors to a package-specific class/message while preserving parent
      rlang::abort(
        class   = "annualize_change_bad_length_mismatch",
        message = glue::glue(
          "Lengths must match (unless scalar). Got start_values = {length(start_values)}, ",
          "end_values = {length(end_values)}, time_elapsed = {length(time_elapsed)}."
        ),
        parent  = cnd
      )
    }
  )

  start_values <- recycled$start_values
  end_values   <- recycled$end_values
  time_elapsed <- recycled$time_elapsed
  # n <- length(start_values)  # (All three now share the same size; 'n' not otherwise needed)

  # Validate Magnitudes (Positivity & Finiteness) ------------------------------
  # start/end must be finite **or NA** (NA is allowed and should propagate);
  # Inf/-Inf/NaN should error. Exclude NA explicitly in the non-finite check.
  nonfinite_start <- !is.na(start_values) & !is.finite(start_values)
  nonfinite_end   <- !is.na(end_values)   & !is.finite(end_values)
  if (any(nonfinite_start | nonfinite_end)) {
    rlang::abort(
      class   = "annualize_change_nonfinite_values",
      message = "`start_values` and `end_values` must be finite (no Inf/-Inf/NaN). NA is allowed."
    )
  }
  if (any(start_values <= 0 | end_values <= 0, na.rm = TRUE)) {
    rlang::abort(
      class = "annualize_change_nonpositive_values",
      message = "`start_values` and `end_values` must be strictly positive."
    )
  }
  if (any(!is.finite(time_elapsed) | time_elapsed <= 0, na.rm = TRUE)) {
    rlang::abort(
      class = "annualize_change_bad_time_elapsed_values",
      message = "`time_elapsed` must be strictly positive and finite."
    )
  }

  # Units-Per-Year Factor ------------------------------------------------------
  # Uses Gregorian average by default (365.2425) for daily; weekly derives from it.
  # For more information on the Gregorian calendar, see: https://aa.usno.navy.mil/faq/leap_year
  units_per_year <- switch(
    time_unit,
    daily     = year_length,
    weekly    = year_length / 7,
    monthly   = 12,
    quarterly = 4,
    annually  = 1
  )

  # Annualization Exponent -----------------------------------------------------
  # If a change occurs over 'time_elapsed' units, the annualized factor repeats
  # 'units_per_year / time_elapsed' times per year.
  exponent <- units_per_year / time_elapsed

  # Numerically Stable Rate Computation ---------------------------------------
  # r = (end/start)^(exponent) - 1  ==  exp(exponent * (log(end) - log(start))) - 1
  # Compute in log space and use expm1() for accuracy when the rate is small.
  # For more information on using log values, see: https://gregorygundersen.com/blog/2020/02/09/log-sum-exp/
  # Here is more information on floating-point math: https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html
  z <- exponent * (log(end_values) - log(start_values))
  rate <- expm1(z)

  # Clean Non-Finite Results ---------------------------------------------------
  rate[!is.finite(rate)] <- NA_real_

  # Return ---------------------------------------------------------------------
  return(rate)
}
