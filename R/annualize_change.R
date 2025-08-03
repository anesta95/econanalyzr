#' Calculate Annualized Rate of Change
#'
#' Computes the annualized rate of change from a numeric vector of values over specified time periods.
#'
#' @param start_values A numeric vector of starting values.
#' @param end_values A numeric vector of ending values.
#' @param time_elapsed A single numeric value or vector of the same length as start_values/end_values, representing time units between values.
#' @param time_unit Character string: "daily", "weekly", "monthly", or "quarterly".
#' @param projection_direction Character string: "up" (project to annual) or "down" (normalize from long period to annual).
#'
#' @return A numeric vector of annualized rates of change.
#' @export
annualize_change <- function(start_values, end_values, time_elapsed,
                              time_unit = c("daily", "weekly", "monthly", "quarterly"),
                              projection_direction = c("up", "down")) {

  # Argument matching
  time_unit <- match.arg(time_unit)
  projection_direction <- match.arg(projection_direction)

  # Validate input types
  if (!is.numeric(start_values) || !is.numeric(end_values) || !is.numeric(time_elapsed)) {
    stop("Arguments 'start_values', 'end_values', and 'time_elapsed' must be numeric.")
  }

  # Validate length of start and end
  if (length(start_values) != length(end_values)) {
    stop("'start_values' and 'end_values' must be of the same length.")
  }

  # Expand single-value time_elapsed if necessary
  n <- length(start_values)
  if (length(time_elapsed) == 1) {
    time_elapsed <- rep(time_elapsed, n)
  } else if (length(time_elapsed) != n) {
    stop("'time_elapsed' must be a single value or a vector the same length as 'start_values'.")
  }

  # Validate contents
  if (any(start_values <= 0 | end_values <= 0, na.rm = TRUE)) {
    stop("Values in 'start_values' and 'end_values' must be positive.")
  }
  if (any(time_elapsed <= 0, na.rm = TRUE)) {
    stop("'time_elapsed' must contain only positive values.")
  }

  # Annualization factors
  time_unit_factors <- c(
    daily = 365,
    weekly = 52,
    monthly = 12,
    quarterly = 4
  )
  factor <- time_unit_factors[time_unit]

  # Exponent calculation
  exponent <- if (projection_direction == "up") {
    factor / time_elapsed
  } else {
    1 / (time_elapsed / factor)
  }

  # Rate calculation
  result <- (end_values / start_values)^exponent - 1
  result[is.nan(result) | is.infinite(result)] <- NA
  return(result)
}
