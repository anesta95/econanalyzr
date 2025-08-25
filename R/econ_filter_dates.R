#' Filter rows by a closed date interval (econanalyzr data; non-grouped)
#'
#' Filters an econanalyzr-valid data frame by a **closed** date interval
#' `[start_date, end_date]`. If `start_date` is not supplied, you can specify a
#' period (`period_type` + `period_amount`) and the function will compute
#' `start_date = end_date - period` (with `end_date` defaulting to the latest
#' non-NA date in `df`).
#'
#' The interval is **inclusive**: `date >= start_date` and `date <= end_date`.
#'
#' @param df A data frame validated by [check_econanalyzr_df()].
#' @param start_date Optional `Date` or `POSIXt`. If `NULL`, you must provide
#'   `period_type` and `period_amount` to derive it.
#' @param end_date Optional `Date` or `POSIXt`. If `NULL`, defaults to the latest
#'   non-NA date in `df`.
#' @param period_type Optional character scalar: one of `"days"`, `"weeks"`,
#'   `"months"`, `"quarters"`, `"years"`. Used only if `start_date` is `NULL`.
#' @param period_amount Optional positive integer-like scalar (e.g., `6` for
#'   "last 6 months"). Used only if `start_date` is `NULL`.
#' @param datetime_tz Timezone for coercing `POSIXt` inputs to `Date`. Default `"UTC"`.
#' @param quiet If `FALSE` (default), emit a short message describing the filter.
#'
#' @return The filtered data frame, arranged by descending `date`.
#'
#' @examples
#' # Last 6 months ending at the table’s latest date
#' # econ_filter_dates(df, period_type = "months", period_amount = 6)
#'
#' # Between two explicit dates
#' # econ_filter_dates(
#' #   df,
#' #   start_date = as.Date("2024-01-01"),
#' #   end_date   = as.Date("2024-12-01")
#' # )
#'
#' @export
#' @importFrom rlang abort inform is_integerish
#' @importFrom glue glue
#' @importFrom dplyr arrange filter
#' @importFrom lubridate days weeks years %m-%
econ_filter_dates <- function(
    df,
    start_date    = NULL,
    end_date      = NULL,
    period_type   = c("days", "weeks", "months", "quarters", "years"),
    period_amount = NULL,
    datetime_tz   = "UTC",
    quiet         = FALSE
) {
  # 0) Validate econanalyzr schema; guarantees `date` is Date
  df <- check_econanalyzr_df(df)

  # Coercion helper
  coerce_to_date <- function(x) {
    if (inherits(x, "POSIXt")) return(as.Date(x, tz = datetime_tz))
    if (inherits(x, "Date"))   return(x)
    rlang::abort("Dates must be Date (or POSIXt coercible to Date).",
                 class = "econ_filter_dates_bad_date_type")
  }

  # Normalize supplied dates
  if (!is.null(start_date)) start_date <- coerce_to_date(start_date)
  if (!is.null(end_date))   end_date   <- coerce_to_date(end_date)

  # Determine end_date (latest non-NA date) if missing
  if (is.null(end_date)) {
    non_na_dates <- df$date[!is.na(df$date)]
    if (length(non_na_dates) == 0L) {
      # No valid dates — return empty with a gentle message
      if (!quiet) rlang::inform("No non-NA dates found; returning 0 rows.", class = "econ_filter_dates_message")
      return(df[0, , drop = FALSE])
    }
    end_date <- max(non_na_dates)
  }

  # If start_date missing, require a period to derive it
  period_type <- match.arg(period_type)
  if (is.null(start_date)) {
    if (is.null(period_amount)) {
      rlang::abort(
        "Provide either `start_date` or (`period_type` + `period_amount`) to derive it.",
        class = "econ_filter_dates_missing_start"
      )
    }
    # Positive integer-ish period_amount
    if (!rlang::is_integerish(period_amount, n = 1, finite = TRUE) || period_amount <= 0) {
      rlang::abort(
        "`period_amount` must be a positive integer-like scalar (e.g., 6).",
        class = "econ_filter_dates_bad_period_amount"
      )
    }

    # Derive start_date from end_date + period (calendar-aware for months/quarters/years)
    start_date <- switch(
      period_type,
      days     = end_date - lubridate::days(period_amount),
      weeks    = end_date - lubridate::weeks(period_amount),
      months   = end_date %m-% base::months(period_amount),
      quarters = end_date %m-% base::months(3L * period_amount),
      years    = end_date %m-% lubridate::years(period_amount)
    )
  } else {
    # If start_date is supplied, don't allow period_amount to avoid ambiguity
    if (!is.null(period_amount)) {
      rlang::abort(
        "Do not supply `period_amount` when `start_date` is given.",
        class = "econ_filter_dates_conflicting_args"
      )
    }
  }

  # Sanity: start <= end
  if (start_date > end_date) {
    rlang::abort(
      glue::glue("`start_date` ({start_date}) is after `end_date` ({end_date})."),
      class = "econ_filter_dates_start_after_end"
    )
  }

  # Filter and order newest-first
  filtered_df <- df |>
    dplyr::filter(.data$date >= start_date, .data$date <= end_date) |>
    dplyr::arrange(dplyr::desc(.data$date))

  # Short message
  if (!quiet) {
    rlang::inform(
      glue::glue("Filtered to [{start_date}, {end_date}] (closed interval)."),
      class = "econ_filter_dates_message"
    )
  }

  filtered_df
}
