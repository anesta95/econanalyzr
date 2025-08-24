#' Apply a function to a numeric column after date filtering (econanalyzr data)
#'
#' Filters an econanalyzr-valid data frame by either a set of membership dates
#' (`dates`) or a closed range (`date_range`), pulls a **numeric** column, and
#' applies a user-supplied function `.fun` to that vector.
#'
#' Rules:
#' - If both `dates` and `date_range` are `NULL`, the whole data is used.
#' - If both are supplied, a classed error is raised.
#' - If the filter yields 0 rows and `empty_ok = FALSE`, returns `NA_real_` and warns.
#' - If `na_rm = TRUE`, `NA`s are removed *before* calling `.fun`.
#'
#' @param df A data frame validated by [check_econanalyzr_df()].
#' @param dates Optional `Date` (or `POSIXt`) scalar/vector; rows with `date %in% dates`
#'   are included/excluded per `filter_type`. `POSIXt` is coerced to `Date` using `dates_tz`.
#' @param date_range Optional length-2 `Date`/`POSIXt` giving a closed range
#'   `[min(date_range), max(date_range)]`. Ignored if `dates` is provided.
#' @param val_col Column to operate on (must be numeric): tidy-select name or numeric position.
#'   Defaults to `value`.
#' @param filter_type `"inclusive"` (keep matches) or `"exclusive"` (drop matches).
#' @param .fun A function (or function name) applied to the pulled vector, e.g. `mean`,
#'   `median`, or `function(x) stats::quantile(x, c(.25,.5,.75))`.
#' @param na_rm Logical; if `TRUE` (default) remove `NA`s before calling `.fun`.
#' @param dates_tz Timezone for coercing `POSIXt` (dates or range) to `Date`. Default `"UTC"`.
#' @param empty_ok If `FALSE` (default) and the filter returns 0 rows, return `NA_real_`
#'   with a warning. If `TRUE`, pass a length-0 vector to `.fun` (which may error).
#' @param ... Extra arguments forwarded to `.fun`.
#'
#' @return Whatever `.fun` returns (often a scalar; may be a vector).
#'
#' @examples
#' # Mean of 'value' for a set of dates
#' # econ_value_summary(
#' #   df, dates = as.Date(c("2025-01-01","2025-02-01")), .fun = mean
#' # )
#'
#' # Median over a date range (exclusive: drop rows in range)
#' # econ_value_summary(
#' #   df,
#' #   date_range = as.Date(c("2025-01-01","2025-03-31")),
#' #   filter_type = "exclusive",
#' #   .fun = median
#' # )
#'
#' # Vector result (quantiles)
#' # econ_value_summary(
#' #   df,
#' #   dates = unique(df$date),
#' #   .fun  = function(x) stats::quantile(x, c(.25,.5,.75))
#' # )
#'
#' @export
#' @importFrom rlang abort warn
#' @importFrom dplyr pull
econ_value_summary <- function(
    df,
    dates       = NULL,
    date_range  = NULL,
    val_col     = value,
    filter_type = c("inclusive", "exclusive"),
    .fun        = mean,
    na_rm       = TRUE,
    dates_tz    = "UTC",
    empty_ok    = FALSE,
    ...
) {
  filter_type <- match.arg(filter_type)

  # 1) Validate econanalyzr schema early ---------------------------------------
  df <- check_econanalyzr_df(df)

  # 2) Validate filters ---------------------------------------------------------
  if (!is.null(dates) && !is.null(date_range)) {
    rlang::abort(
      "`dates` and `date_range` cannot both be supplied.",
      class = "econ_value_summary_conflicting_filters"
    )
  }

  # Coerce/validate dates if supplied
  if (!is.null(dates)) {
    if (inherits(dates, "POSIXt")) dates <- as.Date(dates, tz = dates_tz)
    if (!inherits(dates, "Date")) {
      rlang::abort("`dates` must be Date (or POSIXt coercible to Date).",
                   class = "econ_value_summary_bad_dates_type")
    }
    if (anyNA(dates)) {
      rlang::warn("`dates` contains NA; dropping NA entries.",
                  class = "econ_value_summary_dates_na_dropped")
      dates <- dates[!is.na(dates)]
    }
    crit <- df$date %in% unique(dates)

  } else if (!is.null(date_range)) {
    # Coerce/validate date_range
    if (inherits(date_range, "POSIXt")) date_range <- as.Date(date_range, tz = dates_tz)
    if (!inherits(date_range, "Date") || length(date_range) < 2L) {
      rlang::abort("`date_range` must be length-2 Date/POSIXt (coercible to Date).",
                   class = "econ_value_summary_bad_range_type")
    }
    from <- min(date_range, na.rm = TRUE)
    to   <- max(date_range, na.rm = TRUE)
    # numeric test for finiteness (safe for Date)
    if (!is.finite(as.numeric(from)) || !is.finite(as.numeric(to))) {
      rlang::abort("`date_range` must contain finite dates.",
                   class = "econ_value_summary_bad_range_type")
    }
    crit <- (df$date >= from) & (df$date <= to)

  } else {
    # No filter → use all rows
    crit <- rep(TRUE, nrow(df))
  }

  keep <- if (filter_type == "inclusive") crit else !crit
  df_filtered <- df[keep, , drop = FALSE]

  # 3) Pull the target column (supports name or index); must be numeric ---------
  vals <- dplyr::pull(df_filtered, {{ val_col }})
  if (!is.numeric(vals)) {
    rlang::abort(
      "Selected `val_col` must be numeric.",
      class = "econ_value_summary_non_numeric"
    )
  }

  # 4) Handle empty filter + NA removal -----------------------------------------
  if (length(vals) == 0L) {
    if (!empty_ok) {
      rlang::warn(
        "Filter returned zero rows; returning NA.",
        class = "econ_value_summary_empty_filter"
      )
      return(NA_real_)
    }
  }
  if (isTRUE(na_rm)) vals <- vals[!is.na(vals)]

  # 5) Apply the function -------------------------------------------------------
  FUN <- tryCatch(match.fun(.fun),
                  error = function(e) rlang::abort("`.fun` must be a function or function name.",
                                                   class = "econ_value_summary_bad_fun",
                                                   parent = e))
  tryCatch(
    FUN(vals, ...),
    error = function(e) {
      rlang::abort("`.fun` failed when applied to the filtered vector.",
                   class = "econ_value_summary_fun_error",
                   parent = e)
    }
  )
}
