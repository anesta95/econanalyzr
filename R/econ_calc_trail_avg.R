#' Append trailing-average rows in long form to econanalyzr data frame
#'
#' Computes a trailing (right-aligned) moving average of `value` and **appends**
#' those rows to the original data (long form).
#'
#' - Input must pass [check_econanalyzr_df()].
#' - `trail_amount` must be a positive, integer-ish scalar (e.g., 3L, 6).
#' - If `df` is a **grouped** tibble (via `dplyr::group_by()`), the trailing
#'   average is computed **within each group**. If ungrouped, it is computed
#'   over the entire table.
#' - Derived rows have `data_transform_text` appended with `"; Trail {n}"`.
#' - Windows are **right-aligned** with `.complete = TRUE`: the first
#'   `trail_amount - 1` rows per group are `NA`.
#'
#' @param df An econanalyzr-valid tibble/data frame.
#' @param trail_amount Positive integer-like window size.
#'
#' @return A tibble containing the original rows **and** the trailing-average rows.
#'
#' @export
#' @importFrom rlang abort is_integerish .data
#' @importFrom dplyr arrange mutate bind_rows ungroup group_vars is_grouped_df group_by across all_of
#' @importFrom slider slide_dbl
econ_calc_trail_avg <- function(df, trail_amount) {
  # Preserve incoming grouping vars (if any) before validation,
  # since check_econanalyzr_df() returns a plain tibble.
  was_grouped <- dplyr::is_grouped_df(df)
  gvars <- if (was_grouped) dplyr::group_vars(df) else character(0)

  # Validate econ schema (ensures required columns and types)
  df <- check_econanalyzr_df(df)

  # Re-apply groups (if any and still present)
  if (length(gvars)) {
    missing <- setdiff(gvars, names(df))
    if (length(missing) == 0L) {
      df <- dplyr::group_by(df, dplyr::across(dplyr::all_of(gvars)))
    } else {
      # If validation reordered/renamed columns (shouldn't rename), just skip groups silently
      # or you could warn if you prefer:
      # rlang::warn(glue::glue("Skipping re-grouping; missing columns: {paste(missing, collapse = ', ')}"))
    }
  }

  # Validate trail_amount
  if (!rlang::is_integerish(trail_amount, n = 1, finite = TRUE) || trail_amount <= 0) {
    rlang::abort(
      "`trail_amount` must be a positive integer-like scalar (e.g., 3).",
      class = "econ_calc_trail_avg_bad_trail_amount"
    )
  }
  trail_amount <- as.integer(trail_amount)

  # Compute trailing mean in ascending date; if grouped, arrange within groups
  trailing_rows <- df |>
    dplyr::arrange(.data$date, .by_group = TRUE) |>
    dplyr::mutate(
      value = slider::slide_dbl(
        .x        = .data$value,
        .f        = ~ mean(.x),           # any NA in window -> NA (standard trailing-avg behavior)
        .before   = trail_amount - 1L,    # right-aligned trailing window
        .complete = TRUE                  # incomplete windows yield NA
      ),
      data_transform_text = paste0(.data$data_transform_text, "; Trail ", trail_amount)
    )

  # Long-form: original rows + trailing-average rows; drop groups to avoid surprises
  dplyr::bind_rows(df, trailing_rows) |> dplyr::ungroup()
}
