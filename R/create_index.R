#' Create indexed values from a numeric vector
#'
#' Computes an index column from a numeric vector relative to a base element:
#' - `idx_type = 100`: classic base-100 index (base value -> 100, others scaled).
#' - `idx_type = 0`  : percent change from base (base value -> 0, others as % difference).
#'
#' If `num_vec` has names, `idx_pos` may be the **name** of the base element (character)
#' or a numeric (integer-ish) position. If `num_vec` is unnamed, `idx_pos` must be numeric.
#'
#' @param num_vec A numeric vector of length > 1.
#' @param idx_pos A base selector: either a 1-based (integer-ish) position, or—**only if**
#'   `num_vec` has names—a single character string matching one name.
#' @param idx_type Numeric scalar, either `100` (base-100 index) or `0` (percent change). Default `100`.
#'
#' @return A numeric vector of indexed values (same length as `num_vec`).
#' @export
#' @importFrom rlang abort warn
#' @importFrom glue glue
#' @importFrom vctrs vec_as_location
#'
#' @examples
#' # Basic usage (base-100)
#' create_index(c(50, 100, 150))
#'
#' # Percent change from base (base -> 0%)
#' create_index(c(50, 100, 150), idx_type = 0)
#'
#' # Using the second value as base (by position)
#' create_index(c(90, 100, 120), idx_pos = 2)
#'
#' # Named vector: select base by name (only works because vector has names)
#' x <- c(a = 80, b = 100, c = 120)
#' create_index(x, idx_pos = "b")        # base-100 index with "b" as base
#' create_index(x, idx_pos = "b", idx_type = 0)  # percent change from "b"
create_index <- function(num_vec, idx_pos = 1, idx_type = 100) {
  # Validate num_vec -----------------------------------------------------------
  if (!is.numeric(num_vec) || length(num_vec) < 2L) {
    rlang::abort(
      class   = "create_index_bad_num_vec",
      message = "`num_vec` must be a numeric vector of length > 1."
    )
  }

  # Detect usable names (for optional name-based lookup) -----------------------
  nm <- names(num_vec)
  has_names <- !is.null(nm) && any(!is.na(nm)) && any(nzchar(nm))

  # Resolve idx_pos to a single base index ------------------------------------
  base_idx <- NULL

  # Case A: numeric position (prefer direct resolution after guarding)
  if (is.numeric(idx_pos) && length(idx_pos) == 1L) {
    if (!is.finite(idx_pos) || idx_pos %% 1 != 0 || idx_pos < 1 || idx_pos > length(num_vec)) {
      rlang::abort(
        "`idx_pos` must be a single 1-based whole number within 1..length(num_vec).",
        class = "create_index_bad_idx_pos"
      )
    }
    base_idx <- as.integer(idx_pos)
  }

  # Case B: character name (only if names exist)
  if (is.null(base_idx) && has_names && is.character(idx_pos) && length(idx_pos) == 1L) {
    matches <- which(!is.na(nm) & nzchar(nm) & nm == idx_pos)
    if (length(matches) == 0L) {
      rlang::abort(glue::glue("`idx_pos` ('{idx_pos}') is not an existing name in names(num_vec)."),
                   class = "create_index_bad_idx_pos")
    }
    if (length(matches) > 1L) {
      rlang::abort(
        glue::glue("`idx_pos` ('{idx_pos}') matches multiple elements ({length(matches)} duplicates); please disambiguate or use a numeric position."),
        class = "create_index_ambiguous_idx_pos"
      )
    }
    base_idx <- matches
  }

  # Case C: fallback (rare types; keep vec_as_location as a safety net)
  if (is.null(base_idx)) {
    base_idx <- tryCatch(
      vctrs::vec_as_location(idx_pos, n = length(num_vec), names = if (has_names) nm else NULL),
      error = function(cnd) {
        rlang::abort(
          if (has_names)
            glue::glue("`idx_pos` must be a single 1-based index (1..{length(num_vec)}) or a single existing name.")
          else
            glue::glue("`idx_pos` must be a single 1-based index within 1..{length(num_vec)} (vector is unnamed)."),
          class  = "create_index_bad_idx_pos",
          parent = cnd
        )
      }
    )
    if (length(base_idx) == 0L) {
      rlang::abort("`idx_pos` must resolve to exactly one element (none selected).",
                   class = "create_index_bad_idx_pos")
    }
    if (length(base_idx) > 1L) {
      rlang::abort("`idx_pos` must resolve to a single element (no duplicates or multiple positions).",
                   class = "create_index_ambiguous_idx_pos")
    }
  }

  # Validate idx_type ----------------------------------------------------------
  if (!is.numeric(idx_type) || length(idx_type) != 1L || !(idx_type %in% c(100, 0))) {
    rlang::abort(
      class   = "create_index_bad_idx_type",
      message = "`idx_type` must be a numeric scalar equal to 100 (base-100) or 0 (percent change)."
    )
  }

  # Select base value and validate it for division -----------------------------
  base_value <- num_vec[base_idx]
  # Invalid denominators: NA, NaN, Inf/-Inf, or zero
  bad_base <- is.na(base_value) || is.nan(base_value) || is.infinite(base_value) || base_value == 0
  if (bad_base) {
    rlang::abort(
      class   = "create_index_bad_base_value",
      message = glue::glue(
        "Base value at idx_pos = {base_idx} is invalid for indexing (0, NA, NaN, or Inf)."
      )
    )
  }

  # Warn if other elements are non-finite (they will propagate) ----------------
  mask_other <- (seq_along(num_vec) != base_idx)
  if (any(is.na(num_vec[mask_other]) | is.nan(num_vec[mask_other]) | is.infinite(num_vec[mask_other]))) {
    rlang::warn(
      class   = "create_index_nonfinite_values",
      message = "Some non-base values are NA/NaN/Inf; resulting index will contain NA/NaN at those positions."
    )
  }

  # Compute index --------------------------------------------------------------
  ratio <- num_vec / base_value
  index <- if (idx_type == 100) {
    ratio * 100
  } else {
    (ratio - 1) * 100
  }

  # Return ---------------------------------------------------------------------
  return(index)
}
