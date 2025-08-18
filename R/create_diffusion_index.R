#' Calculate a diffusion index
#'
#' Computes a diffusion index using one of three common methodologies:
#'
#' - `"Federal Reserve"`: \eqn{(pct\_increased - pct\_decreased) * 100}
#' - `"IHS-PMI"`: \eqn{(pct\_increased + 0.5 * pct\_unchanged) * 100}
#' - `"Conference Board"`: encode each element of `pct_change` as 1 (up),
#'   0.5 (unchanged), or 0 (down) using a small threshold (default 0.05%), then
#'   return \eqn{100 \times \mathrm{mean}(\text{encoded}, na.rm=TRUE)}.
#'
#' @param pct_increased Numeric scalar or vector in \[0, 1\]: percent increased.
#' @param pct_decreased Numeric scalar or vector in \[0, 1\]: percent decreased.
#' @param pct_unchanged Numeric scalar or vector in \[0, 1\]: percent unchanged.
#' @param pct_change    Numeric scalar or vector (finite real; may be < 0 or > 1):
#'   used only for `"Conference Board"`.
#' @param idx_type      One of `"Federal Reserve"` (default), `"IHS-PMI"`, or `"Conference Board"`.
#'
#' @return
#'   - For `"Federal Reserve"` and `"IHS-PMI"`: a numeric vector the same length as the recycled inputs.
#'   - For `"Conference Board"`: a single numeric scalar (the encoded mean * 100).
#'
#' @export
#' @importFrom rlang abort warn
#' @importFrom glue glue
#' @importFrom vctrs vec_recycle_common
create_diffusion_index <- function(
    pct_increased = NULL,
    pct_decreased = NULL,
    pct_unchanged = NULL,
    pct_change    = NULL,
    idx_type      = c("Federal Reserve", "IHS-PMI", "Conference Board")
) {
  # Match the index type (scalar choice)
  idx_type <- match.arg(idx_type)

  # --- helpers ---------------------------------------------------------------

  # Validate a numeric vector:
  # - must be numeric
  # - must not contain Inf/-Inf/NaN (NA allowed depending on allow_na)
  # - enforce [0,1] bounds for proportions when bound01 = TRUE
  validate_vec <- function(x, name, bound01 = FALSE, allow_na = TRUE) {
    if (!is.numeric(x)) {
      rlang::abort(glue::glue("`{name}` must be numeric."), class = "create_diffusion_index_bad_type")
    }
    # Disallow Inf/-Inf/NaN anywhere
    if (any(is.infinite(x) | is.nan(x), na.rm = TRUE)) {
      rlang::abort(
        glue::glue("`{name}` must not contain Inf, -Inf, or NaN."),
        class = "create_diffusion_index_nonfinite"
      )
    }
    # NA policy
    if (!allow_na && any(is.na(x))) {
      rlang::abort(
        glue::glue("`{name}` must not contain NA values for this index type."),
        class = "create_diffusion_index_na_forbidden"
      )
    }
    # Bounds [0,1] for proportions
    if (bound01) {
      x_ok <- x[!is.na(x)]
      if (length(x_ok) && any(x_ok < 0 | x_ok > 1)) {
        rlang::abort(
          glue::glue("`{name}` values must lie within [0, 1]."),
          class = "create_diffusion_index_out_of_bounds"
        )
      }
    }
    invisible(x)
  }

  # Recycle two vectors (or scalars) to common length; map vctrs error to our class
  recycle2 <- function(a, b, a_name, b_name) {
    tryCatch(
      vctrs::vec_recycle_common(
        a = a,
        b = b
      ),
      error = function(cnd) {
        rlang::abort(
          glue::glue("`{a_name}` and `{b_name}` must be scalars or have a common length."),
          class  = "create_diffusion_index_bad_length",
          parent = cnd
        )
      }
    )
  }

  # constants
  threshold <- 5e-4  # 0.05% cutoff for "unchanged" band in Conference Board

  # --- methods ---------------------------------------------------------------

  if (idx_type == "Federal Reserve") {
    # Required inputs
    if (is.null(pct_increased) || is.null(pct_decreased)) {
      rlang::abort(
        "`pct_increased` and `pct_decreased` must be provided for `Federal Reserve`.",
        class = "create_diffusion_index_missing_args"
      )
    }
    # Unused inputs -> warn once (classed)
    if (!is.null(pct_unchanged) || !is.null(pct_change)) {
      rlang::warn(
        "Ignoring `pct_unchanged` and `pct_change` for `Federal Reserve`.",
        class = "create_diffusion_index_ignoring_args"
      )
    }

    # Validate and recycle
    validate_vec(pct_increased, "pct_increased", bound01 = TRUE, allow_na = TRUE)
    validate_vec(pct_decreased, "pct_decreased", bound01 = TRUE, allow_na = TRUE)
    rec <- recycle2(pct_increased, pct_decreased, "pct_increased", "pct_decreased")
    inc <- rec$a
    dec <- rec$b

    # Compute
    return((inc - dec) * 100)
  }

  if (idx_type == "IHS-PMI") {
    if (is.null(pct_increased) || is.null(pct_unchanged)) {
      rlang::abort(
        "`pct_increased` and `pct_unchanged` must be provided for `IHS-PMI`.",
        class = "create_diffusion_index_missing_args"
      )
    }
    if (!is.null(pct_decreased) || !is.null(pct_change)) {
      rlang::warn(
        "Ignoring `pct_decreased` and `pct_change` for `IHS-PMI`.",
        class = "create_diffusion_index_ignoring_args"
      )
    }

    validate_vec(pct_increased, "pct_increased", bound01 = TRUE, allow_na = TRUE)
    validate_vec(pct_unchanged, "pct_unchanged", bound01 = TRUE, allow_na = TRUE)
    rec <- recycle2(pct_increased, pct_unchanged, "pct_increased", "pct_unchanged")
    inc <- rec$a
    unc <- rec$b

    return((inc + 0.5 * unc) * 100)
  }

  # Conference Board: encode per-series change and average
  if (idx_type == "Conference Board") {
    if (is.null(pct_change)) {
      rlang::abort(
        "`pct_change` must be provided for `Conference Board`.",
        class = "create_diffusion_index_missing_args"
      )
    }
    if (!is.null(pct_increased) || !is.null(pct_decreased) || !is.null(pct_unchanged)) {
      rlang::warn(
        "Ignoring `pct_increased`, `pct_decreased`, and `pct_unchanged` for `Conference Board`.",
        class = "create_diffusion_index_ignoring_args"
      )
    }

    # Any finite real is allowed; NA allowed (we'll drop in mean)
    validate_vec(pct_change, "pct_change", bound01 = FALSE, allow_na = TRUE)

    # Encode: up (1), down (0), unchanged (0.5), NA stays NA
    encoded <- ifelse(
      pct_change >  threshold, 1,
      ifelse(pct_change < -threshold, 0, 0.5)
    )

    if (all(is.na(pct_change))) { # Average encoded values; if all are NA, return NA rather than NaN
      rlang::warn(
        "All encoded values are NA; returning NA_real_.",
        class = "create_diffusion_index_all_na"
      )
      return(NA_real_)
    } else if (any(is.na(pct_change))) { # If there are NAs, warn once (useful for pipelines/users)
      rlang::warn(
        "NA values found in `pct_change`; they will be dropped when averaging.",
        class = "create_diffusion_index_na_dropped"
      )
    }

    m <- mean(encoded, na.rm = TRUE)
    return(m * 100)
  }

  # Defensive: should never reach due to match.arg()
  rlang::abort("Unhandled `idx_type`.", class = "create_diffusion_index_internal")
}
