#' Validate econanalyzr tibble structure
#'
#' Enforces the econanalyzr schema:
#' - First 9 **required** columns with exact names and expected low-level type & primary class:
#'   `date (Date/double)`, `date_period_text (character)`, `value (numeric/double)`,
#'   `data_element_text (character)`, `data_measure_text (character)`,
#'   `date_measure_text (character)`, `data_transform_text (character)`,
#'   `geo_entity_type_text (character)`, `geo_entity_text (character)`.
#' - Any number of **optional** columns may appear after the first 9.
#' - The **final** column must be `viz_type_text` of type/class `character`.
#'
#' If all required columns and `viz_type_text` are present but out of order, the function
#' emits a warning and **reorders** the columns to: required 9 (in order) → optional(s) → `viz_type_text`.
#'
#' Special handling of `date`:
#' - Accepts base `Date` and subclasses. Subclasses are **coerced to plain `Date`** with a warning.
#' - Accepts `zoo::yearmon`/`zoo::yearqtr` (if `zoo` is installed) and **always coerces to `Date` at
#'   the *start of the period*** (first day of the month/quarter) with a warning.
#' - Accepts `POSIXct`/`POSIXlt` and **coerces to `Date`** (drop time) using `datetime_tz`.
#'
#' Special handling of `value`:
#' - Must be **double**; if numeric but not double (e.g., integer or other numeric class),
#'   the column is **coerced to double** with a warning. Non-numeric is an error.
#'
#' @param df A data frame or tibble to validate.
#' @param datetime_tz String timezone used when coercing `POSIXct/POSIXlt` to `Date`.
#'   Defaults to `"UTC"` for deterministic behavior across environments.
#'
#' @return The validated tibble (possibly reordered and/or with `date`/`value` coerced).
#'
#' @details
#' To support `yearmon`/`yearqtr`, add `zoo` to Suggests. If a `yearmon`/`yearqtr` column is
#' encountered and `zoo` is not installed, a classed error is raised with guidance.
#'
#' @importFrom tibble as_tibble
#' @importFrom rlang abort warn is_installed
#' @importFrom glue glue
#' @examples
#' # check_econanalyzr_df(valid_data)
check_econanalyzr_df <- function(df, datetime_tz = "UTC") {
  ## Must be a data.frame / tibble ------------------------------------------------
  if (!is.data.frame(df)) {
    rlang::abort(
      "Input must be a data.frame or tibble.",
      class = c("econanalyzr_validation_error", "econ_df_not_dataframe")
    )
  }
  # Do not auto-repair names; we explicitly check for duplicates below.
  df  <- tibble::as_tibble(df, .name_repair = "minimal")
  nms <- names(df)

  ## Duplicate names are not allowed ----------------------------------------------
  if (anyDuplicated(nms)) {
    dup_names <- unique(nms[duplicated(nms)])
    rlang::abort(
      glue::glue("Duplicate column names are not allowed: {paste(dup_names, collapse = ', ')}."),
      class = c("econanalyzr_validation_error", "econ_df_duplicate_names")
    )
  }

  ## Required schema (first 9) ----------------------------------------------------
  req_names <- c(
    "date", "date_period_text", "value", "data_element_text",
    "data_measure_text", "date_measure_text", "data_transform_text",
    "geo_entity_type_text", "geo_entity_text"
  )
  # Expected low-level typeof() and primary class()
  req_type  <- c("double", "character", "double", rep("character", 6))
  req_class <- c("Date",   "character", "numeric", rep("character", 6))

  ## At least 10 columns: 9 required + final viz_type_text ------------------------
  min_cols <- length(req_names) + 1L
  if (ncol(df) < min_cols) {
    rlang::abort(
      glue::glue(
        "Data has {ncol(df)} columns; at least {min_cols} required (9 required + final 'viz_type_text')."
      ),
      class = c("econanalyzr_validation_error", "econ_df_too_few_columns")
    )
  }

  ## Presence checks (names only) -------------------------------------------------
  missing_required <- setdiff(req_names, nms)
  if (length(missing_required) > 0L) {
    rlang::abort(
      glue::glue("Missing required columns: {paste(missing_required, collapse = ', ')}."),
      class = c("econanalyzr_validation_error", "econ_df_missing_required_columns")
    )
  }
  if (!("viz_type_text" %in% nms)) {
    rlang::abort(
      "Missing final column 'viz_type_text'.",
      class = c("econanalyzr_validation_error", "econ_df_missing_viz")
    )
  }

  ## Reorder if present-but-out-of-order -----------------------------------------
  # Expected order: required 9 (in spec order) -> optional(s) -> viz_type_text last
  idx_req <- match(req_names, nms)
  idx_viz <- match("viz_type_text", nms)
  idx_opt <- setdiff(seq_along(nms), c(idx_req, idx_viz))
  new_idx <- c(idx_req, idx_opt, idx_viz)

  if (!identical(seq_along(nms), new_idx)) {
    rlang::warn(
      "Columns present but out of expected order; reordering required columns first and moving 'viz_type_text' to last.",
      class = "econ_df_reordered_columns"
    )
    df  <- df[, new_idx, drop = FALSE]
    nms <- names(df)
  }

  ## Normalize 'date' BEFORE enforcing type/class ---------------------------------
  date_col <- df[["date"]]

  if (inherits(date_col, "Date")) {
    # Accept Date and subclasses; coerce subclasses to plain Date
    if (!identical(class(date_col), "Date")) {
      rlang::warn(
        "Column 'date' inherits from 'Date' but has extra classes; coercing to plain 'Date'.",
        class = "econ_df_date_subclass_coerced_to_Date"
      )
      df[["date"]] <- as.Date(date_col)
      class(df[["date"]]) <- "Date"
    }

  } else if (inherits(date_col, "yearmon") || inherits(date_col, "yearqtr")) {
    # Optional support via zoo: convert to Date at *start of period* (frac = 0)
    if (!rlang::is_installed("zoo")) {
      rlang::abort(
        "Column 'date' is a 'zoo' period (yearmon/yearqtr) but the 'zoo' package is not installed. Please install.packages('zoo').",
        class = c("econanalyzr_validation_error", "econ_df_missing_zoo")
      )
    }
    if (inherits(date_col, "yearmon")) {
      df[["date"]] <- zoo::as.Date(date_col, frac = 0)
      src <- "yearmon"
    } else { # yearqtr
      df[["date"]] <- zoo::as.Date(date_col, frac = 0)
      src <- "yearqtr"
    }
    rlang::warn(
      glue::glue("Column 'date' is {src}; coercing to 'Date' at the start of the period (first day)."),
      class = "econ_df_period_coerced_to_Date"
    )
    class(df[["date"]]) <- "Date"

  } else if (inherits(date_col, "POSIXct") || inherits(date_col, "POSIXlt")) {
    # Drop time-of-day deterministically via `datetime_tz`
    rlang::warn(
      glue::glue("Column 'date' is POSIXt; coercing to 'Date' using tz = '{datetime_tz}'."),
      class = "econ_df_datetime_coerced_to_Date"
    )
    df[["date"]] <- as.Date(date_col, tz = datetime_tz)

  } else {
    rlang::abort(
      "Column 'date' must be a 'Date' (or subclass), a 'zoo' period (yearmon/yearqtr), or a POSIXct/POSIXlt convertible to 'Date'.",
      class = c("econanalyzr_validation_error", "econ_df_bad_date_type")
    )
  }

  ## Special handling for 'value' — check the column directly ---------------------
  vcol <- df[["value"]]

  # If double but has extra/other classes, strip to plain numeric with a warning
  if (is.double(vcol) && !identical(class(vcol), "numeric")) {
    rlang::warn(
      "Column 'value' is double but has extra class tags; coercing to plain 'numeric'.",
      class = "econ_df_value_class_normalized"
    )
    df[["value"]] <- as.double(vcol)
  }

  if (!is.double(vcol)) {
    vcol <- df[["value"]]
    if (is.numeric(vcol) || inherits(vcol, "integer64")) {
      rlang::warn(
        "Column 'value' is numeric but not double; coercing to double.",
        class = "econ_df_value_coerced_to_double"
      )
      df[["value"]] <- as.double(vcol)
    } else {
      rlang::abort(
        "Column 'value' must be numeric (double preferred).",
        class = c("econanalyzr_validation_error", "econ_df_bad_required_types")
      )
    }
  }

  ## Validate typeof()/class() for the first 9 columns ----------------------------
  actual_type  <- vapply(req_names, function(nm) typeof(df[[nm]]),  character(1))
  actual_class <- vapply(req_names, function(nm) class(df[[nm]])[1], character(1))

  bad <- (actual_type != req_type) | (actual_class != req_class)
  if (any(bad)) {
    msgs <- paste0(
      sprintf("[%d] '%s'", which(bad), req_names[bad]),
      ": type=", actual_type[bad], " (exp ", req_type[bad], "); ",
      "class=", actual_class[bad], " (exp ", req_class[bad], ")"
    )
    rlang::abort(
      glue::glue(
        "Column type/class mismatches in first 9 required columns:\n{paste(msgs, collapse = '\n')}"
      ),
      class = c("econanalyzr_validation_error", "econ_df_bad_required_types")
    )
  }

  ## Final column must be 'viz_type_text' and character ---------------------------
  last_name <- names(df)[ncol(df)]
  if (!identical(last_name, "viz_type_text")) {
    rlang::abort(
      "The last column must be named 'viz_type_text'.",
      class = c("econanalyzr_validation_error", "econ_df_bad_last_name")
    )
  }
  last_type  <- typeof(df[[last_name]])
  last_class <- class(df[[last_name]])[1]
  if (!(identical(last_type, "character") && identical(last_class, "character"))) {
    rlang::abort(
      "Column 'viz_type_text' must have type and class 'character'.",
      class = c("econanalyzr_validation_error", "econ_df_bad_last_type")
    )
  }

  ## Return validated (and possibly reordered/coerced) tibble ---------------------
  return(df)
}
