#' Summarize metadata columns into filename tokens
#'
#' @param df_text Data frame of *_text columns.
#' @param na_token Token to use when a column is all NA.
#' @param warn_all_na If TRUE, warn when a column is all NA.
#' @return Character vector of tokens in column order.
#' @keywords internal
#' @noRd
make_metadata_vec <- function(df_text, na_token = "na", warn_all_na = TRUE) {
  stopifnot(is.data.frame(df_text))
  nms <- names(df_text)

  vapply(seq_along(df_text), function(i) {
    col <- df_text[[i]]
    col_chr <- as.character(col)
    vals <- unique(col_chr[!is.na(col_chr)])
    # Replace semicolons with spaces (avoid delimiter leaks in filenames)
    vals <- gsub(";", " ", vals, fixed = TRUE)

    token <- if (length(vals) == 0L) {
      if (warn_all_na) {
        rlang::warn(
          glue::glue("Metadata column '{nms[i]}' is all NA; using token '{na_token}'."),
          class = "econ_csv_meta_all_na"
        )
      }
      na_token
    } else if (length(vals) == 1L) {
      vals
    } else {
      aspect <- sub("_text$", "", nms[i], perl = TRUE)
      paste0(length(vals), "_", aspect)
    }

    token <- tolower(trim_ws(token))
    token
  }, character(1L), USE.NAMES = FALSE)
}
