#' Validate econanalyzr tibble structure
#'
#' This function checks whether the input is a valid econanalyzr data frame/tibble:
#' - It must be a data.frame or tibble.
#' - It must contain exactly the first 9 required columns with specific types and classes in order.
#' - It may contain optional columns between required columns and the final one.
#' - The final column must be `viz_type_text` with type and class `character`.
#'
#' @param df A data frame or tibble to validate
#'
#' @return A validated tibble
#'
#' @importFrom tibble tibble as_tibble
#' @importFrom purrr map_chr
#' @importFrom dplyr mutate filter pull
#' @importFrom rlang abort
#' @importFrom glue glue
#'
#' @examples
#' # check_econanalyzr_df(valid_data)
check_econanalyzr_df <- function(df) {
  if (!base::is.data.frame(df)) {
    rlang::abort("Input must be a data.frame or tibble.",
                 class = "econanalyzr_validation_error")
  }

  df <- tibble::as_tibble(df)

  # Required column specs
  required_spec <- tibble::tibble(
    col = c("date", "date_period_text", "value", "data_element_text",
            "data_measure_text", "date_measure_text", "data_transform_text",
            "geo_entity_type_text", "geo_entity_text"),
    type = c("double", "character", "double", rep("character", 6)),
    class = c("Date", "character", "numeric", rep("character", 6))
  )

  # Check names of the first 9 columns
  actual_first <- base::names(df)[1:9]
  if (!base::identical(actual_first, required_spec$col)) {
    rlang::abort(
      message = glue::glue(
        "First 9 columns must be named (and in order): {paste(required_spec$col, collapse = ', ')}."
      ),
      class = "econanalyzr_validation_error"
    )
  }

  # Validate types and classes of the first 9 columns
  issues <- required_spec |>
    dplyr::mutate(
      actual_type = purrr::map_chr(.data$col, ~ base::typeof(df[[.x]])),
      actual_class = purrr::map_chr(.data$col, ~ base::class(df[[.x]])[1]),
      type_ok = .data$actual_type == .data$type,
      class_ok = .data$actual_class == .data$class
    ) |>
    dplyr::filter(!.data$type_ok | !.data$class_ok)

  if (nrow(issues) > 0) {
    msg <- issues |>
      dplyr::mutate(msg = glue::glue(
        "'{.data$col}' must be type '{.data$type}' and class '{.data$class}', but got type '{.data$actual_type}' and class '{.data$actual_class}'."
      )) |>
      dplyr::pull(.data$msg) |>
      base::paste(collapse = "\n")

    rlang::abort(
      message = msg,
      class = "econanalyzr_validation_error"
    )
  }

  # Check last column
  last_col <- base::names(df)[base::ncol(df)]
  if (last_col != "viz_type_text") {
    rlang::abort(
      message = "The last column must be named 'viz_type_text'.",
      class = "econanalyzr_validation_error"
    )
  }

  if (!(base::typeof(df[[last_col]]) == "character" &&
        base::class(df[[last_col]])[1] == "character")) {
    rlang::abort(
      message = "Column 'viz_type_text' must have type and class 'character'.",
      class = "econanalyzr_validation_error"
    )
  }

  return(df)
}
