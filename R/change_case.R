#' Change case
#'
#' Randomly switch between title case and lowercase for
#' @param data input dataframe
#' @param cols set of columns to apply transformation to. If `NULL`
#' will apply to all columns. Default `NULL`.
#' @param messiness Percentage of values to change. Must be
#' between 0 and 1. Default 0.1.
#' @importFrom rlang .data
#' @return a dataframe the same size as the input data.
#' @export

change_case <- function(data,
                        cols = NULL,
                        messiness = 0.1) {
  if (messiness < 0 || messiness > 1) {
    stop("'messiness' must be between 0 and 1")
  }
  if (is.null(cols)) {
    output <- data |>
      dplyr::mutate(
        rs_col = stats::runif(nrow(data))
      ) |>
      dplyr::mutate(
        dplyr::across(
          dplyr::where(~ is.character(.x) | is.factor(.x)),
          ~ dplyr::case_when(
            .data$rs_col <= messiness / 2 ~ stringr::str_to_lower(.x),
            (.data$rs_col > messiness / 2 & .data$rs_col <= messiness) ~
              stringr::str_to_upper(.x),
            TRUE ~ .x
          )
        )
      ) |>
      dplyr::select(-.data$rs_col)
  } else {
    # check if all cols present in colnames
    if (!all((cols %in% colnames(data)))) {
      stop("All elements of 'cols' must be a column name in 'data'")
    } else {
      output <- data |>
        dplyr::mutate(
          rs_col = stats::runif(nrow(data))
        ) |>
        dplyr::mutate(
          dplyr::across(
            dplyr::all_of(cols) &
              dplyr::where(~ is.character(.x) | is.factor(.x)),
            ~ dplyr::case_when(
              .data$rs_col <= messiness / 2 ~ stringr::str_to_lower(.x),
              (.data$rs_col > messiness / 2 & .data$rs_col <= messiness) ~
                stringr::str_to_upper(.x),
              TRUE ~ .x
            )
          )
        ) |>
        dplyr::select(-.data$rs_col)
    }
  }
  return(output)
}
