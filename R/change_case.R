#' Change case
#'
#' Randomly switch between title case and lowercase for
#' @param data input dataframe
#' @param cols set of columns to apply transformation to. If `NULL`
#' will apply to all columns. Default `NULL`.
#' @param messiness Percentage of values to change. Must be
#' between 0 and 1. Default 0.1.
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
        dplyr::across(
          dplyr::where(is.character),
          ~ dplyr::case_when(
            runif(nrow(data)) <= messiness ~ stringr::str_to_lower(.x),
            TRUE ~ stringr::str_to_title(.x)
          )
        )
      )
  } else {
    # check if all cols present in colnames
    if (!all((cols %in% colnames(data)))) {
      stop("All elements of 'cols' must be a column name in 'data'")
    } else {
      output <- data |>
        dplyr::mutate(
          dplyr::across(
            dplyr::all_of(cols) & dplyr::where(is.character),
            ~ dplyr::case_when(
              runif(nrow(data)) <= messiness ~ stringr::str_to_lower(.x),
              TRUE ~ stringr::str_to_title(.x)
            )
          )
        )
    }
  }
  return(output)
}
