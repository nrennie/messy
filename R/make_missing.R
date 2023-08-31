#' Make missing
#'
#' Randomly make values missing in all data columns, or a
#' subset of columns
#' @param data input dataframe
#' @param cols set of columns to apply transformation to. If `NULL`
#' will apply to all columns. Default `NULL`.
#' @param messiness Percentage of values to change. Must be
#' between 0 and 1. Default 0.1.
#' @param missing A single value, vector, or list of what the
#' missing values will be replaced with. If length is greater
#' than 1, values will be replaced randomly.
#' Default `NA`.
#' @return a dataframe the same size as the input data.
#' @export

make_missing <- function(data,
                         cols = NULL,
                         messiness = 0.1,
                         missing = NA) {
  if (messiness < 0 || messiness > 1) {
    stop("'messiness' must be between 0 and 1")
  }
  if (is.null(cols)) {
    output <- data |>
      dplyr::mutate(
        dplyr::across(
          dplyr::everything(),
          ~ dplyr::case_when(
            runif(nrow(data)) <= messiness ~ unlist(sample(missing, 1)),
            TRUE ~ .x
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
            dplyr::all_of(cols),
            ~ dplyr::case_when(
              runif(nrow(data)) <= messiness ~ unlist(sample(missing, 1)),
              TRUE ~ .x
            )
          )
        )
    }
  }
  return(output)
}
