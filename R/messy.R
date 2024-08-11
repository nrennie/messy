#' Messy
#'
#' Make a data frame messier.
#' @param data input dataframe
#' @param messiness Percentage of values to change per function. Must be
#' between 0 and 1. Default 0.1.
#' @param missing A single value, vector, or list of what the
#' missing values will be replaced with. If length is greater
#' than 1, values will be replaced randomly.
#' Default `NA`.
#' @return a dataframe the same size as the input data.
#' @export


messy <- function(data,
                  messiness = 0.1,
                  missing = NA) {
  output <- data |>
    add_whitespace(messiness = messiness) |>
    make_missing(messiness = messiness, missing = missing) |>
    change_case(messiness = messiness)
  return(output)
}
