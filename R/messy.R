#' Messy
#'
#' Make a data frame messier.
#' @param data input dataframe
#' @param umessiness Percentage of values to change per function. Must be
#' between 0 and 1. Default 0.1.
#' @return a dataframe the same size as the input data.
#' @export


messy <- function(data, messiness = 0.1) {
  output <- data |>
    add_whitespace(messiness = messiness)
  return(output)
}
