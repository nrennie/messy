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
#' @param case_type Whether the case should change based on
#' the `"word"` or `"letter"`.
#' @return a dataframe the same size as the input data.
#' @export
#' @examples
#' messy(mtcars)

messy <- function(data,
                  messiness = 0.1,
                  missing = NA,
                  case_type = "word") {
  output <- data |>
    add_special_chars(messiness = messiness) |>
    add_whitespace(messiness = messiness) |>
    make_missing(messiness = messiness, missing = missing) |>
    change_case(messiness = messiness, case_type = case_type)
  return(output)
}
