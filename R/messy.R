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
#' @param sep_in A single value, or vector, or list of what is considered
#' a separator in the input data. Default `c("-", "_", "  ", " ")`.
#' @param sep_out A single value, or vector, or list of what the separators
#' may be randomly with. Default `c("-", "_", "  ", " ")`.
#' @return a dataframe the same size as the input data.
#' @export
#' @examples
#' messy(mtcars)
messy <- function(data,
                  messiness = 0.1,
                  missing = NA,
                  case_type = "word",
                  sep_in = c("-", "_", "  ", " "),
                  sep_out = c("-", "_", "  ", " ")) {
  output <- data |>
    add_special_chars(messiness = messiness) |>
    add_whitespace(messiness = messiness) |>
    make_missing(messiness = messiness, missing = missing) |>
    change_case(messiness = messiness, case_type = case_type) |>
    change_separators(messiness = messiness, sep_in = sep_in, sep_out = sep_out)
  return(output)
}
