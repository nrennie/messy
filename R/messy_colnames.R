#' Make column names messy
#'
#' Adds special characters and randomly
#' capitalises characters in the column
#' names of a data frame.
#' @param data data.frame to alter column names
#' @param messiness Percentage of values to change per function. Must be
#' between 0 and 1. Default 0.1.
#' @return data.frame with messy column names
#' @export
#' @examples
#' messy_colnames(mtcars)
messy_colnames <- function(data, messiness = 0.2) {
  new_names <- data.frame(x = names(data)) |>
    add_special_chars(messiness = messiness) |>
    change_case(messiness = messiness) |>
    add_whitespace(messiness = messiness) |>
    dplyr::pull(.data$x)
  # Assign the new column names to the dataframe
  names(data) <- new_names
  return(data)
}
