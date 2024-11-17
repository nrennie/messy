#' Make column names messy
#'
#' Adds special characters and randomly
#' capitalises characters in the column
#' names of a data frame.
#' @param data data.frame to alter column names
#' @return data.frame with messy column names
#' @export
#' @examples
#' messy_colnames(mtcars)
messy_colnames <- function(data) {
  # Assign the new column names to the dataframe
  names(data) <- special_chars(names(data))
  return(data)
}
