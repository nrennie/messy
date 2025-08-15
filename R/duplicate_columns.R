#' Duplicate columns and insert them into the dataframe at random
#'
#' @param data input dataframe
#' @param messiness Probability that each column is duplicated. Must be
#' between 0 and 1. Default 0.1.
#' @param random Whether duplicated column names should be randomly selected
#' from other column names, or maintain the original. Default `TRUE`.
#' @param name_sep Separator to use for adding numbers to end of names. Default `""`.
#' @return A dataframe with duplicated rows inserted
#' @author Jordi Rosell
#' @export
#' @examples
#' duplicate_columns(mtcars, messiness = 0.1)

duplicate_columns <- function(
    data,
    messiness = 0.1,
    random = TRUE,
    name_sep = "") {
  if (messiness < 0 || messiness > 1) {
    stop("'messiness' must be between 0 and 1")
  }
  if (!is.logical(random)) {
    stop("'random' must be either 'TRUE' or 'FALSE'")
  }

  original_names <- colnames(data)
  n <- ncol(data)
  for (i in seq_len(n)) {
    if (stats::runif(1) < messiness) {
      if (random) {
        new_col_name <- sample(original_names, 1)
      } else {
        new_col_name <- original_names[i]
      }
      new_col_name <- paste0(new_col_name, name_sep, sample(100 * n, 1))
      data[[new_col_name]] <- data[[i]]
    }
  }

  return(data)
}
