#' Duplicate rows and insert them into the dataframe in order or at random. May result in numbers being added to the end of row names.
#'
#' @param data input dataframe
#' @param messiness Percentage of rows to duplicate. Must be
#' between 0 and 1. Default 0.1.
#' @param shuffle Insert duplicated data underneath original data or insert randomly
#' @return A dataframe with duplicated rows inserted
#' @author Philip Leftwich, Barry Rowlingson
#' @export
#' @examples
#' duplicate_rows(mtcars, messiness = 0.1)
duplicate_rows <- function(data, messiness = 0.1, shuffle = FALSE) {
  if (messiness < 0 || messiness > 1) {
    stop("'messiness' must be between 0 and 1")
  }
  nr <- nrow(data)
  ndups <- ceiling(nr * messiness)
  dups <- sort(sample.int(nr, ndups, replace = TRUE))

  data <- rbind(data, data[dups, , drop = FALSE])
  if (shuffle) {
    dups <- sample.int(nr, ndups)
  }
  final_data <- data[order(c(1:nr, dups)), , drop = FALSE]
  return(final_data)
}
