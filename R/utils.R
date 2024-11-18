#' Resample
#'
#' Resamples x of a specifc size
#' @param x either a vector of one or more elements from which to choose.
#' @return a vector of length size with elements drawn from either x
#' @noRd
resample <- function(x, ...) x[sample.int(length(x), ...)]

#' Loop through dataframe names to find columns of specific class
#' @param data,cols Inherited from parent
#' @param target Desired class (e.g., "Date")
#' @noRd
find_col_types <- function(data, cols, target) {
  if (is.null(cols)) {
    types <- lapply(data, class)
    cols <- c()
    for (i in names(types)) {
      if (target %in% types[[i]]) {
        cols <- c(cols, i)
      }
    }
  }
  return(cols)
}
