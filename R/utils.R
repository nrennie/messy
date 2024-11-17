#' Resample
#'
#' Resamples x of a specifc size
#' @param x either a vector of one or more elements from which to choose.
#' @return a vector of length size with elements drawn from either x
#' @noRd
resample <- function(x, ...) x[sample.int(length(x), ...)]
