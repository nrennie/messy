#' Change separators
#'
#' Randomly change the separators in character strings
#' through random replacement
#' @param data input dataframe
#' @param cols set of columns to apply transformation to. If `NULL`
#' will apply to all columns. Default `NULL`.
#' @param messiness Percentage of values to change. Must be
#' between 0 and 1. Default 0.1.
#' @param sep_in A single value, or vector, or list of what is considered
#' a separator in the input data. Default `c("-", "_", "  ", " ")`.
#' @param sep_out A single value, or vector, or list of what the separators
#' may be randomly with. Default `c("-", "_", "  ", " ")`.
#' @return a dataframe the same size as the input data.
#' @export
#' @examples
#' change_separators(mtcars)
change_separators <- function(data,
                              cols = NULL,
                              messiness = 0.1,
                              sep_in = c("-", "_", "  ", " "),
                              sep_out = c("-", "_", "  ", " ")) {
  if (messiness < 0 || messiness > 1) {
    stop("'messiness' must be between 0 and 1")
  }

  if (is.null(cols)) {
    output <- data |>
      dplyr::mutate(
        dplyr::across(
          dplyr::where(is.character),
          \(x) change_sep(x,
            messiness = messiness,
            sep_in = sep_in,
            sep_out = sep_out
          )
        )
      )
  } else {
    # are cols present
    if (!all((cols %in% colnames(data)))) {
      stop("All elements of 'cols' must be a column name in 'data'")
    } else {
      output <- data |>
        dplyr::mutate(
          dplyr::across(
            dplyr::all_of(cols),
            \(x) change_sep(x,
              messiness = messiness,
              sep_in = sep_in,
              sep_out = sep_out
            )
          )
        )
    }
  }
  return(output)
}


#' Function to change separators
#'
#' @param x Character vector
#' @param messiness Percentage of values to change. Must be
#' between 0 and 1. Default 0.1.
#' @param sep_in A single value, or vector, or list of what is considered
#' a separator in the input data. Default `c("-", "_", "  ", " ")`.
#' @param sep_out A single value, or vector, or list of what the separators
#' may be randomly with. Default `c("-", "_", "  ", " ")`.
#' @return Messy character vector
#' @noRd
change_sep <- function(x,
                       messiness = 0.1,
                       sep_in = c("-", "_", "  ", " "),
                       sep_out = c("-", "_", "  ", " ")) {
  sep_in_escaped <- stringr::str_escape(sep_in)[order(nchar(sep_in), decreasing = TRUE)]
  pattern <- paste0("(", paste(sep_in_escaped, collapse = "|"), ")")
  replace_match <- function(match) {
    if (stats::runif(1) < messiness) sample(sep_out, 1) else match
  }
  stringr::str_replace_all(x, pattern, replace_match)
}
