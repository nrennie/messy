#' Add special characters to strings

#' @param data input dataframe
#' @param cols set of columns to apply transformation to. If `NULL`
#' will apply to all columns. Default `NULL`.
#' @param messiness Percentage of values to change. Must be
#' between 0 and 1. Default 0.1.
#' @importFrom rlang .data
#' @return a dataframe the same size as the input data.
#' @export
#' @examples
#' add_special_chars(mtcars)

add_special_chars <- function(data,
                              cols = NULL,
                              messiness = 0.1) {
  if (messiness < 0 || messiness > 1) {
    stop("'messiness' must be between 0 and 1")
  }
  if (is.null(cols)) {
    output <- data |>
      dplyr::mutate(
        dplyr::across(
          dplyr::where(~ is.character(.x) | is.factor(.x)),
          ~ special_chars(.x, messiness = messiness)
        )
      )
  } else {
    # check if all cols present in colnames
    if (!all((cols %in% colnames(data)))) {
      stop("All elements of 'cols' must be a column name in 'data'")
    } else {
      output <- data |>
        dplyr::mutate(
          dplyr::across(
            dplyr::all_of(cols) &
              dplyr::where(~ is.character(.x) | is.factor(.x)),
            ~ special_chars(.x, messiness = messiness)
          )
        )
    }
  }
  return(output)
}

#' Function to make a character string messy
#'
#' Adds special characters and randomly
#' capitalises strings.
#' @param x Character vector
#' @param messiness Percentage of values to change. Must be
#' between 0 and 1. Default 0.1.
#' @return Messy character vector
#' @author Athanasia Monika Mowinckel
#' @noRd

special_chars <- function(x, messiness = 0.1) {
  # if factor, convert to character
  if (is.factor(x)) {
    x <- as.character(x)
  }

  special_chars_string <- function(s, ...) {
    # characters to insert
    random_chars <- c(
      "!", "@", "#", "$", "%", "^", "&",
      "*", "(", ")", "_", "+", "-", "."
    )

    # Convert to vector of characters
    chars <- strsplit(s, NULL)[[1]]

    # Randomly insert special characters using lapply
    chars <- Reduce(function(acc, char) {
      if (stats::runif(1) < messiness) {
        char_to_insert <- sample(random_chars, 1)
        return(c(acc, char_to_insert, char))
      } else {
        return(c(acc, char))
      }
    }, chars, init = character(0))

    # Reassemble the string
    return(paste(chars, collapse = ""))
  }

  x_messy <- sapply(x, special_chars_string, USE.NAMES = FALSE)
  return(x_messy)
}
