#' Change case
#'
#' Randomly switch between title case and lowercase for
#' character strings
#' @param data input dataframe
#' @param cols set of columns to apply transformation to. If `NULL`
#' will apply to all columns. Default `NULL`.
#' @param messiness Percentage of values to change. Must be
#' between 0 and 1. Default 0.1.
#' @param case_type Whether the case should change based on
#' the `"word"` or `"letter"`.
#' @importFrom rlang .data
#' @return a dataframe the same size as the input data.
#' @export

change_case <- function(data,
                        cols = NULL,
                        messiness = 0.1,
                        case_type = "word") {
  if (messiness < 0 || messiness > 1) {
    stop("'messiness' must be between 0 and 1")
  }
  if (!(case_type %in% c("word", "letter"))) {
    stop("'case_type' must be either 'word' or 'letter'")
  }

  if (case_type == "letter") {
    if (is.null(cols)) {
      output <- data |>
        dplyr::mutate(
          dplyr::across(
            dplyr::where(~ is.character(.x) | is.factor(.x)),
            ~ change_letter_case(.x, messiness = messiness)
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
              ~ change_letter_case(.x, messiness = messiness)
            )
          )
      }
    }
  } else {
    if (is.null(cols)) {
      output <- data |>
        dplyr::mutate(
          rs_col = stats::runif(nrow(data))
        ) |>
        dplyr::mutate(
          dplyr::across(
            dplyr::where(~ is.character(.x) | is.factor(.x)),
            ~ dplyr::case_when(
              .data$rs_col <= messiness / 2 ~ stringr::str_to_lower(.x),
              (.data$rs_col > messiness / 2 & .data$rs_col <= messiness) ~
                stringr::str_to_upper(.x),
              TRUE ~ .x
            )
          )
        ) |>
        dplyr::select(-.data$rs_col)
    } else {
      # check if all cols present in colnames
      if (!all((cols %in% colnames(data)))) {
        stop("All elements of 'cols' must be a column name in 'data'")
      } else {
        output <- data |>
          dplyr::mutate(
            rs_col = stats::runif(nrow(data))
          ) |>
          dplyr::mutate(
            dplyr::across(
              dplyr::all_of(cols) &
                dplyr::where(~ is.character(.x) | is.factor(.x)),
              ~ dplyr::case_when(
                .data$rs_col <= messiness / 2 ~ stringr::str_to_lower(.x),
                (.data$rs_col > messiness / 2 & .data$rs_col <= messiness) ~
                  stringr::str_to_upper(.x),
                TRUE ~ .x
              )
            )
          ) |>
          dplyr::select(-.data$rs_col)
      }
    }
  }
  return(output)
}


#' Function to change case of each individual letter
#'
#' @param x Character vector
#' @param messiness Percentage of values to change. Must be
#' between 0 and 1. Default 0.1.
#' @return Messy character vector
#' @noRd
change_letter_case <- function(x, messiness = 0.1) {
  # if factor, convert to character
  if (is.factor(x)) {
    x <- as.character(x)
  }

  change_letter_case_string <- function(s, ...) {
    # Convert to vector of characters
    chars <- strsplit(s, NULL)[[1]]

    # Randomly change the case of each character using sapply
    chars <- sapply(chars, function(char) {
      if (stats::runif(1) < messiness) {
        return(toupper(char))
      } else {
        return(tolower(char))
      }
    })

    # Reassemble the string
    return(paste(chars, collapse = ""))
  }

  x_messy <- sapply(x, change_letter_case_string, USE.NAMES = FALSE)
  return(x_messy)
}
