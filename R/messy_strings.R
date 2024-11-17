#' Make character vector messy
#'
#' Adds special characters and randomly
#' capitalises characters in the provided
#' character vector.
#'
#' @param x string vector to mess up
#' @return string vector that is messed up
#' @export
#' @examples
#' make_string_messy(c("Hello", "world"))
make_string_messy <- function(x) {
  sapply(x, messy_string, USE.NAMES = FALSE)
}

#' Function to make a string messy
#'
#' Adds special characters and randomly
#' capitalises strings.
#' @param s string to mess up
#' @return messy string
#' @importFrom stats runif
#' @noRd
messy_string <- function(s) {
  random_chars <- c("!", "@", "#", "$", "%", "^", "&", "*", "(", ")", "_", "+", "-", ".")

  # Convert to vector of characters
  chars <- strsplit(s, NULL)[[1]]

  # Randomly change the case of each character using sapply
  chars <- sapply(chars, function(char) {
    if (runif(1) < 0.5) {
      return(toupper(char))
    } else {
      return(tolower(char))
    }
  })

  # Randomly insert special characters using lapply
  chars <- Reduce(function(acc, char) {
    if (runif(1) < 0.2) {
      char_to_insert <- sample(random_chars, 1)
      return(c(acc, char_to_insert, char))
    } else {
      return(c(acc, char))
    }
  }, chars, init = character(0))

  # Reassemble the string
  return(paste(chars, collapse = ""))
}
