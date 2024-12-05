#' Alter separators
#'
#' Randomly alter the separators (spaces and underscores) in character strings
#' through duplication or replacement
#' @param data input dataframe
#' @param cols set of columns to apply transformation to. If `NULL`
#' will apply to all columns. Default `NULL`.
#' @param messiness Percentage of values to change. Must be
#' between 0 and 1. Default 0.1.
#' @return a dataframe the same size as the input data.
#' @export
#' @examples
#' players <- data.frame(
#' Player = c("Rey McSriff", "Kevin Nogilny", "Mike Sernandez", "Glenallen Mixon"),
#' Rating = c("B_3", "A_4", "C_2", "C_3"),
#'State = c("Arizona", "New Mexico", "North Carolina", "Texas"),
#'Season = c(2001L, 2002L, 2000L, 2002L)
#')
#' alter_separators(players)
alter_separators <- function(data, cols = NULL, messiness = 0.1) {
  if (messiness < 0 || messiness > 1) {
    stop("'messiness' must be between 0 and 1")
  }

  alter_sep <- function(x) {
    alterations <- list(
      function(s) gsub(" ", "  ", s),  # duplicate spaces
      function(s) gsub(" ", "_", s),   # replace spaces with underscores
      function(s) gsub("_", " ", s)    # replace underscores with spaces
    )

    n <- length(x)
    to_alter <- sample(n, size = round(n * messiness), replace = FALSE)

    x[to_alter] <- sapply(x[to_alter], function(s) {
      # enforce changes
      applicable_alterations <- alterations[!sapply(alterations, function(f) identical(f(s), s))]

      if (length(applicable_alterations) > 0) {
        # Sample one of the modifications
        chosen_alteration <- sample(applicable_alterations, 1)[[1]]
        return(chosen_alteration(s))
      } else {
        # If not applicable
        return(s)
      }
    })

    return(x)
  }

  if (is.null(cols)) {
    output <- data |>
      dplyr::mutate(dplyr::across(dplyr::where(is.character), \(x) alter_sep(x)))
  } else {
    # are cols present
    if (!all((cols %in% colnames(data)))) {
      stop("All elements of 'cols' must be a column name in 'data'")
    } else {
      output <- data |>
        dplyr::mutate(dplyr::across(dplyr::all_of(cols), \(x) alter_sep(x)))
    }
  }
  return(output)
}
