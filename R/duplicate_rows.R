#' Duplicate rows and insert them into the dataframe in order or at random
#'
#' @param data input dataframe
#' @param messiness Percentage of rows to duplicate. Must be
#' between 0 and 1. Default 0.1.
#' @param shuffle Insert duplicated data underneath original data or insert randomly
#' @return A dataframe with duplicated rows inserted
#' @author Philip Leftwich
#' @export
#' @examples
#' duplicate_rows(mtcars, messiness = 0.1)

duplicate_rows <- function(data, messiness = 0.1, shuffle = FALSE) {
  if (messiness < 0 || messiness > 1) {
    stop("'messiness' must be between 0 and 1")
  }

  # Calculate the number of rows to duplicate
  num_rows_to_duplicate <- ceiling(nrow(data) * messiness)

  # Add an index column to preserve original order
  # Mark rows as originals
  data <- data |>
    dplyr::mutate(original_index = dplyr::row_number()) |>
    dplyr::mutate(is_duplicate = FALSE)

  # Duplicate rows according to messiness
  duplicated_rows <- data |>
    dplyr::slice_sample(n = num_rows_to_duplicate, replace = TRUE)

  # Add an identifier to distinguish duplicated rows
  duplicated_rows <- duplicated_rows |>
    dplyr::mutate(is_duplicate = TRUE)


  # Combine original and duplicated rows
  combined_data <- dplyr::bind_rows(data, duplicated_rows)

  # By default duplicated rows are added in the same order as original data
  if (shuffle == FALSE) {
    # Insert duplicated rows into the original dataframe
    final_data <- combined_data |>
      dplyr::arrange(.data$original_index)

    # Drop helper columns
    final_data <- final_data |>
      dplyr::select(-c(.data$original_index, .data$is_duplicate))
  } else {
    # if shuffle is TRUE then duplicated data is added randomly while the original data order is maintained
    # Assign a random index to the duplicated rows
    final_data <- combined_data |>
      dplyr::mutate(random_index = ifelse(
        .data$is_duplicate,
        sample(length(combined_data)),
        .data$original_index
      )) |>
      dplyr::arrange(.data$random_index)

    # Drop helper columns
    final_data <- final_data |>
      dplyr::select(-c(
        .data$original_index,
        .data$is_duplicate,
        .data$random_index
      ))
  }
  return(final_data)
}
