#' Splits a dataframe into two, such that it could be reassembled with a
#' mutating join
#'
#' This function takes an arbitrary number of 'joining' columns and any number
#' of additional column names and splits a dataframe in two such that a user
#' could then re-join using [merge()] or [dplyr::left_join()]. The user may find
#' it appropriate to go on and apply [messy()] to each new dataframe
#' independently to impede rejoining.
#'
#' Real data is often found across multiple datasets. For example, in
#' environmental monitoring, measurements at a monitoring station may need to be
#' bound with metadata about the station such as geographic coordinates, or even
#' meteorological data from an external source, to produce desired outputs. In
#' clinical research it may be necessary to combine the results of a clinical
#' trial with relevant patient information, such as weight or sex. This function
#' undoes existing joins to present learners with an authentic problem to solve;
#' joining two independent datasets to achieve some goal.
#'
#' @param data input dataframe
#' @param by a vector of column names which will be present in both outputs, to
#'   rejoin the dataframes
#' @param cols specific columns to be present in the 'right' dataframe.
#'   implicitly, all other columns not in 'cols' will be present in the 'left'
#'   dataframe.
#' @param distinct Apply [dplyr::distinct()] to `"both"` dataframes, the
#'   `"left"` or `"right"` dataframes, or `"none"` of the dataframes. This may
#'   be useful if one table is a 'lookup' or metadata table that has its values
#'   repeated many times in `data`.
#' @param names The names of the output list. If `NULL` the list will be
#'   unnamed.
#'
#' @returns A list of two dataframes
#'
#' @examples
#' dummy <-
#'   dplyr::tibble(
#'     patient_id = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
#'     test = c(1, 2, 3, 1, 2, 3, 1, 2, 3),
#'     result = c("++", "+", "-", "--", "+", "-", "+", "++", "-"),
#'     sex = c("M", "M", "M", "M", "M", "M", "F", "F", "F"),
#'     age = c(50, 50, 50, 25, 25, 25, 30, 30, 30)
#'   )
#'
#' unjoin(
#'   dummy,
#'   by = "patient_id",
#'   cols = c("sex", "age"),
#'   distinct = "right",
#'   names = c("tests", "patient_info")
#' )
#'
#' @author Jack Davison
#' @family data deconstructors
#' @export
unjoin <- function(data,
                   by,
                   cols,
                   distinct = "none",
                   names = c("left", "right")) {
  if (!any(cols %in% names(data))) {
    stop("Not all of 'cols' are in 'data' names.")
  }

  distinct <- match.arg(distinct, c("both", "right", "left", "none"))

  x_names <- c(by, names(data)[!names(data) %in% cols])
  y_names <- c(by, cols)

  x <- dplyr::select(data, dplyr::all_of(x_names))
  y <- dplyr::select(data, dplyr::all_of(y_names))

  if (distinct %in% c("both", "left")) {
    x <- dplyr::distinct(x)
  }
  if (distinct %in% c("both", "right")) {
    y <- dplyr::distinct(y)
  }

  out <- list(x, y)

  if (!is.null(names)) {
    if (length(names) != 2L) {
      stop("'names' should 'NULL' or a vector of length 2.")
    }
    out <- stats::setNames(out, names)
  }

  return(out)
}

#' Splits a dataframe row-wise or col-wise into any arbitrary number of
#' dataframes
#'
#' This function splits a dataframe into any number of dataframes such that they
#' can be rejoined by using [rbind()]/[dplyr::bind_rows()] for [unrbind()] or
#' [cbind()]/[dplyr::bind_cols()] for [uncbind()]. The user may find it
#' appropriate to go on and apply [messy()] to each new dataframe independently
#' to impede rejoining.
#'
#' Real data can often be found in disparate files. For example, data reports
#' may come in monthly and require row-binding together to obtain a complete
#' annual time series. Scientific results may arrive from different laboratories
#' and require binding together for further analysis and comparisons. This
#' function may simulate a single dataframe having come from different sources
#' and requiring binding back together. Base R's [split()] offers an alternative
#' to [unrbind()], but requires a pre-existing factor column to split by and
#' cannot as easily create random splits in the data.
#'
#' @inheritParams unjoin
#' @param sizes A vector of numeric inputs summing to `nrow(data)` for
#'   [unrbind()] or `ncol(data)` for [uncbind()]; the number of rows of each
#'   resulting dataframe. See `probs` for an alternative approach. If neither
#'   are provided, the dataframe will be split roughly in half.
#' @param probs A vector of numeric inputs summing to `1`; the proportion of
#'   rows/columns in each resulting dataframe. An alternative to `sizes`.
#' @param shuffle Shuffle rows in [unrbind()] or columns in [uncbind()]?
#'   Defaults to `TRUE`.
#'
#' @returns A list of dataframes
#'
#' @rdname unrbind
#' @order 1
#'
#' @author Jack Davison
#' @family data deconstructors
#' @export
#'
#' @examples
#' unrbind(dplyr::tibble(mtcars), probs = c(0.5, 0.3, 0.2))
#'
#' uncbind(dplyr::tibble(mtcars), probs = c(0.5, 0.3, 0.2))
unrbind <- function(data,
                    sizes = NULL,
                    probs = NULL,
                    names = NULL,
                    shuffle = TRUE) {
  if (is.null(sizes) & is.null(probs)) {
    half <- round(nrow(data) / 2)
    sizes <- c(half, half)
  } else if (is.null(sizes)) {
    if (sum(probs) != 1) {
      stop("'probs' must sum to 1")
    }
    # Convert probs to row counts
    sizes <- round(probs * nrow(data))
  } else {
    if (sum(sizes) != nrow(data)) {
      stop("'sizes' must sum to ", nrow(data))
    }
  }

  # Shuffle row indices
  if (shuffle) {
    shuffled_rows <- sample(nrow(data))
    data <- data[shuffled_rows, ]
  }

  # Adjust to ensure the total matches nrow(data) due to rounding issues
  diff <- nrow(data) - sum(sizes)
  if (diff != 0) {
    max_index <- which.max(sizes)  # Adjust the largest group
    sizes[max_index] <- sizes[max_index] + diff
  }

  # assign groups based on sizes
  groups <- rep(seq_along(sizes), times = sizes)

  # split dataframe
  split_data <- split(data, groups)

  # names
  if (!is.null(names)) {
    if (length(names) != length(split_data)) {
      stop("The number of names must equal the number of output dataframes")
    }
    split_data <- stats::setNames(split_data, names)
  } else {
    split_data <- unname(split_data)
  }

  # returns a list of dataframes
  return(split_data)
}

#' @rdname unrbind
#' @order 2
#' @export
uncbind <- function(data,
                    sizes = NULL,
                    probs = NULL,
                    names = NULL,
                    shuffle = TRUE) {
  if (is.null(sizes) & is.null(probs)) {
    half <- round(ncol(data) / 2)
    sizes <- c(half, half)
  } else if (is.null(sizes)) {
    if (sum(probs) != 1) {
      stop("'probs' must sum to 1")
    }
    # Convert probs to row counts
    sizes <- round(probs * ncol(data))
  } else {
    if (sum(sizes) != ncol(data)) {
      stop("'sizes' must sum to ", ncol(data))
    }
  }

  # Shuffle col indices
  if (shuffle) {
    shuffled_cols <- sample(ncol(data))
    data <- data[, shuffled_cols]
  }

  # Adjust to ensure the total matches ncol(data) due to rounding issues
  diff <- ncol(data) - sum(sizes)
  if (diff != 0) {
    min_index <- which.min(sizes)  # Adjust the largest group
    sizes[min_index] <- sizes[min_index] + diff
  }

  # assign groups based on sizes
  groups <- rep(seq_along(sizes), times = sizes)

  # split dataframe
  ends <- cumsum(sizes)
  starts <- c(1, ends + 1)[-(length(ends) + 1)]
  split_data <- lapply(seq_along(starts), function(x)
    data[, starts[x]:ends[x]])

  # names
  if (!is.null(names)) {
    if (length(names) != length(split_data)) {
      stop("The number of names must equal the number of output dataframes")
    }
    split_data <- stats::setNames(split_data, names)
  } else {
    split_data <- unname(split_data)
  }

  # returns a list of dataframes
  return(split_data)
}
