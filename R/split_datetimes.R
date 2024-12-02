
#' Splits date(time) column(s) into multiple columns
#'
#' These functions can split the "date" and "time" components of POSIXt columns
#' and the "hour", "month", and "day" components of Date columns into multiple
#' columns.
#'
#' @param data input dataframe
#' @param cols set of columns to apply transformation to. If `NULL` will apply
#'   to all POSIXt columns (for [split_datetimes()]) or Date columns (for
#'   [split_dates()]).
#' @param class For [split_datetimes()]. The desired output of the separate
#'   "date" and "time" columns. `"character"` leaves the columns as character
#'   vectors. `"date"` will reformat the date as a "Date" and the time as a
#'   "POSIXct" object, with a dummy date appended to it. In [split_dates()], all
#'   returned columns are integers.
#' @return a dataframe
#' @family Messy date(time) functions
#' @export
#'
#' @author Jack Davison
#'
#' @rdname split-dates
#' @order 1
#'
#' @examples
#' # split datetimes
#' data <- data.frame(today = Sys.time())
#' split_datetimes(data)

split_datetimes <-
  function(data,
           cols = NULL,
           class = c("character", "date")) {
    class <- match.arg(class)

    cols <- find_col_types(data, cols, "POSIXt")

    if (length(cols) == 0) {
      return(data)
    }

    split_datetime_helper <- function(data, col) {
      if (!(inherits(data[[col]], "POSIXt"))) {
        stop("The column '", col, "' is not a datetime object.")
      }

      old_date_name <- paste0(col, "_tochange")
      new_date_name <- paste0(col, "_date")
      new_time_name <- paste0(col, "_time")

      names(data)[names(data) == col] <- old_date_name

      data[[new_date_name]] <-
        format(data[[old_date_name]], format = "%Y/%m/%d")
      data[[new_time_name]] <-
        format(data[[old_date_name]], format = "%H:%M:%S")

      data <-
        dplyr::relocate(data, dplyr::all_of(c(new_date_name, new_time_name)), .after = dplyr::all_of(old_date_name))

      if (class == "date") {
        data[[new_date_name]] <- as.Date(data[[new_date_name]])
        data[[new_time_name]] <-
          as.POSIXct(paste("2000-01-01", data[[new_time_name]]))
      }

      data[[old_date_name]] <- NULL

      return(data)
    }

    for (i in cols) {
      data <- split_datetime_helper(data, i)
    }

    return(data)
  }

#' @rdname split-dates
#' @order 2
#' @export
#'
#' @examples
#' # split dates
#' data <- data.frame(today = Sys.Date())
#' data
#' split_dates(data)
#'
split_dates <- function(data, cols = NULL) {
  cols <- find_col_types(data, cols, "Date")

  if (length(cols) == 0) {
    return(data)
  }

  split_date_helper <- function(data, col) {
    if (!(inherits(data[[col]], "Date"))) {
      stop("The column '", col, "' is not a Date object.")
    }

    old_date_name <- paste0(col, "_tochange")
    new_year_name <- paste0(col, "_year")
    new_month_name <- paste0(col, "_month")
    new_day_name <- paste0(col, "_day")

    names(data)[names(data) == col] <- old_date_name

    data[[new_year_name]] <-
      as.integer(format(data[[old_date_name]], format = "%Y"))
    data[[new_month_name]] <-
      as.integer(format(data[[old_date_name]], format = "%m"))
    data[[new_day_name]] <-
      as.integer(format(data[[old_date_name]], format = "%d"))

    data <-
      dplyr::relocate(data, dplyr::all_of(c(
        new_year_name, new_month_name, new_day_name
      )), .after = dplyr::all_of(old_date_name))

    data[[old_date_name]] <- NULL

    return(data)
  }

  for (i in cols) {
    data <- split_date_helper(data, i)
  }

  return(data)
}
