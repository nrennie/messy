


#' Make date(time) formats inconsistent
#'
#' Takes any date(times) column and transforms it into a character column,
#' sampling from any number of random of valid character representations.
#'
#' @param data input dataframe
#' @param cols set of columns to apply transformation to. If `NULL` will apply
#'   to all POSIXt columns (for [messy_datetime_formats()]) or Date columns (for
#'   [messy_date_formats()]).
#' @param formats A vector of any number of valid [strptime()] formats. Multiple
#'   formats will be sampled at random.
#'
#' @rdname messy-date-fmt
#' @order 1
#'
#' @family Messy date(time) functions
#' @author Jack Davison
#' @export
messy_datetime_formats <-
  function(data,
           cols = NULL,
           formats = c("%Y/%m/%d %H:%M:%S", "%d/%m/%Y %H:%M:%S")) {
    messy_datetime_format_helper(
      data = data,
      cols = cols,
      formats = formats,
      target = "POSIXt"
    )
  }

#' @rdname messy-date-fmt
#' @order 2
#' @export
messy_date_formats <-
  function(data,
           cols = NULL,
           formats = c("%Y/%m/%d", "%d/%m/%Y")) {
    messy_datetime_format_helper(
      data = data,
      cols = cols,
      formats = formats,
      target = "Date"
    )
  }

#' Change the timezone of datetime columns
#'
#' Takes any number of datetime columns and changes their timezones either
#' totally at random, or from a user-provided list of timezones.
#'
#' @param data input dataframe
#' @param cols set of columns to apply transformation to. If `NULL` will apply
#'   to all POSIXt columns.
#' @param tzones Valid time zones to sample from. By default samples from all
#'   [OlsonNames()], but can be set to options more relevant to the data.
#' @param force By default (`force = FALSE`) the datetimes will have their
#'   actual hour/minute values changed along with the timezones. If `force =
#'   TRUE`, which requires [lubridate][lubridate::force_tz()], the datetime values
#'   will remain the same and only the timezone will differ.
#'
#' @family Messy date(time) functions
#' @author Jack Davison
#' @export
messy_datetime_tzones <-
  function(data,
           cols = NULL,
           tzones = OlsonNames(),
           force = FALSE) {
    cols <- find_col_types(data, cols, target = "POSIXt")

    for (i in cols) {
      if (!(inherits(data[[i]], "POSIXt"))) {
        stop("The column '", i, "' is not a 'POSIXt' object.")
      }
      if (force) {
        rlang::check_installed("lubridate")
        tz <- sample(tzones, size = 1L)
        message("Setting '", i, "' to '", tz, "'")
        data[[i]] <-
          lubridate::force_tz(data[[i]], tzone = tz)
      } else {
        attr(data[[i]], "tzone") <- sample(tzones, size = 1L)
      }
    }

    return(data)
  }

#' Helper function to run the messy date formatters
#' @noRd
messy_datetime_format_helper <-
  function(data,
           cols,
           formats,
           target) {
    cols <- find_col_types(data, cols, target = target)

    formats <- sample(formats, size = nrow(data), replace = TRUE)

    for (i in cols) {
      if (!(inherits(data[[i]], target))) {
        stop("The column '", i, "' is not a '", target, "' object.")
      }
      data[[i]] <- format(data[[i]], format = formats)
    }

    return(data)
  }
