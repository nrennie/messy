# messy (development version)

* Add `change_separators()` function

# messy 0.1.0

* CRAN release
* Add `duplicate_rows()` function
* Add date(time) messy-ing functions:
  - `messy_datetime_tzones()` will randomly set different timezones to datetime columns
  - `messy_datetime_formats()` and `messy_date_formats()` will format date(times) as characters, and scramble their strptime formats.
  - `split_datetimes()` and `split_dates()` will split datetime columns into "date" and "time" columns, and Date columns into "year", "month", and "day" columns.

# messy 0.0.2

* Add pkgdown site
* Add lintr file
* Add `messy_colnames()` function
* Add `messy_strings()` function

# messy 0.0.1

* Add `add_whitespaces()` function
* Add `make_missing()` function
* Add `change_case()` function
* Add initial `messy()` function
* Package initialisation

