# Datetime functions ------------------------------------------------------

#' Datetime sequence
#'
#' @param year integer, year of the datetime sequence
#' @param tzone character, time-zone of the datetime sequence
#' @param resolution_mins integer, interval of minutes between two consecutive datetime values
#' @param fullyear boolean, whether to return a full-year sequence
#' @param start_date Date, if `fullyear` is `FALSE` set a starting date. Ignored when `fullyear` is `TRUE`.
#' @param end_date Date, if `fullyear` is `FALSE` set a final date. Ignored when `fullyear` is `TRUE`.
#'
#' @return vector of datetime values
#' @export
#'
#' @importFrom lubridate as_datetime dmy force_tz minutes
#'
get_datetime_seq <- function(year, tzone, resolution_mins, fullyear = FALSE, start_date = NULL, end_date = NULL) {
  if (!fullyear & is.null(start_date) & is.null(end_date)) {
    message( "if start_date and end_date are not provided, fullyear must be TRUE" )
    return( NULL )
  }
  if (fullyear) {
    return(
      seq.POSIXt(
        from = force_tz(as_datetime(dmy(paste0("0101", year))), tzone = tzone),
        to = force_tz(as_datetime(dmy(paste0("0101", year+1))), tzone = tzone) - minutes(resolution_mins),
        by = paste(resolution_mins, "min")
      )
    )
  } else {
    if (is.null(start_date) | is.null(end_date)) {
      message( "both start_date and end_date must be provided")
      return( NULL )
    }
    return(
      seq.POSIXt(
        from = force_tz(as_datetime(start_date), tzone = tzone),
        to = force_tz(as_datetime(end_date), tzone = tzone) - minutes(resolution_mins),
        by = paste(resolution_mins, "min")
      )
    )
  }
}


#' Convert date to datetime with a timezone
#'
#' Only valid for positive time zone differences (right part pf the mapamundi)
#'
#' @param date date
#' @param tzone character, time zone
#'
#' @return datetime
#' @export
#'
#' @importFrom lubridate floor_date with_tz
#'
date_to_datetime_with_tz <- function(date, tzone="UTC") {
  # Not valid for negative tzones differences! (Left part of the mapamundi)
  floor_date(with_tz(date, tzone), 'day')
}


#' Interpolate `n` values between two numeric values
#'
#' @param y1 first value
#' @param y2 second value
#' @param n integer, number of intra-values (counting the original value as the first one)
#'
#' @importFrom dplyr tibble
#' @importFrom stats lm predict
#'
interpolation <- function(y1, y2, n) {
  predict(lm(y ~ x, tibble(x = c(1, (n+1)), y = c(y1, y2))), tibble(x=c(1:n)))
}

#' Increase numeric vector resolution
#'
#' @param y original numeric vector
#' @param n integer, number of intra-values (counting the original value as the first one)
#'
#' @return numeric vector
#' @export
#'
#' @importFrom dplyr tibble lead %>%
#' @importFrom purrr pmap simplify
#'
#' @details
#' if we have a vector v = c(1, 2), then `increase_numeric_resolution(v, 4)`
#' returns c(1, 1.25, 1.5, 1.75, 2)
increase_numeric_resolution <- function(y, n) {
  tibble(y1 = y, y2 = lead(y, default = 0)) %>%
    pmap(~ interpolation(..1, ..2, n)) %>%
    simplify() %>%
    as.double()
}

#' Increase datetime vector resolution
#'
#' @param y vector of datetime values
#' @param interval_mins integer, interval of minutes between two consecutive datetime values
#'
#' @return datetime vector
#' @export
#'
#' @importFrom lubridate minutes as_datetime tz
increase_datetime_resolution <- function(y, interval_mins) {
  seq.POSIXt(y[1], y[length(y)]+(y[2]-y[1])-minutes(interval_mins), by = paste(interval_mins, 'min')) %>% as_datetime(tz = tz(y))
}

#' Increase time resolution of a timeseries data frame
#'
#' @param df data.frame or tibble, first column of name `datetime` being of class datetime and rest of columns being numeric
#' @param n integer, number of intra-values (counting the original value as the first one)
#'
#' @return tibble
#' @export
#'
#' @importFrom dplyr tibble select_if %>%
#'
increase_timeseries_resolution <- function(df, n) {
  new_df <- tibble(datetime = increase_datetime_resolution(df[['datetime']], 60/n))
  numeric_df <- df %>% select_if(is.numeric)
  for (col in colnames(numeric_df)) {
    new_df[[col]] <- increase_numeric_resolution(numeric_df[[col]], n)
  }
  return( new_df )
}


#' Decrease time resolution of timeseries data frame
#'
#' @param df data.frame or tibble, first column of name `datetime` being of class datetime and rest of columns being numeric
#' @param resolution_mins integer, interval of minutes between two consecutive datetime values
#' @param value character being 'average' or 'first'
#'
#' @return tibble
#' @export
#'
#' @importFrom dplyr %>% mutate group_by summarise_all distinct
#' @importFrom lubridate floor_date
#' @importFrom rlang .data
#'
decrease_resolution <- function(df, resolution_mins, value = c('average', 'first')) {
  df2 <- df %>%
    mutate(datetime = floor_date(.data$datetime, paste(resolution_mins, 'minute')))
  if (value == 'average') {
    return(
      df2 %>%
        group_by(.data$datetime) %>%
        summarise_all(mean)
    )
  } else if (value == 'first') {
    return(
      df2 %>%
        distinct(.data$datetime, .keep_all = T)
    )
  } else {
    return( NULL )
  }
}


#' Week date from datetime value
#'
#' @param dttm datetime vector
#'
#' @return date vector
#' @export
#'
#' @importFrom lubridate as_date year week
get_week_from_datetime <- function(dttm) {
  as_date(paste(year(dttm), week(dttm), 1), format="%Y %W %u")
}

#' Week datetime from datetime value
#'
#' @param dttm datetime vector
#'
#' @return datetime vector
#' @export
#'
#' @importFrom lubridate as_datetime hours hour
#'
get_weektime_from_datetime <- function(dttm) {
  weeks <- get_week_from_datetime(dttm)
  as_datetime(weeks) + hours(hour(dttm))
}

#' Summarise dataframe with weekly total column values
#'
#' @param df data.frame or tibble, first column of name `datetime` being of class datetime and rest of columns being numeric
#'
#' @return tibble
#' @export
#'
#' @importFrom dplyr %>% mutate select group_by summarise_all
#' @importFrom rlang .data
#'
get_week_total <- function(df) {
  df %>%
    mutate(week = get_week_from_datetime(.data$datetime)) %>%
    select(-"datetime") %>%
    group_by(.data$week) %>%
    summarise_all(sum)
}


#' Aggregate multiple timeseries columns to a single one
#'
#' The first column `datetime` will be kept.
#'
#' @param df data.frame or tibble, first column of name `datetime` being of class datetime and rest of columns being numeric
#' @param varname character, name of the aggregation column
#'
#' @return tibble
#' @export
#'
aggregate_timeseries <- function(df, varname) {
  tbl <- df[1]
  tbl[[varname]] <- rowSums(df[-1])
  return( tbl )
}

