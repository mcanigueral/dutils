# Datetime functions ------------------------------------------------------

#' Convert date or datetime value to timestamp number
#'
#' @param date date or datetime value
#' @param tzone character, time-zone of the current time
#' @param milliseconds logical, whether the timestamp is in milliseconds or seconds
#'
#' @return numeric
#' @export
#'
#' @importFrom lubridate force_tz as_datetime
#'
date_to_timestamp <- function(date, tzone = "Europe/Paris", milliseconds = T) {
  timestamp <- as.integer(
    force_tz(
      as_datetime(date, tz = "UTC"),
      tzone
    )
  )
  if (milliseconds) {
    return ( timestamp*1000 )
  } else {
    return( timestamp )
  }
}


#' Split the period between start and end dates with a defined interval of days
#'
#' The dates are converted to timestamp with function `date_to_timestamp`,
#' but the inputs `start_date` and `end_date` must be of class `Date` and not `datetime`.
#'
#' @param start_date Date, start date.
#' @param end_date Date, end date
#' @param tzone character, time zone of the timeseries data
#' @param interval_days integer, number of days of each query interval
#' @param milliseconds logical, whether the timestamp variable is in milliseconds or not
#'
#' @return tibble
#' @export
#'
#' @importFrom dplyr tibble select %>%
#' @importFrom rlang .data
#' @importFrom lubridate days as_datetime
#'
adapt_date_range <- function(start_date, end_date, tzone = "Europe/Paris", interval_days = 30, milliseconds = T) {
  if (as.integer(end_date - start_date, units = 'days') > interval_days) {
    tibble(
      start.date = seq.Date(start_date, end_date, by = paste(interval_days, 'days')),
      end.date = .data$start.date + days(interval_days),
      start.timestamp = date_to_timestamp(.data$start.date, tzone, milliseconds),
      end.timestamp = date_to_timestamp(.data$end.date, tzone, milliseconds)
    ) %>%
      select(.data$start.timestamp, .data$end.timestamp)
  } else {
    tibble(
      start.timestamp = date_to_timestamp(start_date, tzone, milliseconds),
      end.timestamp = date_to_timestamp(end_date, tzone, milliseconds)
    )
  }
}

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


#' Return the time resolution of a datetime sequence
#'
#' @param dttm_seq datetime sequence
#' @param units character being one of "auto", "secs", "mins", "hours", "days" and "weeks"
#'
#' @return numeric
#' @export
#'
get_time_resolution <- function(dttm_seq, units = 'mins') {
  difftime(dttm_seq[2], dttm_seq[1], units = units)[[1]]
}


#' Adapt the timezone of a time series dataframe
#'
#' @param df tibble with first column being `datetime`
#' @param tz_in character, time zone of the parameter `df`
#' @param tz_out character, time zone of the desired df
#'
#' @return tibble
#' @export
#'
#' @importFrom lubridate year with_tz year<-
#' @importFrom dplyr %>% mutate filter tibble left_join bind_rows arrange
#' @importFrom rlang .data
#' @importFrom tidyr drop_na
#'
adapt_df_timezone <- function(df, tz_in="UTC", tz_out="Europe/Amsterdam") {
  df_year <- unique(year(df$datetime))
  datetime_seq_out <- get_datetime_seq(df_year, tz_out, fullyear = T, resolution_mins = 60)

  df_tz_out <- df %>% mutate(datetime = with_tz(.data$datetime, tz_out))
  df_tz_out_year_shift <- df_tz_out %>%
    filter(year(.data$datetime) != df_year)
  year(df_tz_out_year_shift$datetime) <- df_year

  df_out <- tibble(
    datetime = datetime_seq_out
  ) %>%
    left_join(
      df_tz_out, by = 'datetime'
    ) %>%
    drop_na() %>%
    bind_rows(df_tz_out_year_shift) %>%
    arrange(.data$datetime)

  return( df_out )
}


# Preprocessing ----------------------------------------------------------

#' Fill from past values
#'
#' If back index ( NA index - `back`) is lower than zero then the it is filled with the first value of the data frame.
#' If the value in the back index is also NA, it iterates backwards until finding a non-NA value.
#'
#' @param tbl tibble or data.frame, with NA values in some columns
#' @param colnames character or vector of characters, column names with NA values
#' @param back integer, number of indices (rows) to go back and get the filling value
#'
#' @return tibble or data.frame
#' @export
#'
fill_from_past <- function(tbl, colnames, back=24) {
  tbl_to_fill <- tbl[colnames]
  for (col in colnames) {
    na_idx <- which(is.na(tbl_to_fill[col]))
    for (idx in na_idx) {
      back_idx <- idx
      # if (back_idx <= 0) back_idx <- 1
      while (is.na(tbl_to_fill[back_idx, col])) {
        back_idx <- back_idx - back
        if (back_idx <= 0) {
          back_idx <- 1
          break
        }
      }
      new_value <- tbl_to_fill[back_idx, col]
      if (is.na(new_value)) {
        message(paste("Could not find numeric values in the past for column", col, "and index", idx))
      }
      tbl_to_fill[idx, col] <- tbl_to_fill[back_idx, col]
    }
  }
  tbl[colnames] <- tbl_to_fill
  return( tbl )
}


#' Fill NA values of a datetime sequence vector
#'
#' @param dttm datetime sequence vector
#'
#' @return filled datetime sequence vector
#' @export
#'
#' @importFrom lubridate minutes
#'
fill_datetime <- function(dttm) {
  # detect the time interval of the sequence
  dttm_diff <- as.numeric(dttm - lag(dttm), units = 'mins')
  time_interval_minutes <- as.integer(dttm_diff[which(!is.na(dttm_diff))[1]])

  # find missing values
  dttm_na_i <- which(is.na(dttm))

  # fill missing values
  while (sum(is.na(dttm)) > 0) {
    for (i in dttm_na_i) {
      last_i <- i - 1
      next_i <- i + 1

      if ((last_i %in% dttm_na_i) | (last_i < 1)) {
        if ((next_i %in% dttm_na_i) | (next_i > length(dttm))) {
          next
        } else {
          dttm[i] <- dttm[next_i] - minutes(time_interval_minutes)
        }
      } else {
        dttm[i] <- dttm[last_i] + minutes(time_interval_minutes)
      }
    }
  }

  return(dttm)
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
#' @param method character, being `interpolate` or `repeat` as valid options
#'
#' @return numeric vector
#' @export
#'
#' @importFrom dplyr tibble lead %>%
#' @importFrom purrr pmap simplify
#'
#' @details
#' if we have a vector v = c(1, 2), and we choose the `interpolate` method,
#' then:
#'
#' `increase_numeric_resolution(v, 4, 'interpolate')`
#'
#' returns `c(1, 1.25, 1.5, 1.75, 2)`
#'
#' if we choose the `repeat` method, then:
#'
#' `increase_numeric_resolution(v, 4, 'repeat')`
#'
#' returns c(1, 1, 1, 1, 2)
#'
increase_numeric_resolution <- function(y, n, method = c('interpolate', 'repeat')) {
  if (method == 'interpolate') {
    tibble(y1 = y, y2 = lead(y, default = 0)) %>%
      pmap(~ interpolation(..1, ..2, n)) %>%
      simplify() %>%
      as.double()
  } else {
    rep(y, each = n)
  }
}

#' Increase datetime vector resolution
#'
#' @param y vector of datetime values
#' @param resolution_mins integer, interval of minutes between two consecutive datetime values
#'
#' @return datetime vector
#' @export
#'
#' @importFrom lubridate minutes as_datetime tz
increase_datetime_resolution <- function(y, resolution_mins) {
  seq.POSIXt(y[1], y[length(y)]+(y[2]-y[1])-minutes(resolution_mins), by = paste(resolution_mins, 'min')) %>% as_datetime(tz = tz(y))
}

#' Increase time resolution of a timeseries data frame
#'
#' @param df data.frame or tibble, first column of name `datetime` being of class datetime and rest of columns being numeric
#' @param resolution_mins integer, interval of minutes between two consecutive datetime values
#' @param method character, being `interpolate` or `repeat` as valid options.
#' See `increase_numeric_resolution` function for more information.
#'
#' @return tibble
#' @export
#'
#' @importFrom dplyr tibble select_if %>%
#'
increase_timeseries_resolution <- function(df, resolution_mins, method = c('interpolate', 'repeat')) {
  new_df <- tibble(datetime = increase_datetime_resolution(df$datetime, resolution_mins))
  current_resolution <- as.numeric(df$datetime[2] - df$datetime[1], units = "mins")
  numeric_df <- df %>% select_if(is.numeric)
  for (col in colnames(numeric_df)) {
    new_df[[col]] <- increase_numeric_resolution(numeric_df[[col]], n = current_resolution/resolution_mins, method)
  }
  return( new_df )
}


#' Decrease resolution of a numeric vector
#'
#' @param y original numeric vector
#' @param n integer, number of intra-values (counting the original value as the first one)
#' @param method character, being `average` or `first` as valid options
#'
#' @return numeric vector
#' @export
#'
#' @importFrom dplyr %>% group_by summarise pull
#' @importFrom tibble tibble
#' @importFrom rlang .data
#'
decrease_numeric_resolution <- function(y, n, method = c('average', 'first')) {
  if ((length(y)%%n) > 0) {
    message("Error decreasing resolution: the original vector should have a length multiple of `n`.")
    return(NULL)
  }

  if (method == 'average') {
    return(
      tibble(
        idx = rep(seq(1, length(y)/n), each = n),
        y = y
      ) %>%
        group_by(.data$idx) %>%
        summarise(y = mean(y)) %>%
        pull(y) %>%
        as.numeric()
    )
  } else if (method == 'first') {
    return(
      y[seq(1, length(y), n)]
    )
  } else {
    message("Error decreasing resolution: method not valid.")
    return( NULL )
  }
}

#' Decrease time resolution of timeseries data frame
#'
#' @param df data.frame or tibble, first column of name `datetime` being of class datetime and rest of columns being numeric
#' @param resolution_mins integer, interval of minutes between two consecutive datetime values
#' @param method character being 'average' or 'first'
#'
#' @return tibble
#' @export
#'
#' @importFrom dplyr %>% mutate group_by summarise_all distinct
#' @importFrom lubridate floor_date
#' @importFrom rlang .data
#'
decrease_timeseries_resolution <- function(df, resolution_mins, method = c('average', 'first')) {
  df2 <- df %>%
    mutate(datetime = floor_date(.data$datetime, paste(resolution_mins, 'minute')))
  if (method == 'average') {
    return(
      df2 %>%
        group_by(.data$datetime) %>%
        summarise_all(mean)
    )
  } else if (method == 'first') {
    return(
      df2 %>%
        distinct(.data$datetime, .keep_all = T)
    )
  } else {
    return( NULL )
  }
}


#' Fill down tibble columns until a maximum number of time slots
#'
#' @param data data.frame or tibble
#' @param vars character, names of columns to fill
#' @param max_timeslots integer, maximum number of time slots to fill
#'
#' @return tibble
#' @export
#'
#' @importFrom dplyr lag
#'
fill_down_until <- function(data, vars, max_timeslots = 1) {

  for (var_name in vars) {
    var_values <- data[[var_name]]
    na_idxs <- which(is.na(var_values) & !is.na(dplyr::lag(var_values)))
    na_idxs <- na_idxs[na_idxs != 1]
    var_values_filled <- var_values
    for (na_idx in na_idxs) {
      for (i in na_idx:(na_idx+max_timeslots-1)) {
        if (!is.na(var_values_filled[i]) | length(var_values_filled) < i)
          break
        var_values_filled[i] <- var_values_filled[na_idx-1]
      }
    }
    data[[var_name]] <- var_values_filled
  }

  return( data )
}


# Processing --------------------------------------------------------------

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
#' @importFrom dplyr %>% mutate select group_by summarise_if mutate_if
#' @importFrom rlang .data
#'
get_week_total <- function(df) {
  resolution <- get_time_resolution(df$datetime, units = "mins")
  df %>%
    mutate_if(
      is.numeric,
      `*`,
      resolution/60
    ) %>%
    group_by(
      week = get_week_from_datetime(.data$datetime)
    ) %>%
    summarise_if(
      is.numeric,
      sum
    )
}


#' Aggregate multiple timeseries columns to a single one
#'
#' The first column `datetime` will be kept.
#'
#' @param df data.frame or tibble, first column of name `datetime` being of class datetime and rest of columns being numeric
#' @param varname character, name of the aggregation column
#' @param omit character, name of columns to not aggregate
#'
#' @return tibble
#' @export
#'
aggregate_timeseries <- function(df, varname, omit = NULL) {
  tbl <- df['datetime']
  omit_col_n <- which(colnames(df) %in% c('datetime', omit))
  tbl[[varname]] <- rowSums(df[-omit_col_n])
  if (!is.null(omit)) {
    for (omit_var in omit) {
      tbl[[omit_var]] <- df[[omit_var]]
    }
  }
  return( tbl )
}

