
#' Get DynamoDB boto3 resource
#'
#' @param aws_access_key_id AWS access key ID of a user with permissions for DynamoDB
#' @param aws_secret_access_key AWS secret access key of a user with permissions for DynamoDB
#' @param region_name AWS DynamoDB region name (e.g. eu-west-1)
#'
#' @return boto3 DynamoDB resource
#' @export
#'
#' @details To create a user with permissions go to IAM AWS service and create a new user with Programmatic access, attaching existing policies directly for a fast configuration.
#'
get_dynamodb <- function(aws_access_key_id, aws_secret_access_key, region_name) {
  boto3 <- reticulate::import("boto3")
  boto3$resource(
    'dynamodb',
    aws_access_key_id = aws_access_key_id,
    aws_secret_access_key = aws_secret_access_key,
    region_name = region_name
  )
}


#' Get boto3 DynamoDB Table object
#'
#' @param dynamodb boto3 DynamoDB resource obtained with `get_dynamodb` function
#' @param table_name character, table name
#'
#' @return boto3 DynamoDB Table object
#' @export
#'
get_dynamo_table <- function(dynamodb, table_name) {
  dynamo_table <- dynamodb$Table(table_name)
  items_count <- count_table_items(dynamo_table)
  if (!is.null(items_count) & (items_count > 0)) {
    return( dynamo_table )
  } else {
    if (!is.null(items_count)) message("This table does not exist.")
    if (items_count <= 0) message("This table is empty.")
    return( NULL )
  }
}

table_item_count <- function(table) {
  table$item_count
}
count_table_items <- purrr::possibly(table_item_count, otherwise = NULL)


date_to_timestamp <- function(date, tzone = "Europe/Paris", milliseconds = T) {
  timestamp <- as.integer(
    lubridate::force_tz(
      lubridate::as_datetime(date, tz = "UTC"),
      tzone
    )
  )
  if (milliseconds) {
    return ( timestamp*1000 )
  } else {
    return( timestamp )
  }
}


#' Convert start and end dates to DynamoDB timestamp
#'
#' @param start_date Date, start date
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
  # start_dttm <- as_datetime(start_date)
  # end_dttm <- as_datetime(end_date)
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


#' From non-SQL DynamoDB table to spread tibble
#'
#' @param table data.frame, response from DynamoDB query
#' @param tzone character, time zone
#'
#' @return tibble
#' @export
#'
#' @importFrom dplyr as_tibble mutate select %>%
#' @importFrom lubridate as_datetime
#' @importFrom xts align.time
#' @importFrom rlang .data
#'
adapt_data_table_format <- function(table, tzone = "Europe/Paris") {
  table %>%
    as_tibble() %>%
    mutate(
      datetime = as_datetime(.data$timestamp/1000, tz = tzone),
      datetime = align.time(.data$datetime, n=60*5)
    ) %>%
    select(.data$id, .data$datetime, .data$data) %>%
    spread_data_column()
}

spread_data_column <- function(table, data_column_name = 'data') {
  table %>%
    dplyr::mutate(purrr::map_dfr(.data[[data_column_name]], ~ spread_data_item(.x))) %>%
    dplyr::select(- data_column_name)
}

spread_data_item <- function(item) {
  purrr::map_dfc(item, ~ parse_python_object(.x))
}


#' Python to R parsing for DynamoDB items
#'
#' @param object Python object of class decimal.Decimal, character or logical. For other types a NA value will be returned.
#'
#' @return R object
#' @export
#'
parse_python_object <- function(object) {
  if ('decimal.Decimal' %in% class(object)) {
    return( as.numeric(as.character(object)) )
  } else if ('character' %in% class(object)) {
    return( as.character(object) )
  } else if ('logical' %in% class(object)) {
    return( as.logical(object) )
  } else if ('list' %in% class(object)) {
    return( parse_python_objects_list(object) )
  } else {
    return( object )
  }
}

#' Parse a list of Python objects
#'
#' @param objects_list list of Python objects of class decimal.Decimal, character or logical. For other types a NA value will be returned.
#'
#' @return list of R objects
#' @export
#'
#' @importFrom purrr map
parse_python_objects_list <- function(objects_list) {
  map(objects_list, ~ parse_python_object(.x))
}

#' Convert DynamoDB Items list to an R tibble
#'
#' @param items list, Items from DynamoDB, being each Item an individual list of Python objects of class decimal.Decimal, character or logical. For other types a NA value will be returned.
#'
#' @return tibble
#' @export
#'
#' @importFrom purrr map_dfr
#' @importFrom dplyr as_tibble
#'
dynamodb_items_to_tbl <- function(items) {
  map_dfr(items, ~ as_tibble(parse_python_objects_list(.x)))
}


#' Parse data frame with Python objects to R object
#'
#' @param df data frame with Python objects
#'
#' @return data frame or tibble
#' @export
#'
#' @importFrom dplyr %>% mutate_all
#' @importFrom purrr simplify
#'
parse_python_data_frame <- function(df) {
  df %>%
    mutate_all(parse_python_object) %>%
    mutate_all(simplify)
}



#' Get data from a specific partition key
#'
#' @param dynamodb boto3 DynamoDB resource obtained with `get_dynamodb` function
#' @param table_name character, table name
#' @param id character, partition key ID
#' @param start_date Date, start date
#' @param end_date Date, end date
#' @param tzone character, time zone
#'
#' @return tibble
#' @export
#'
#' @importFrom purrr pmap_dfr
#' @importFrom dplyr %>% select
#' @importFrom rlang .data
get_id_table <- function(dynamodb, table_name, id, start_date, end_date, tzone = "Europe/Paris") {
  dynamodb_table <- get_dynamo_table(dynamodb, table_name)
  if (is.null(dynamodb_table)) return( NULL )

  time_range <- adapt_date_range(start_date, end_date)

  table <- pmap_dfr(
    time_range,
    ~ query_data_table(dynamodb_table, id, ..1, ..2)
  )
  if (nrow(table) == 0) return( NULL )
  table %>%
    select(.data$id, .data$timestamp, .data$data)  %>%
    adapt_data_table_format(tzone)
}




# Wrap python functions ---------------------------------------------------
pyenv <- new.env()


#' Query items from DynamoDB Table given specific partition and sorting keys
#'
#' @param dynamo_table boto3 DynamoDB Table obtained with `get_dynamo_table` function
#' @param partition_key_name character, name of the partiton key of the DynamoDB Table. It has to be a string variable.
#' @param partition_key_values vector of values of the partiton key of the DynamoDB Table
#' @param sort_key_name character, name of the sorting key of the DynamoDB Table. It has to be a numeric variable.
#' @param sort_key_start numeric, start value of the sorting key
#' @param sort_key_end  numeric, end value of the sorting key
#' @param parse logical, whether to parse Python objects to R objects
#'
#' @return tibble
#' @export
#'
#' @importFrom dplyr as_tibble %>% select everything
#' @importFrom rlang .data
#'
query_table <- function(dynamo_table, partition_key_name, partition_key_values,
                        sort_key_name = NULL, sort_key_start = NULL, sort_key_end = NULL, parse = T) {
  reticulate::source_python(system.file("python/dynamodb/utils.py", package = 'dutils'), envir = pyenv)

  df <- pyenv$query_table(
    dynamo_table,
    partition_key_name,
    partition_key_values,
    reticulate::r_to_py(sort_key_name),
    reticulate::r_to_py(sort_key_start),
    reticulate::r_to_py(sort_key_end)
  ) %>%
    as_tibble()

  if ((nrow(df) == 0) | (is.null(df))) {
    return(NULL)
  }

  if (!parse) {
    return( df )
  } else {
    return( parse_python_data_frame(df) )
  }
}


#' Query table using a timestamp variable as sorting key
#'
#' @param dynamo_table boto3 DynamoDB Table obtained with `get_dynamo_table` function
#' @param partition_key_name character, name of the partiton key of the DynamoDB Table. It has to be a string variable.
#' @param partition_key_values vector of values of the partiton key of the DynamoDB Table
#' @param sort_key_name character, name of the sorting key of the DynamoDB Table. It has to be a numeric variable.
#' @param start_date Date, start date
#' @param end_date Date, end date
#' @param tzone character, time zone of the timeseries data
#' @param query_interval_days integer, number of days of each query interval
#' @param milliseconds logical, whether the timestamp is in milliseconds or not
#' @param time_interval_mins integer, interval of minutes to align datetime column. If `NULL` the datetime column is not aligned.
#' @param spread_column character, name of the dictionary variable to spread. If `NULL` no columns are spread.
#'
#' @return tibble
#' @export
#'
#' @importFrom purrr map_dfr
#' @importFrom dplyr mutate select everything
#' @importFrom lubridate as_datetime
#' @importFrom xts align.time
#' @importFrom rlang .data
#'
query_timeseries_data_table <- function(dynamo_table, partition_key_name, partition_key_values,
                                        sort_key_name, start_date, end_date, tzone = 'Europe/Paris',
                                        query_interval_days = 30, milliseconds = TRUE,
                                        time_interval_mins = NULL, spread_column = 'data') {
  time_range <- adapt_date_range(start_date, end_date, tzone, query_interval_days, milliseconds)

  df <- pmap_dfr(
    time_range,
    ~ query_table(dynamo_table, partition_key_name, partition_key_values,
                  sort_key_name, ..1, ..2, parse = T)
  )

  if (is.null(df)) {
    return(NULL)
  }

  if (milliseconds) {
    df <- mutate(df, datetime = as_datetime(.data$timestamp/1000, tz = tzone))
  } else {
    df <- mutate(df, datetime = as_datetime(.data$timestamp, tz = tzone))
  }

  if (!is.null(time_interval_mins)) {
    df <- mutate(df, datetime = align.time(.data$datetime, n=60*time_interval_mins))
  }

  df <- select(df, .data[[partition_key_name]], .data[['datetime']], everything(), - .data[[sort_key_name]])

  if (is.null(spread_column)) {
    return( df )
  } else {
    return( spread_data_column(df, spread_column) )
  }
}


#' Scan items from DynamoDB Table given specific numeric attribute range
#'
#' @param dynamo_table boto3 DynamoDB Table obtained with `get_dynamo_table` function
#' @param attribute_name character, numeric attribute name
#' @param attribute_start numeric, attribute start value
#' @param attribute_end numeric, attribute end value
#' @param parse logical, whether to parse Python objects to R objects
#'
#' @return tibble
#' @export
#'
#' @importFrom dplyr as_tibble %>%
#'
scan_table <- function(dynamo_table, attribute_name, attribute_start, attribute_end, parse = T) {
  reticulate::source_python(system.file("python/dynamodb/utils.py", package = 'dutils'), envir = pyenv)
  df <- pyenv$scan_table(
    dynamo_table,
    attribute_name,
    attribute_start,
    attribute_end
  ) %>% as_tibble()

  if (!parse) {
    return( df )
  } else {
    return( parse_python_data_frame(df) )
  }
}




query_data_table <- function(dynamodb_table, user_id, start_timestamp, end_timestamp) {
  reticulate::source_python(system.file("python/dynamodb/utils.py", package = 'dutils'), envir = pyenv)
  pyenv$query_data_table(dynamodb_table, user_id, start_timestamp, end_timestamp)
}


#' Put data frame to DynamoDB table
#'
#' @param dynamo_table boto3 DynamoDB Table obtained with `get_dynamo_table` function
#' @param df data frame to put
#'
#' @export
#'
dynamodb_put_df <- function(dynamo_table, df) {
  reticulate::source_python(system.file("python/dynamodb/utils.py", package = 'dutils'), envir = pyenv)
  pyenv$put_df(dynamo_table, df)
}


#' Delete data frame from DynamoDB table
#'
#' @param dynamo_table boto3 DynamoDB Table obtained with `get_dynamo_table` function
#' @param df data frame to put
#'
#' @export
#'
dynamodb_delete_df <- function(dynamo_table, df) {
  reticulate::source_python(system.file("python/dynamodb/utils.py", package = 'dutils'), envir = pyenv)
  pyenv$delete_df(dynamo_table, df)
}




