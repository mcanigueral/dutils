
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
get_dynamodb_py <- function(aws_access_key_id, aws_secret_access_key, region_name) {
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
get_dynamo_table_py <- function(dynamodb, table_name) {
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
query_table_py <- function(dynamo_table, partition_key_name, partition_key_values,
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

  if (nrow(df) == 0) {
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
#' @param milliseconds logical, whether the sorting key is a timestamp in milliseconds or not
#'
#' @return tibble
#' @export
#'
#' @importFrom purrr map_dfr
#' @importFrom dplyr mutate select everything
#' @importFrom lubridate as_datetime floor_date
#' @importFrom rlang .data
#'
query_timeseries_data_table_py <- function(dynamo_table, partition_key_name, partition_key_values,
                                        sort_key_name, start_date, end_date, tzone = 'Europe/Paris',
                                        query_interval_days = 30, milliseconds = T) {
  time_range <- adapt_date_range(start_date, end_date, tzone, query_interval_days, milliseconds)

  df <- pmap_dfr(
    time_range,
    ~ query_table_py(dynamo_table, partition_key_name, partition_key_values,
                  sort_key_name, ..1, ..2, parse = T)
  )

  if (nrow(df) == 0) {
    return(NULL)
  } else {
    return(df)
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
scan_table_py <- function(dynamo_table, attribute_name, attribute_start, attribute_end, parse = T) {
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


#' Put data frame to DynamoDB table
#'
#' @param dynamo_table boto3 DynamoDB Table obtained with `get_dynamo_table` function
#' @param df data frame to put
#'
#' @export
#'
dynamodb_put_df_py <- function(dynamo_table, df) {
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
dynamodb_delete_df_py <- function(dynamo_table, df) {
  reticulate::source_python(system.file("python/dynamodb/utils.py", package = 'dutils'), envir = pyenv)
  pyenv$delete_df(dynamo_table, df)
}




