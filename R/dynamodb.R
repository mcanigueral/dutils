
#' Get data from a specific partition key
#'
#' @param dynamodb boto3 DynamoDB object
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
  dynamodb_table <- get_dynamodb_table(dynamodb, table_name)
  if (is.null(dynamodb_table)) return( NULL )

  time_range <- adapt_date_range(start_date, end_date)

  table <- pmap_dfr(
    time_range,
    ~ query_table(dynamodb_table, id, ..1, ..2)
  )
  if (nrow(table) == 0) return( NULL )
  table %>%
    select(.data$id, .data$timestamp, .data$data)  %>%
    adapt_table_format(tzone)
}


#' Get boto3 DynamoDB Table object
#'
#' @param dynamodb boto3 DynamoDB object
#' @param table_name character, table name
#'
#' @return boto3 DynamoDB Table object
#' @export
#'
get_dynamodb_table <- function(dynamodb, table_name) {
  dynamodb_table <- dynamodb$Table(table_name)
  items_count <- count_table_items(dynamodb_table)
  if (!is.null(items_count) & (items_count > 0)) {
    return( dynamodb_table )
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


#' Convert start and end dates to DynamoDB timestamp
#'
#' @param start_date Date, start date
#' @param end_date Date, end date
#'
#' @return tibble
#' @export
#'
#' @importFrom dplyr tibble select %>%
#' @importFrom rlang .data
#' @importFrom lubridate days as_datetime
#'
adapt_date_range <- function(start_date, end_date) {
  start_dttm <- as_datetime(start_date)
  end_dttm <- as_datetime(end_date)
  if (as.integer(start_dttm - end_dttm, units = 'days') > 30) {
    tibble(
      start.date = seq.POSIXt(start_dttm, end_dttm, by = '30 days'),
      end.date = .data$start.date + days(30),
      start.timestamp = as.integer(.data$start.date)*1000,
      end.timestamp = as.integer(.data$end.date)*1000
    ) %>%
      select(.data$start.timestamp, .data$end.timestamp)
  } else {
    tibble(
      start.timestamp = as.integer(start_dttm)*1000,
      end.timestamp = as.integer(end_dttm)*1000
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
adapt_table_format <- function(table, tzone = "Europe/Paris") {
  table %>%
    as_tibble() %>%
    mutate(
      datetime = as_datetime(.data$timestamp/1000, tz = tzone),
      datetime = align.time(.data$datetime, n=60*5)
    ) %>%
    select(.data$id, .data$datetime, .data$data) %>%
    spread_data_column()
}

spread_data_column <- function(table) {
  table %>%
    dplyr::mutate(purrr::map_dfr(.data$data, ~ spread_data_item(.x))) %>%
    dplyr::select(-'data')
}

spread_data_item <- function(item) {
  dplyr::bind_cols(purrr::map(item, ~ parse_python_object(.x)))
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
  } else if ('character' == class(object)) {
    return( as.character(object) )
  } else if ('logical' == class(object)) {
    return( as.logical(object) )
  } else {
    return( NA )
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


# Wrap python functions ---------------------------------------------------
pyenv <- new.env()


#' Title
#'
#' @param dynamodb_table boto3 DynamoDB Table object
#' @param partition_key_name character, name of the partiton key of the DynamoDB Table
#' @param partition_key_values vector of values of the partiton key of the DynamoDB Table
#' @param sort_key_name character, name of the sorting key of the DynamoDB Table
#' @param sort_key_values vector of values of the sorting key of the DynamoDB Table
#'
#' @return tibble
#' @export
#'
#' @importFrom purrr set_names map map_dfr
#' @importFrom dplyr %>% select everything
dynamodb_query_items <- function(dynamodb_table, partition_key_name, partition_key_values, sort_key_name = NULL, sort_key_values = NULL) {
  # reticulate::source_python(system.file("python/dynamodb/utils.py", package = 'dutils'), envir = pyenv)
  dbKey <- reticulate::import("boto3.dynamodb")$conditions$Key
  keys_items <- map(
    set_names(partition_key_values),
    ~ dynamodb_table$query(KeyConditionExpression = dbKey(partition_key_name)$eq(.x))[['Items']]
  )
  map_dfr(keys_items, ~ dynamodb_items_to_tbl(.x)) %>%
    select(partition_key_name, everything())
}



dynamodb_query_table <- function(dynamodb_table, user_id, start_timestamp, end_timestamp) {
  reticulate::source_python(system.file("python/dynamodb/utils.py", package = 'dutils'), envir = pyenv)
  pyenv$query_table(dynamodb_table, user_id, start_timestamp, end_timestamp)
}


#' Put data frame to DynamoDB table
#'
#' @param dynamodb_table boto3 DynamoDB Table object
#' @param df data frame to put
#'
#' @export
#'
dynamodb_put_df <- function(dynamodb_table, df) {
  reticulate::source_python(system.file("python/dynamodb/utils.py", package = 'dutils'), envir = pyenv)
  pyenv$put_df(dynamodb_table, df)
}


#' Delete data frame from DynamoDB table
#'
#' @param dynamodb_table boto3 DynamoDB Table object
#' @param df data frame to put
#'
#' @export
#'
dynamodb_delete_df <- function(dynamodb_table, df) {
  reticulate::source_python(system.file("python/dynamodb/utils.py", package = 'dutils'), envir = pyenv)
  pyenv$delete_df(dynamodb_table, df)
}




