#'
#' #' Get DynamoDB paws resource
#' #'
#' #' @param aws_access_key_id AWS access key ID of a user with permissions for DynamoDB
#' #' @param aws_secret_access_key AWS secret access key of a user with permissions for DynamoDB
#' #' @param region_name AWS DynamoDB region name (e.g. eu-west-1)
#' #'
#' #' @return paws DynamoDB object list
#' #' @export
#' #'
#' #' @details To create a user with permissions go to IAM AWS service and create a new user with Programmatic access, attaching existing policies directly for a fast configuration.
#' #'
#' get_dynamodb <- function(aws_access_key_id, aws_secret_access_key, region_name) {
#'   paws::dynamodb(
#'     config = list(
#'       credentials = list(
#'         creds = list(
#'           access_key_id = aws_access_key_id,
#'           secret_access_key = aws_secret_access_key
#'         )
#'       ),
#'       region = region_name
#'     )
#'   )
#' }
#'
#'
#'
#' #' Query items from DynamoDB Table given specific partition and sorting keys
#' #'
#' #' @param dynamodb_obj DynamoDB object obtained with `get_dynamodb` function
#' #' @param table_name character, DynamoDB table name
#' #' @param partition_key_name character, name of the partiton key of the DynamoDB Table. It has to be a string variable
#' #' @param partition_key_values vector of values of the partiton key of the DynamoDB Table
#' #' @param sorting_key_name character, name of the sorting key of the DynamoDB Table. It has to be a numeric variable
#' #' @param sorting_key_start numeric, start value of the sorting key
#' #' @param sorting_key_end  numeric, end value of the sorting key
#' #' @param parse_item_func function to convert every Item from the table to a tibble
#' #'
#' #' @return tibble
#' #' @export
#' #'
#' #' @importFrom purrr map map_dfr flatten
#' #'
#' #' @details Note that the amount of items is limited to 1 MB. If the items between the start and end values of the sorting key exceeds this limit,
#' #' the data obtained will go from start to the limited maximum.
#' #'
#' query_dynamodb_table <- function(dynamodb_obj, table_name, partition_key_name, partition_key_values, sorting_key_name, sorting_key_start, sorting_key_end, parse_item_func = NULL) {
#'   response <- map(
#'     partition_key_values,
#'     ~ dynamodb_obj$query(
#'       KeyConditionExpression = "#partition_key = :sensor_id AND #sorting_key BETWEEN :start AND :end",
#'       ExpressionAttributeNames = list(
#'         `#partition_key` = list(
#'           S = partition_key_name
#'         ),
#'         `#sorting_key` = list(
#'           S = sorting_key_name
#'         )
#'       ),
#'       ExpressionAttributeValues = list(
#'         `:sensor_id` = list(
#'           S = .x
#'         ),
#'         `:start` = list(
#'           N = sorting_key_start
#'         ),
#'         `:end` = list(
#'           N = sorting_key_end
#'         )
#'       ),
#'       TableName = table_name
#'     )[['Items']]
#'   )
#'
#'   if (is.null(parse_item_func)) {
#'     return( flatten(response) )
#'   } else {
#'     map_dfr(response, ~ parse_items(.x, parse_item_func))
#'   }
#' }
#'
#' parse_items <- function(items, parse_item_func) {
#'   map_dfr(items, ~ parse_item_func(.x))
#' }
#'
#'
#' #' Query table using a timestamp variable as sorting key
#' #'
#' #' @param dynamodb_obj DynamoDB object obtained with `get_dynamodb` function
#' #' @param table_name character, DynamoDB table name
#' #' @param partition_key_name character, name of the partiton key of the DynamoDB Table. It has to be a string variable.
#' #' @param partition_key_values vector of values of the partiton key of the DynamoDB Table
#' #' @param sorting_key_name character, name of the sorting key of the DynamoDB Table. It has to be a numeric variable
#' #' @param start_date Date, start date (only class `Date`, not `datetime`)
#' #' @param end_date Date, end date (only class `Date`, not `datetime`)
#' #' @param parse_item_func function to convert every Item from the table to a tibble
#' #' @param tzone character, time zone of the timeseries data
#' #' @param query_interval_days integer, number of days of each query interval
#' #' @param milliseconds logical, whether the timestamp is in milliseconds or seconds
#' #'
#' #' @return tibble
#' #' @export
#' #'
#' #' @importFrom purrr pmap_dfr
#' #'
#' query_dynamodb_table_timeseries <- function(dynamodb_obj, table_name, partition_key_name, partition_key_values,
#'                                             sorting_key_name, start_date, end_date, parse_item_func = NULL,
#'                                             tzone = 'Europe/Paris', query_interval_days = 30, milliseconds = TRUE) {
#'
#'   time_range <- adapt_date_range(start_date, end_date, tzone, query_interval_days, milliseconds)
#'
#'   df <- pmap_dfr(
#'     time_range,
#'     ~ query_dynamodb_table(dynamodb_obj, table_name, partition_key_name, partition_key_values,
#'                            sorting_key_name,..1, ..2, parse_item_func)
#'   )
#'
#'   if (nrow(df) == 0) {
#'     return( NULL )
#'   } else {
#'     return( df )
#'   }
#' }
#'
#'
#'
#' #' Delete items from DynamoDB table
#' #'
#' #' The table must have partition and sorting key and the items deleted are within a range of the sorting key
#' #'
#' #' @param dynamodb_obj DynamoDB object obtained with `get_dynamodb` function
#' #' @param table_name character, DynamoDB table name
#' #' @param partition_key_name character, name of the partiton key of the DynamoDB Table. It has to be a string variable
#' #' @param partition_key_values vector of values of the partiton key of the DynamoDB Table
#' #' @param sorting_key_name character, name of the sorting key of the DynamoDB Table. It has to be a numeric variable
#' #' @param sorting_key_start numeric, start value of the sorting key
#' #' @param sorting_key_end  numeric, end value of the sorting key
#' #'
#' #' @export
#' #'
#' #' @importFrom purrr walk
#' #'
#' dynamodb_delete_items <- function(dynamodb_obj, table_name, partition_key_name, partition_key_values,
#'                                   sorting_key_name, sorting_key_start, sorting_key_end) {
#'   items <- query_dynamodb_table(dynamodb_obj, table_name, partition_key_name, partition_key_values,
#'                                 sorting_key_name, sorting_key_start, sorting_key_end)
#'   walk(
#'     items,
#'     ~ dynamodb_obj$delete_item(
#'       TableName = table_name,
#'       Key = .x[c(partition_key_name, sorting_key_name)]
#'     )
#'   )
#' }
#'
#'
#' #' Put items to DynamoDB table
#' #'
#' #'
#' #' @param dynamodb_obj DynamoDB object obtained with `get_dynamodb` function
#' #' @param table_name character, DynamoDB table name
#' #' @param items list of value-key lists, with all Keys (partition and sorting) of the table and other optional attributes.
#' #' For example one item could be: `list(id = list(S = 'id123'), timestamp = list(N = 1622190385), data = list(temperature = list(N = 24.2), humidity = list(N = 32)))`
#' #'
#' #' @export
#' #'
#' #' @importFrom purrr walk
#' #'
#' dynamodb_put_items <- function(dynamodb_obj, table_name, items) {
#'   walk(
#'     items,
#'     ~ dynamodb_obj$put_item(
#'       TableName = table_name,
#'       Item = .x
#'     )
#'   )
#' }
#'
#'
#'
