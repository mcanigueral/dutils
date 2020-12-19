
# Fill NA values ----------------------------------------------------------

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
