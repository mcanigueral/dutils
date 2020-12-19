
# Fill NA values ----------------------------------------------------------

#' Fill from past values
#'
#' @param tbl tibble or data.frame, with NA values in some columns
#' @param colnames character or vector of characters, column names with NA values
#' @param back integer, number of positions (rows) to go back and get the filling value
#'
#' @return tibble or data.frame
#' @export
#'
fill_from_past <- function(tbl, colnames, back=24) {
  tbl_to_fill <- tbl[colnames]
  for (col in colnames) {
    na_idx <- which(is.na(tbl_to_fill[col]))
    for (idx in na_idx) {
      back_idx <- idx-back
      if (back_idx <= 0) back_idx <- 1
      tbl_to_fill[idx, col] <- tbl_to_fill[back_idx, col]
    }
  }
  tbl[colnames] <- tbl_to_fill
  return( tbl )
}
