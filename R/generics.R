

#' Get current date from Internet, not from local machine
#'
#' @return date
#' @export
#'
#' @importFrom httr GET
get_current_date <- function() {
  response <- httr::GET('api.github.com/repos/mcanigueral/dutils')
  as.Date(response$date)
}


#' Get percentage of tibble variables
#'
#' The percentage is relative to the variable in `from` argument
#'
#' @param tbl tibble or data.frame
#' @param from character, column name of the base variable
#' @param discard character or character vector, columns to discard of the percentage operations (but included in the results)
#' @param percent logical, whether to output the percentages in percent or not
#' @param keep logical, whether to include the base variable in `from` argument to the output tibble
#' @param digits integer, number of digits to round the percentages
#'
#' @return tibble
#' @export
#'
get_percentage <- function(tbl, from, discard = NULL, percent = TRUE, keep = FALSE, digits = 2) {
  if (is.character(discard)) {
    .tbl <- tbl[, !(colnames(tbl) %in% c(discard, from))]
  } else {
    .tbl <- tbl[, colnames(tbl) != from]
  }

  .tbl <- .tbl/tbl[[from]]
  .tbl[is.na(.tbl)] <- 0
  .tbl <- round(.tbl, digits)
  if (percent) {
    .tbl <- .tbl*100
  }

  if (is.character(discard)) {
    .tbl[discard] <- tbl[discard]
  }

  if (keep) {
    .tbl[from] <- tbl[from]
    return( .tbl[colnames(tbl)] )
  } else {
    cols <- colnames(tbl)[colnames(tbl) != from]
    return( .tbl[cols] )
  }
}
