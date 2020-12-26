

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
