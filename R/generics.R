
# Package environment -----------------------------------------------------------------

dutils_env <- new.env()


# Tic-toc -----------------------------------------------------------------

#' Time difference start function
#'
#' Use this function together with `toc()` to control time spent by functions
#'
#' @return numeric
#' @export
#'
tic <- function() {
  assign("tic", Sys.time(), envir = dutils_env)
}

#' Time difference end function
#'
#' Use this function together with `tic()` to control time spent by functions
#'
#' @param units character, one of "auto", "secs", "mins", "hours", "days" and "weeks"
#' @param digits integer, number of decimals
#'
#' @return numeric
#' @export
#'
toc <- function(units = "secs", digits = 2) {
  tic <- get("tic", envir = dutils_env)
  time_diff <- round(difftime(Sys.time(), tic, units = units)[[1]], digits)
  message(paste("---- Done in", time_diff, units))
}


# Get percentage ----------------------------------------------------------

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



# Tooltip --------------------------------------------------------

#' Add info icon with a pop-up tip to a label
#'
#' @param label character, label
#' @param tip character, info text
#'
#' @export
#'
text_info <- function (label, tip) {
  if (is.null(tip)) {
    return(label)
  } else {
    shiny::tags$div(
      shiny::includeCSS(system.file("www", "tooltip.css", package = "dutils")),
      shiny::tags$p(label, style = "display: inline-block;vertical-align:top;margin-bottom:0"),
      shiny::tags$div(
        class = "tooltip2",
        style = "display: inline-block;vertical-align:top;",
        shiny::HTML('<i class="fa fa-info-circle" style="color:#4682B4; font-size: 16px;"></i>'),
        shiny::tags$span(
          class="tooltiptext",
          style = "font-size: 14px; font-weight: normal; line-height: 120%;",
          shiny::HTML(tip)
        )
      )
    )
  }
}


# Preprocessing -----------------------------------------------------------

#' Round to nearest interval
#'
#' @param dbl number to round
#' @param interval rounding interval
#'
#' @return numeric value
#' @export
#'
round_to_interval <- function(dbl, interval) {
  round(dbl/interval)*interval
}

