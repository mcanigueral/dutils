
# Dygraph plots -----------------------------------------------------------


#' Convert time-series data.frame to time-series object
#'
#' @param df data.frame or tibble, first column of name `datetime` being of class datetime and rest of columns being numeric
#'
#' @return xts time-series object
#' @export
#'
#' @importFrom xts xts
#'
df_to_ts <- function(df) {
  df <- df[!is.na(df[[1]]), ]
  xts::xts(df[-1], order.by = df [[1]])
}


#' Format Dygraphs plot with CSS file and extra options
#'
#' @param dyplot dygraph object
#' @param css_file character path to a CSS file to format dygraph plot. If NULL, no CSS formating is applied to dygraph.
#' @param ... extra arguments to pass to `dygraphs::dyOptions` function
#'
#' @return dygraphs plot
#' @export
#'
#' @importFrom dplyr %>%
#' @importFrom dygraphs dyLegend dyOptions dyCSS
#'
#' @details
#' Atention! By default this function needs a CSS file in WWW folder.
#'
format_dygraph <- function(dyplot, css_file="www/style.css", ...) {
  dyplot <- dyplot %>%
    dyLegend(show = "onmouseover") %>%
    dyOptions(retainDateWindow = TRUE,
              useDataTimezone = TRUE,
              ...)
  if (is.null(css_file)) {
    return( dyplot )
  } else {
    dyplot %>% dyCSS(css_file)
  }
}



#' Function to plot components according to a configuration table
#'
#' @param df data.frame or tibble, first column of name `datetime` being of class datetime and rest of columns being numeric
#' @param plot_conf tibble of the plot configuration with columns `variable`, `label`, `color`, `fill` and `width` to pass to dygraph.
#' @param ylab character, label of y axis
#' @param css_file character path to a CSS file to format dygraph plot. If NULL, no CSS formating is applied to dygraph.
#' @param ... extra arguments to pass to `dygraphs::dyOptions` function
#'
#' @return dygraphs plot
#' @export
#'
#' @importFrom dplyr %>%
#' @importFrom dygraphs dygraph dySeries
#' @importFrom purrr transpose
#'
#' @details
#' Atention! By default this function needs a CSS file in WWW folder.
#'
plot_components <- function(df, plot_conf, ylab = "kW", css_file = "www/style.css", ...) {
  dyplot <- df %>% df_to_ts() %>% dygraph(group = "a", ylab = ylab)
  for (component in transpose(plot_conf)) {
    if (component$variable %in% names(df)) {
      dyplot <- dySeries(dyplot, component$variable, component$label, component$color,
                         fillGraph = component$fill, strokeWidth = component$width)
    }
  }
  format_dygraph(dyplot, css_file, ...)
}

