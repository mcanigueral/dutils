
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
#' @param css_file character path to a CSS file to format dygraph plot. If NULL, custom CSS is applied to dygraph.
#' @param ... extra arguments to pass to `dygraphs::dyOptions` function
#'
#' @return dygraphs plot
#' @export
#'
#' @importFrom dplyr %>%
#' @importFrom dygraphs dyOptions dyCSS
#'
#' @details
#' Atention! By default this function applies custom CSS file.
#'
format_dygraph <- function(dyplot, css_file=NULL, ...) {
  dyplot <- dyplot %>%
    dyOptions(retainDateWindow = TRUE,
              useDataTimezone = TRUE,
              ...)
  if (is.null(css_file)) {
    css_file <- system.file("www", "dystyle.css", package = "dutils")
  }
  dyplot %>% dyCSS(css_file)
}


#' Plot a timeseries data.frame in a Dygraphs HTML plot
#'
#' First column of the data.frame must be a datetime or date column.
#' The rest of columns must be numeric of the same units.
#'
#' @param df data.frame or tibble, first column of name `datetime` being of class datetime and rest of columns being numeric
#' @param title character, title of the plot (accepts HTML code)
#' @param xlab character, X axis label (accepts HTML code)
#' @param ylab character, Y axis label (accepts HTML code)
#' @param group character, dygraphs group to associate this plot with. The x-axis zoom level of dygraphs plots within a group is automatically synchronized.
#' @param legend_width integer, width (in pixels) of the div which shows the legend.
#' @param format logical, whether to format dygraph with custom CSS file
#' @param css_file character path to a CSS file to format dygraph plot. If NULL, custom CSS is applied to dygraph. Only used when `format` is `TRUE`.
#' @param width Width in pixels (optional, defaults to automatic sizing)
#' @param height Height in pixels (optional, defaults to automatic sizing)
#' @param ... extra arguments to pass to `dygraphs::dyOptions` function. Only used when `format` is `TRUE`
#'
#' @return dygraph
#' @export
#'
#' @importFrom dygraphs dygraph dyLegend
#'
dyplot <- function(df, title = NULL, xlab = NULL, ylab = NULL, group = NULL, legend_width = 250, format = TRUE, css_file=NULL, width = NULL, height = NULL, ...) {
  dyplot <- dygraph(df_to_ts(df), main = title, xlab = xlab, ylab = ylab, group = group, width = width, height = height) %>%
    dyLegend(show = "always", width = legend_width)
  if (!format) {
    return( dyplot )
  } else {
    return( format_dygraph(dyplot, css_file, ...) )
  }
}



#' Function to plot components according to a configuration table
#'
#' @param df data.frame or tibble, first column of name `datetime` being of class datetime and rest of columns being numeric
#' @param plot_conf tibble of the plot configuration with columns `variable`, `label`, `color`, `fill` and `width` to pass to dygraph.
#' @param ylab character, label of y axis
#' @param legend_width integer, width (in pixels) of the div which shows the legend.
#' @param css_file character path to a CSS file to format dygraph plot. If NULL, custom CSS is applied to dygraph.
#' @param ... extra arguments to pass to `dygraphs::dyOptions` function
#'
#' @return dygraphs plot
#' @export
#'
#' @importFrom dygraphs dygraph dySeries
#' @importFrom purrr transpose
#' @importFrom stringr str_length
#'
#' @details
#' Attention! By default this function applies dutils CSS file.
#'
plot_components <- function(df, plot_conf, ylab, legend_width = NULL, css_file = NULL, ...) {
  ts <- df_to_ts(df)
  dyplot <- dygraph(ts, group = "a", ylab = ylab)
  min_legend_width <- 0
  for (component in transpose(plot_conf)) {
    if (component$variable %in% names(df)) {
      dyplot <- dySeries(dyplot, component$variable, component$label, component$color,
                         fillGraph = component$fill, strokeWidth = component$width)
      if (is.null(legend_width)) {
        # The legend width is calculated using a relation of 9 pixels = 1 character
        # also including the line symbol and margins applied to dyLegend box (+14)
        min_legend_width <- max(min_legend_width, 9*stringr::str_length(component$label)+14)
      }
    }
  }

  if (is.null(legend_width)) {
    legend_width <- min_legend_width
  }

  format_dygraph(dyplot, css_file, ...) %>%
    dyLegend(show = "always", width = legend_width)
}

