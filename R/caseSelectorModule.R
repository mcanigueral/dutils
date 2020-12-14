
# Module for case (static, flexible, comparison) radio selector --------------------------------------------------------

#' UI function for caseSelector module
#'
#' @param id character, module ID
#' @param tabs character vector, tabs names
#' @param height Dygraphs height. Must be a valid CSS unit (like "100%", "400px", "auto") or a number, which will be coerced to a string and have "px" appended.
#'
#' @return shiny UI tagList
#' @export
#'
#' @importFrom shiny NS tags
#' @importFrom shinyWidgets radioGroupButtons
#' @importFrom dygraphs dygraphOutput
#'
caseSelectorUI <- function(id, tabs, height) {
  ns <- NS(id)
  tags$div(
    radioGroupButtons(ns("case"), NULL, tabs, justified = TRUE),
    dygraphOutput(ns("graph"), height = height)
  )
}

#' Server function of caseSelector module
#'
#' @param id character, module ID
#' @param df1 data.frame or tibble, data of case 1 being `datetime` the first column
#' @param df2 data.frame or tibble, data of case 2 being `datetime` the first column
#' @param plot_func function to plot the dygraph output, with same parameters as `dutils::plot_components` function
#' @param plot_conf tibble of the plot configuration to pass to `dutils::plot_components` function
#' @param ... extra parameters passed to dyOptions
#'
#' @return reactive `case` module input
#' @export
#'
#' @importFrom shiny moduleServer reactive
#' @importFrom dygraphs renderDygraph
#' @importFrom dplyr %>% tibble
#'
caseSelector <- function(id, df1, df2, plot_func, plot_conf, ...) {
  moduleServer(
    id,
    function(input, output, session) {
      output$graph <-
        renderDygraph({
          plot <- switch(
            as.integer(input$case),
            df1() %>% plot_func(plot_conf(), ...),
            df2() %>% plot_func(plot_conf(), ...),
            tibble(
              datetime = df1()[[1]],
              case1 = rowSums(df1()[-1]),
              case2 = rowSums(df2()[-1])
            ) %>% plot_func(plot_conf())
          )
          plot
        })

      return(reactive(input$case))
    }
  )
}



#' Function to plot components according to a configuration table
#'
#' @param df data.frame or tibble, first column of name `datetime` being of class datetime and rest of columns being numeric
#' @param plot_conf tibble of the plot configuration with columns `variable`, `label`, `color`, `fill` and `width` to pass to dygraph.
#' @param ylab character, label of y axis
#' @param ... extra arguments to pass to `dygraphs::dyOptions` function
#'
#' @return dygraphs plot
#' @export
#'
#' @importFrom dplyr %>%
#' @importFrom dygraphs dygraph dySeries dyLegend dyOptions dyCSS
#' @importFrom purrr transpose
#'
plot_components <- function(df, plot_conf, ylab = "kW", ...) {
  dyplot <- df %>% df_to_ts() %>% dygraph(group = "a", ylab = ylab)
  for (component in transpose(plot_conf)) {
    if (component$variable %in% names(df)) {
      dyplot <- dySeries(dyplot, component$variable, component$label, component$color,
                         fillGraph = component$fill, strokeWidth = component$width)
    }
  }
  dyplot %>%
    dyLegend(show = "onmouseover") %>%
    dyOptions(retainDateWindow = TRUE,
              useDataTimezone = TRUE,
              ...) %>%
    dyCSS("www/style.css")
}



