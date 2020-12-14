
# Module for case (static, flexible, comparison) radio selector --------------------------------------------------------

#' UI function for caseSelector module
#'
#' @param id character, module ID
#' @param tabs character vector, tabs names
#' @param height Dygraphs height. Must be a valid CSS unit (like "100%", "400px", "auto") or a number, which will be coerced to a string and have "px" appended.
#'
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
#' @param plot_func function to plot the dygraph output, being the first parameter the configuration tibble
#' @param plot_conf tibble of the plot configuration
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




