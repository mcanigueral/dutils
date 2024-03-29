
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
#' @importFrom dygraphs dygraphOutput
#'
caseSelectorUI <- function(id, tabs, height) {
  ns <- shiny::NS(id)
  shiny::tags$div(
    shinyWidgets::radioGroupButtons(ns("case"), NULL, tabs, justified = TRUE),
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
#' @param ... extra parameters passed to dyOptions for `df1` and `df2` plots (not comparison plot)
#'
#' @return reactive `case` module input
#' @export
#'
#' @importFrom dygraphs renderDygraph
#' @importFrom dplyr %>% tibble
#'
caseSelector <- function(id, df1, df2, plot_func, plot_conf, ...) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      output$graph <-
        renderDygraph({
          switch(
            as.integer(input$case),
            df1() %>% plot_func(plot_conf(), ...),
            df2() %>% plot_func(plot_conf(), ...),
            tibble(
              datetime = df1()[[1]],
              case1 = rowSums(df1()[-1]),
              case2 = rowSums(df2()[-1])
            ) %>% plot_func(plot_conf(), ...)
          )
        })

      return(shiny::reactive(input$case))
    }
  )
}

