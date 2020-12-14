
# Module for several infoBoxes -------------------

#' UI functions of kpis module
#'
#' @param id character, module ID
#'
#' @return shiny UI tagList
#' @export
#'
#' @importFrom shiny NS fluidRow column uiOutput
#'
kpisUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(12, uiOutput(ns('kpis')))
  )
}


#' Server function of kpis module
#'
#' @param id character, module ID
#' @param kpis_df data.frame or tibble with kpis configuration
#' @param kpis_values named list with kpis values
#'
#' @export
#'
#' @importFrom shiny moduleServer renderUI column
#' @importFrom shinydashboard renderInfoBox infoBox
#' @importFrom purrr transpose map
#'
kpisServer <- function(id, kpis_df, kpis_values) {
  moduleServer(
    id,
    function(input, output, session) {

      output[['kpis']] <- renderUI({

        val <- kpis_values()
        kpis_list <- transpose( kpis_df() )

        map(
          kpis_list,
          ~ column(
            as.integer(.x[['width']]),
            # getInfoBox(
            #   title = .x[['title']],
            #   value = paste(round(val[[.x[['variable']]]]/.x[['division']], .x[['digits']]), .x[['units']]),
            #   subtitle = eval(parse(text=.x[['subtitle']])),
            #   icon = icon(.x[['icon']], class = .x[['icon-class']]),
            #   color = .x[['color']]
            # )
            renderInfoBox({
              infoBox(
                title = .x[['title']],
                value = paste(round(val[[.x[['variable']]]]/.x[['division']], .x[['digits']]), .x[['units']]),
                subtitle = eval(parse(text=.x[['subtitle']])),
                icon = icon(.x[['icon']], class = .x[['icon-class']]),
                color = .x[['color']],
                fill = as.logical(.x[['fill']])
              )
            })
          )
        )
      })
    }
  )
}


# getInfoBox <- function(title, value, subtitle, icon, color) {
#   HTML(paste0(
#     '<div class="info-box" style= background:', color, '>',
#       '<span class="info-box-icon">', icon,
#         # '<i class="fa fa-bolt normal_size_icon"></i>',
#       '</span>',
#       '<div class="info-box-content">',
#         '<span class="info-box-text">', title, '</span>',
#         '<span class="info-box-number">', value, '</span>',
#         '<p>', subtitle, '</p>',
#       '</div>',
#     '</div>'
#   ))
# }
