
# Module for several infoBoxes -------------------

#' UI functions of kpis module
#'
#' @param id character, module ID
#'
#' @return shiny UI tagList
#' @export
#'
kpisUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidRow(
    shiny::column(12, shiny::uiOutput(ns('kpis')))
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
#' @importFrom purrr transpose map
#'
kpisServer <- function(id, kpis_df, kpis_values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      output[['kpis']] <- shiny::renderUI({

        val <- kpis_values()
        kpis_list <- transpose( kpis_df() )

        map(
          kpis_list,
          ~ shiny::column(
            as.integer(.x[['width']]),
            # getInfoBox(
            #   title = .x[['title']],
            #   value = paste(round(val[[.x[['variable']]]]/.x[['division']], .x[['digits']]), .x[['units']]),
            #   subtitle = eval(parse(text=.x[['subtitle']])),
            #   icon = icon(.x[['icon']], class = .x[['icon-class']]),
            #   color = .x[['color']]
            # )
            shinydashboard::renderInfoBox({
              shinydashboard::infoBox(
                title = .x[['title']],
                value = paste(round(val[[.x[['variable']]]]/.x[['division']], .x[['digits']]), .x[['units']]),
                subtitle = stringr::str_replace(eval(parse(text=.x[['subtitle']])), 'NaN', '0'),
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
