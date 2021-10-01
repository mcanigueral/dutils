
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
  css_file <- system.file("www", "tooltip.css", package = "dutils")
  shiny::fluidRow(
    shiny::includeCSS(path = css_file),
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
                value = infobox_value(
                  paste(round(val[[.x[['variable']]]]/.x[['division']], .x[['digits']]), .x[['units']]),
                  tip = eval(parse(text=.x[['info']]))
                ),
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


infobox_value <- function(value, tip) {
  if (is.null(tip)) {
    return(value)
  } else {
    shiny::tags$div(
      shiny::tags$p(value, style = "display: inline-block;vertical-align:top;margin-bottom:0"),
      info_tip(tip, style = "display: inline-block;vertical-align:top;")
    )
  }
}

info_tip <- function(...) {
  shiny::tags$div(
    class = "tooltip2",
    shiny::HTML('<i class="fa fa-info-circle" style="color:#4682B4; font-size: 16px;"></i>'),
    shiny::tags$span(class="tooltiptext", style = "font-size: 14px; font-weight: normal; line-height: 120%;", ...)
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
