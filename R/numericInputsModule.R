
# Module for several numericInput with UI enabling feature -------------------

#' UI function of numericInput module
#'
#' @param id character, module ID
#'
#' @return shiny UI tagList
#' @export
#'
#' @importFrom shiny NS uiOutput
#'
numericInputsUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns('ui'))
}


#' Server function of numericInput module
#'
#' @param id character, module ID
#' @param inputs_tbl tibble, inputs characteristics
#' @param sliders boolean, whether to use sliders or numeric inputs
#' @param delay integer, number of milliseconds to delay an input change
#'
#' @return named list with reactive input values
#' @export
#'
#' @importFrom shiny moduleServer reactive debounce column sliderInput numericInput
#' @importFrom purrr pmap map set_names
#'
numericInputsServer <- function(id, inputs_tbl, sliders = FALSE, delay=0) {
  moduleServer(
    id,
    function(input, output, session) {

      output[['ui']] <- renderUI({
        ns <- session$ns
        inputFunc <- ifelse(sliders, sliderInput, numericInput)

        if (round(12/sum(inputs_tbl[["show"]])) <= 2) {
          columns_width <- 2
        } else {
          columns_width <- 3
        }

        pmap(
          inputs_tbl[inputs_tbl[["show"]], ],
          ~ column(
            columns_width,
            inputFunc(
              inputId = ns(..1), label = ..2, value = ..3, min = ..4, max = ..5, step = ..6
            )
          )
        )
      })

      # return(reactive(pmap(
      #   inputs_tbl[c('input', 'value', 'show')],
      #   ~ reactive(get_input_valid_value(..1, ..2, ..3, input, delay))
      # )))

      inputs_list <- map(
        set_names(inputs_tbl[["input"]], inputs_tbl[["input"]]),
        ~ reactive(input[[.x]]) %>% debounce(delay)
      )
      return(reactive(get_inputs_valid_values(inputs_tbl, inputs_list)))

    })
}



# Support for using this module --------------------------------------------

#' Get single input value
#'
#' If the component is not in the inouts list, then the returned value is the
#' configuration value.
#'
#' @param id character, input ID
#' @param config_value numeric, initial value
#' @param inputs_list `input` object of the module
#'
#' @return numeric input value
#'
get_input_valid_value <- function (id, config_value, inputs_list) {
  if (is.null(inputs_list[[id]]())) {
    return(config_value)
  } else {
    return(inputs_list[[id]]())
  }
}
# get_input_valid_value <- function(input_name, config_value, show, module_inputs, delay) {
#   if (show) {
#     print(paste(input_name, input[[input_name]]))
#     return(debounce(input[[input_name]], delay))
#   } else {
#     print(paste(input_name, config_value))
#     return(config_value)
#   }
# }


#' Get numeric inputs values from numericInput module
#'
#' @param inputs_tbl data.frame or tibble, with inputs configuration
#' @param inputs_list `input` object of the module
#'
#' @return named list with input values
#'
#' @importFrom purrr pmap
#'
get_inputs_valid_values <- function (inputs_tbl, inputs_list) {
  values <- pmap(inputs_tbl, ~ get_input_valid_value(..1, ..3, inputs_list))
  names(values) <- inputs_tbl[["input"]]
  return(values)
}


#' Update a time-series column according to component input
#'
#' @param df data.frame or tibble, being `datetime` the first column followed by numeric and UNITARY variables
#' @param inputs named list with the name and size of each component
#' @param prefix character, prefix of all inputs names
#' @param input_factor numeric, factor to apply to each input value when multiplying to the unitary profile.
#'
#' @return tibble, resulting of multiplying unitary vectors by it's corresponding component size
#' @export
#'
#' @details
#' The names of `inputs` parameter should correspond to `df` variables after splitting the `suffix`.
#' In other words: `inputs` = `prefix` + `names(df)` (of course without considering `datetime` column in `df`)
#' The returned tibble is the result of `` df` * `inputs` * `prefix``
#'
update_components_by_inputs <- function(df, inputs, prefix, input_factor=1) {
  if (length(inputs) == 0) return( df )
  for (name in names(inputs)) {
    colname <- strsplit(name, prefix)[[1]][2]
    df[[colname]] <- df[[colname]]*inputs[[name]]*input_factor
  }
  return( df )
}

