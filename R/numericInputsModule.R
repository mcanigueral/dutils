
# Module for several numericInput with UI enabling feature -------------------

#' UI function of numericInput module
#'
#' @param id character, module ID
#' @param inputs_tbl tibble, inputs characteristics
#' @param sliders boolean, whether to use sliders or numeric inputs
#'
#' @return shiny UI tagList
#' @export
#'
#' @importFrom shiny NS column sliderInput numericInput tagList
#' @importFrom purrr pmap
numericInputsUI <- function(id, inputs_tbl, sliders=FALSE) {
  ns <- NS(id)

  inputFunc <- ifelse(sliders, sliderInput, numericInput)

  # Minimum width=2, for other values width=3
  if (round(12/nrow(inputs_tbl)) == 2) {
    columns_width <- 2
  } else {
    columns_width <- 3
  }

  numericInputsList <- pmap(
    inputs_tbl,
    ~ column(columns_width, inputFunc(
      inputId = ns(..1), label = ..2, value = ..3, min = ..4, max = ..5, step = ..6)
    )
  )

  enabled_numericInputsList <- numericInputsList[inputs_tbl[['show']]]

  tagList(
    enabled_numericInputsList
  )
}


#' Server function of numericInput module
#'
#' @param id character, module ID
#' @param inputs_ids character vector of inputs to get values from
#' @param delay integer, number of milliseconds to delay an input change
#'
#' @return named list with reactive input values
#' @export
#'
#' @importFrom shiny moduleServer reactive debounce
#' @importFrom purrr map set_names
#'
numericInputsServer <- function(id, inputs_ids, delay=0) {
  moduleServer(
    id,
    function(input, output, session) {
      map(set_names(inputs_ids, inputs_ids), ~ reactive(input[[.x]]) %>% debounce(delay))
    }
  )
}



# Support for using this module --------------------------------------------

#' Get single component value
#'
#' @param id character, input ID
#' @param config_value numeric, initial value
#' @param numericInputs_module numericInputServer object of the module
#'
#' @return numeric input value
#'
get_component_value <- function(id, config_value, numericInputs_module) {
  if (is.null(numericInputs_module[[id]]())) {
    return( config_value )
  } else {
    return( numericInputs_module[[id]]() )
  }
}

#' Get components values from numericInput module
#'
#' @param inputs_tbl data.frame or tibble, with inputs configuration
#' @param numericInputs_module numericInputServer object of the module
#'
#' @return named list with input values
#' @export
#'
#' @importFrom purrr pmap
#'
get_components_values <- function(inputs_tbl, numericInputs_module) {
  values <- pmap(inputs_tbl, ~ get_component_value(..1, ..3, numericInputs_module))
  names(values) <- inputs_tbl[['input']]
  return( values )
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

