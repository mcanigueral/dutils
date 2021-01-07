
# STEP LOAD -----------------------------------------------------------

#' Title
#'
#' @param df data.frame or tibble, first column of name `datetime` being of class datetime and rest of columns being numeric
#' @param colname character, column name where the step load should be detected
#' @param power numeric, value of power (units according to `df`) of the step load
#' @param enable_half logical, whether to consider half of power demand during indices before and after the step into the final step profile.
#'
#' @return data.frame or tibble
#' @export
#'
#' @importFrom dplyr lag lead
#'
detect_step_demand <- function(df, colname, power, enable_half = FALSE){
  interval_mins_factor <- 60/as.numeric(df[['datetime']][2], df[['datetime']][1], units='mins')
  power_demand <- df[[colname]]*interval_mins_factor
  active <- power_demand > power
  activation <- active & !lag(active, default = F) # Index of activation
  deactivation <- !active & lag(active, default = F) # Index of deactivation
  if (enable_half) {
    activation_half <- (power_demand > power/2) & lead(activation, default = F)
    deactivation_half <- (power_demand > power/2) & deactivation
    df[[paste0(colname, '_step')]] <- power*active + power/2*(activation_half + deactivation_half)
  } else {
    df[[paste0(colname, '_step')]] <- power*active
  }
  return( df )
}

