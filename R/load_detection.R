
# STEP LOAD -----------------------------------------------------------

#' Detect step load
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
detect_step_load <- function(df, colname, power, enable_half = FALSE){
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


#' Simualte step load
#'
#' @param time_start datetime value to start the simulation
#' @param time_end datetime value to finish the simulation
#' @param mins_on numeric, number of minutes with step ON
#' @param mins_off numeric, number of minutes with step OFF
#' @param power numeric, value of power of the step load (in kW)
#' @param resolution numeric, minutes interval of the datetime sequence
#'
#' @return data.frame or tibble
#' @export
#'
#' @importFrom dplyr tibble bind_rows filter
#' @importFrom lubridate minutes
#'
simulate_step_load <- function(time_start, time_end, mins_on, mins_off, power, resolution) {

  step_period <- mins_on + mins_off
  simulation_period <- difftime(time_end, time_start, units = 'mins')[[1]]
  n_steps <- ceiling(simulation_period/step_period)

  simulation_df <- tibble()

  for (tstart in seq.POSIXt(from = as_datetime(time_start), by = paste(step_period, 'min'), length.out = simulation_period)) {
    new_step_cycle <- bind_rows(
      tibble(
        datetime = seq.POSIXt(as_datetime(tstart), by = paste(resolution, 'min'), length.out = mins_off),
        power = 0
      ),
      tibble(
        datetime = seq.POSIXt(as_datetime(tstart)+minutes(mins_off), by = paste(resolution, 'min'), length.out = mins_on),
        power = power
      )
    )

    simulation_df <- bind_rows(simulation_df, new_step_cycle)
  }

  return(
    filter(simulation_df, .data$datetime < time_end)
  )
}

# -------------------------------------------------------------------------


