# Indicators -------------------------------------------------------------

#' Obtain a list of energy indicators given the energy flows
#'
#' @param df data.frame or tibble, with columns `datetime`, `consumption`, `production` and `kg_co2_kwh` (in this order)
#' @param kg_co2_kwh factor of CO2 kg emissions per kWh of energy consumed from the distribution grid
#'
#' @return named list
#' @export
#'
#' @importFrom dplyr %>% mutate summarise
#' @importFrom rlang .data
#'
get_energy_kpis <- function(df, kg_co2_kwh = 0.5) {
  if (!('kg_co2_kwh' %in% names(df))) {
    df[['kg_co2_kwh']] <- kg_co2_kwh
  }
  # Energy = Power * resolution(h)
  resolution <- as.numeric(df$datetime[2] - df$datetime[1], unit='hours')

  df2 <- df %>%
    mutate(
      local = ifelse(.data$production > .data$consumption, .data$consumption, .data$production),
      imported = .data$consumption - .data$local,
      exported = .data$production - .data$local,
      kg_co2 = .data$kg_co2_kwh*.data$imported*resolution,
    )

  peak_to_grid_dttm <- df2$datetime[which(df2$exported == max(df2$exported, na.rm = T))][1]
  peak_from_grid_dttm <- df2$datetime[which(df2$imported == max(df2$imported, na.rm = T))][1]

  df2 %>%
    summarise(
      total_consumption = sum(.data$consumption*resolution, na.rm = T),
      total_production = sum(.data$production*resolution, na.rm = T),
      total_local = sum(.data$local*resolution, na.rm = T),
      total_exported = sum(.data$exported*resolution, na.rm = T),
      total_imported = sum(.data$imported*resolution, na.rm = T),
      selfconsumption = .data$total_local/.data$total_production,
      selfsufficiency = .data$total_local/.data$total_consumption,
      peak_to_grid = max(.data$exported, na.rm = T),
      peak_from_grid = max(.data$imported, na.rm = T),
      peak_to_grid_dttm = peak_to_grid_dttm,
      peak_from_grid_dttm = peak_from_grid_dttm,
      kg_co2 = sum(.data$kg_co2, na.rm = T)
    ) %>%
    as.list()
}

#' For each month independently, obtain a list of energy indicators given the energy flows
#'
#' @param df data.frame or tibble, with columns `datetime`, `consumption`, `production` and `kg_co2_kwh` (in this order)
#'
#' @return named list
#' @export
#'
#' @importFrom dplyr %>% mutate group_by select everything
#' @importFrom lubridate month
#' @importFrom tidyr nest
#' @importFrom purrr map_dfr
#' @importFrom rlang .data
#'
get_monthly_energy_kpis <- function(df) {
  d_nest <- df %>%
    mutate(month = month(.data$datetime)) %>%
    group_by(.data$month) %>%
    nest()
  map_dfr(d_nest$data, ~ get_energy_kpis(.x)) %>%
    mutate(month = d_nest$month) %>%
    select(.data$month, everything())
}


#' Compare KPIs (final kpis - original kpis)
#'
#' @param kpis_o named list with original kpis values
#' @param kpis_f named list with final kpis values
#'
#' @return named list
#' @export
#'
#' @importFrom purrr possibly map2
compare_kpis <- function(kpis_o, kpis_f) {
  possible_subtraction <- possibly(`-`, otherwise = NULL)
  map2(kpis_f, kpis_o, ~ possible_subtraction(.x, .y))
}
