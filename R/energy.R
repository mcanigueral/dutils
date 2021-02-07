
# Energy flows ------------------------------------------------------------

#' Get energy balance time-series
#'
#' Input data frame must have columns `consumption`, `production`.
#' Output data frame has extra columns `balance`, `local`, `imported`, `exported`.
#' Column `balance` is positive when there is more production than consumption.
#'
#' @param df data.frame or tibble, with columns `datetime`, `consumption`, `production`
#'
#' @return tibble or data.frame
#' @export
#'
#' @importFrom dplyr %>% mutate
#' @importFrom rlang .data
#'
get_energy_balance <- function(df) {
  df %>%
    mutate(
      balance = .data$production - .data$consumption,
      local = ifelse(.data$balance > 0, .data$consumption, .data$production),
      imported = .data$consumption - .data$local,
      exported = .data$production - .data$local,
    )
}



# Indicators -------------------------------------------------------------

#' Obtain a list of energy indicators given the energy flows
#'
#' @param df data.frame or tibble, with columns `datetime`, `consumption`, `production` and `kg_co2_kwh`
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
    get_energy_balance() %>%
    mutate(
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
#' @param df data.frame or tibble, with columns `datetime`, `consumption`, `production` and `kg_co2_kwh`
#'
#' @return named list
#' @export
#'
#' @importFrom dplyr %>% mutate group_by
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
  map_dfr(
    set_names(d_nest$data, d_nest$month),
    ~ get_energy_kpis(.x),
    .id = 'month'
  )
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



# Solar from PVGIS --------------------------------------------------------

#' Get TMY from PVGIS given the latitude and longitude coordinates.
#'
#' For more information visit https://ec.europa.eu/jrc/en/pvgis
#'
#' @param lat numeric, latitude coordinate
#' @param lon numeric, longitude coordinate
#'
#' @return tibble
#' @export
#'
#' @importFrom httr GET content
#' @importFrom dplyr bind_rows
#'
get_tmy <- function(lat=52.370, lon=4.908) {
  url <- paste0(
    "https://re.jrc.ec.europa.eu/api/tmy?",
    "lat=", lat, "&", "lon=", lon, "&",
    "outputformat=json"
  )
  response <- httr::GET(url)
  content <- httr::content(response)
  return(
    bind_rows(content$outputs$months_selected)
  )
}

#' Get PV generation from PVGIS given the latitude and longitude coordinates and PV installation characteristics.
#'
#' For more information visit https://ec.europa.eu/jrc/en/pvgis.
#' For parameters meaning visit https://ec.europa.eu/jrc/en/PVGIS/docs/noninteractive
#'
#' @param year integer, year for the returned time-series data frame
#' @param lat numeric, latitude coordinate
#' @param lon numeric, longitude coordinate
#' @param database character, PVGIS database
#' @param kWp numeric, PV installation kWp
#' @param loss numeric, PV installation loss
#' @param tilt numeric, PV panels inclination angle (in degrees)
#' @param azimuth numeric, PV panels orientation (0=south, 90=west, -90=east)
#'
#' @return tibble
#' @export
#'
#' @importFrom httr GET content
#' @importFrom dplyr bind_rows %>% mutate select filter
#' @importFrom lubridate as_datetime minutes month year
#' @importFrom rlang .data
#' @importFrom purrr pmap_dfr
#'
get_pv_timeseries <- function(year=2021, lat=52.370, lon=4.908, database = "PVGIS-SARAH", kWp=1, loss=14, tilt=37, azimuth=0) {
  message(paste('Getting PV data with: tilt =', tilt, ', azimuth =', azimuth))
  url <- paste0(
    "https://re.jrc.ec.europa.eu/api/seriescalc?",
    "lat=", lat, "&",
    "lon=", lon, "&",
    "raddatabase=", database, "&",
    "pvcalculation=1&",
    "peakpower=", kWp, "&",
    "mountingplace=building&",
    "loss=", loss, "&",
    "angle=", tilt, "&",
    "aspect=", azimuth, "&",
    "outputformat=json"
  )

  response <- httr::GET(url)
  content <- httr::content(response)
  timeseries <- bind_rows(content$outputs$hourly) %>%
    mutate(
      datetime = as_datetime(.data$time, format = "%Y%m%d:%H%M", tz = "UTC") - minutes(11),
      kW = .data$P/1000
    ) %>%
    select(.data$datetime, .data$kW)

  tmy <- get_tmy(lat, lon)

  pv_tmy <- pmap_dfr(
    tmy,
    ~ filter(timeseries, lubridate::month(.data$datetime) == ..1, lubridate::year(.data$datetime) == ..2)
  )
  lubridate::year(pv_tmy$datetime) <- year

  return( pv_tmy )
}


#' Get PV generation from PVGIS given the latitude and longitude coordinates and PV installation characteristics.
#'
#' For more information visit https://ec.europa.eu/jrc/en/pvgis.
#' For parameters meaning visit https://ec.europa.eu/jrc/en/PVGIS/docs/noninteractive
#'
#' @param year integer, year for the returned time-series data frame
#' @param lat numeric, latitude coordinate
#' @param lon numeric, longitude coordinate
#' @param database character, PVGIS database
#' @param loss numeric, PV installation loss
#' @param panels_tbl tibble, with columns `varname`, `tilt`, `azimouth` and `kWp`
#'
#' @return tibble
#' @export
#'
#' @importFrom dplyr bind_rows select sym mutate %>% everything
#' @importFrom purrr pmap_dfc
#' @importFrom rlang .data
#'
get_multiple_pv_timeseries <- function(year=2021, lat=52.370, lon=4.908, database = "PVGIS-SARAH", loss=14, panels_tbl) {
  solar_pvgis <- pmap_dfc(
    panels_tbl,
    ~ get_pv_timeseries(year = year, lat=lat, lon=lon, kWp=..4, loss=loss, tilt=..2, azimuth=..3) %>%
      select(!!sym(..1) := .data$kW)
  ) %>%
    mutate(datetime = get_datetime_seq(year, "UTC", 60, fullyear=T)) %>%
    select(.data$datetime, everything())
}
