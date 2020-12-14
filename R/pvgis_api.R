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
  print(paste('Getting PV data with: tilt =', tilt, ', azimouth =', azimuth))
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
#' @param kWp numeric, PV installation kWp
#' @param loss numeric, PV installation loss
#' @param orientation_tbl tibble, with columns `varname`, `tilt` and `azimouth`
#'
#' @return tibble
#' @export
#'
#' @importFrom httr GET content
#' @importFrom dplyr bind_rows select sym mutate %>%
#' @importFrom purrr pmap_dfc
#' @importFrom rlang .data
#'
get_multiple_pv_timeseries <- function(year=2021, lat=52.370, lon=4.908, database = "PVGIS-SARAH", kWp=1, loss=14, orientation_tbl) {
  solar_pvgis <- pmap_dfc(
    orientation_tbl,
    ~ get_pv_timeseries(year = year, lat=lat, lon=lon, kWp=kWp, loss=loss, tilt=..2, azimuth=..3) %>%
      select(!!sym(..1) := .data$kW)
  ) %>%
    mutate(datetime = get_datetime_seq(year, "UTC", 60, fullyear=T)) %>%
    select(.data$datetime, everything())
}

