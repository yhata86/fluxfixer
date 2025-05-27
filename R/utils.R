#' Obtain time interval of input time vector
#'
#' @description `get_interval()` retrieves time interval of input time vector
#'   and checks whether the format of the time vector is acceptable for
#'   the successive process.
#'
#' @param vctr_time Timestamp vector (POSIXct or POSIXt class)


get_interval <- function(vctr_time) {
  if(lubridate::is.POSIXt(vctr_time) != TRUE) {
    stop("input time vector must be a POSIXct or POSIXt class")
  }

  if(length(vctr_time[is.na(vctr_time)]) >= 1) {
    stop("one or more NA values exist in the input time vector")
  }

  max_interval <- vctr_time %>% diff() %>% as.numeric() %>% max()
  min_interval <- vctr_time %>% diff() %>% as.numeric() %>% min()

  if(all.equal(max_interval, min_interval) != TRUE) {
    stop("input time vector does not have equal intervals")
  }

  return(max_interval)
}


#' Obtain TOA global radiation time series
#'
#' @description `calc_global_radiation_toa()` calculates TOA (top of atmosphere)
#'   global radiation time series at specific place based on Campbell and
#'   Norman (1998)
#'
#' @param vctr_time Timestamp vector (POSIXct or POSIXt class).
#' @param time_tail Timestamp (POSIXct or POSIXt class) in local time,
#'   which indicates the end time of the needed time series.
#' @param latitude Latitude (unit: degree).
#' @param longitude Longitude (unit: degree).
#' @param std_meridian Standard meridian (unit: W/m2).
#' @param solar_const Solar constant (unit: W/m2).
#' @param sbeta_min Minimum solar elevation angle (unit: degree).
#'   If the calculated solar elevation angle is less than this value,
#'   the corresponding TOA global radiation becomes zero.

calc_global_radiation_toa <- function(vctr_time, lat, lon, std_meridian,
                                      solar_const = 1365, sbeta_min = 0.001) {
  DOY <- lubridate::yday(vctr_time)
  HourMin <- lubridate::hour(vctr_time) + lubridate::minute(vctr_time) / 60

  lct <- -24 * (std_meridian - lon) / 360
  fff <- pi * (279.575 + DOY * 0.9856) / 180
  etim <-
    (-104.7 * sin(fff) + 596.2 * sin(2*fff) + 4.3 * sin(3 * fff) -
       12.7 * sin(4 * fff) - 429.3 * cos(fff) - 2 * cos(2*fff) +
       19.3 * cos(3 * fff)) / 3600
  snoon <- 12 - lct - etim
  sdelt <-
    asin(0.398 * sin(4.871 + 2 * pi * DOY / 365 + 0.033 *
                       sin(2 * pi * DOY / 365)))
  sbeta <-
    asin(sin(lat / 180 * pi) * sin(sdelt) +
           cos(lat / 180 * pi) * cos(sdelt) *
           cos(-2 * pi * ((HourMin - 0.25) - snoon) / 24))

  sbeta <- ifelse(sin(sbeta) <= sbeta_min, 0, sbeta)
  global_radiation_toa <- solar_const * sin(sbeta)

  return(global_radiation_toa)
}


calc_global_radiation_toa(lubridate::ymd_hm("2025/01/01 12:30"), 4.33, 113.83, 120)
calc_global_radiation_toa(lubridate::ymd_hm("2025/06/21 12:00"), 35, 140, 135)





