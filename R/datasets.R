#' Raw dT time series with various artifacts
#'
#' Dataset consisting of the time series of a raw dT (the temperature
#' difference between thermal dissipation sap flow probes), meteorological
#' factors, and soil water content at Lambir Hills National Park, Malaysia.
#' Missing values are represented as -9999.
#'
#' @format A data frame with 17520 rows and 8 variables:
#' \describe{
#'   \item{time}{Timestamp of the measurement end timing}
#'   \item{dt}{Raw dT (degrees Celsius)}
#'   \item{p}{Precipitation (mm)}
#'   \item{sw_in}{Global solar radiation (W m-2)}
#'   \item{ta}{Air temperature (degrees Celsius)}
#'   \item{vpd}{Vapor pressure deficit (hPa)}
#'   \item{ws}{Horizontal wind speed (m s-1)}
#'   \item{swc}{Soil water content (m3 m-3)}
#' }
#'
#' @docType data
#' @keywords datasets
#' @name dt_noisy
#' @usage data(dt_noisy)
#' @source Meteorological factors and soil water content data were provided by
#'  Dr. Tomonori Kume [ORCiD: 0000-0001-6569-139X]
"dt_noisy"


#' Quality-controlled and gap-filled dT time series
#'
#' Dataset consisting of the time series of the quality-controlled and
#' gap-filled dT (the temperature difference between thermal dissipation sap
#' flow probes), meteorological factors, and soil water content at Lambir Hills
#' National Park, Malaysia.
#'
#' @format A data frame with 17520 rows and 8 variables:
#' \describe{
#'   \item{time}{Timestamp of the measurement end timing}
#'   \item{dt}{Quality-controlled and gap-filled dT (degrees Celsius)}
#'   \item{p}{Precipitation (mm)}
#'   \item{sw_in}{Global solar radiation (W m-2)}
#'   \item{ta}{Air temperature (degrees Celsius)}
#'   \item{vpd}{Vapor pressure deficit (hPa)}
#'   \item{ws}{Horizontal wind speed (m s-1)}
#'   \item{swc}{Soil water content (m3 m-3)}
#' }
#'
#' @docType data
#' @keywords datasets
#' @name dt_gf
#' @usage data(dt_gf)
#' @source Meteorological factors and soil water content data were provided by
#'  Dr. Tomonori Kume [ORCiD: 0000-0001-6569-139X]
"dt_gf"


#' dTmax time series estimated by multiple methods
#'
#' Dataset consisting of the time series of the dTmax (the maximum temperature
#' difference between thermal dissipation sap flow probes under zero-flow
#' conditions) calculated by the successive predawn (SP), daily predawn (PD),
#' moving window (MW), double regression (DR), and environmental dependent (ED)
#' methods.
#'
#' @format A data frame with 17520 rows and 6 variables:
#' \describe{
#'   \item{time}{Timestamp of the measurement end timing}
#'   \item{dtmax_sp}{dTmax estimated by the SP method (degrees Celsius)}
#'   \item{dtmax_pd}{dTmax estimated by the PD method (degrees Celsius)}
#'   \item{dtmax_mw}{dTmax estimated by the MW method (degrees Celsius)}
#'   \item{dtmax_dr}{dTmax estimated by the DR method (degrees Celsius)}
#'   \item{dtmax_ed}{dTmax estimated by the ED method (degrees Celsius)}
#' }
#'
#' @docType data
#' @keywords datasets
#' @name dtmax
#' @usage data(dtmax)
"dtmax"
