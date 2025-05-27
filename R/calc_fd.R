#' Calculate Fd from dT and dTmax time series
#'
#' @description calc_fd() calculates sap flow rate using the Granier equation
#'   from the dT and dTmax time series, considering heartwood correction.
#'
#' @param vctr_dt Vector of dT (the temperature difference between sap flow
#'   probes) time series. Missing values must be gap-filled previously.
#' @param vctr_dtmax Vector of the dTmax (the maximum temperature difference
#'   between sap flow probes) time series estimated by a specific method.
#' @param alpha Parameter in Granier equation (unit: m/s)
#' @param beta Parameter in Granier equation (dimensionless)
#' @param do_heartwood_correction Logical. Do Heartwood correction described in
#'   Clearwater et al. (1999) or not?
#' @param ratio_conductive Ratio of conductive versus non-conductive length
#'   (dimensionless)
#'
#' @examples
#' data(dt_gf)
#' data(dtmax)
#'
#' dt <- dt_gf$dt
#' dtmax <- dtmax$dtmax_sp
#'
#' # Calculate Fd from data
#' result <- calc_fd(dt, dtmax)
#'
#' @include utils.R utils-pipe.R
#' @export

calc_fd <-
  function(vctr_dt, vctr_dtmax, alpha = 1.19 * 10^{-4}, beta = 1.231,
           do_heartwood_correction = FALSE, ratio_conductive = NULL) {
    ## avoid "No visible binding for global variable" notes
    dt <- NULL
    dtmax <- NULL
    dt_sw <- NULL
    fd <- NULL

    ## check input values
    data_all <- data.frame(dt = vctr_dt,
                           dtmax = vctr_dtmax)

    if(do_heartwood_correction) {
      vctr_fd <-
        data_all %>%
        dplyr::mutate(dt_sw = (dt - (1 - ratio_conductive) * dtmax) /
                        ratio_conductive,
                      fd = ifelse(dtmax - dt_sw >= 0,
                                  alpha * ((dtmax - dt_sw) / dt_sw)^beta,
                                  0)) %>%
        dplyr::pull(fd)
    } else {
      vctr_fd <-
        data_all %>%
        dplyr::mutate(fd = ifelse(dtmax - dt >= 0,
                                  alpha * ((dtmax - dt) / dt)^beta, 0)) %>%
        dplyr::pull(fd)
    }

    return(vctr_fd)
  }
