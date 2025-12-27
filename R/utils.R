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
#' @param lat Latitude (unit: degree).
#' @param lon Longitude (unit: degree).
#' @param std_meridian Standard meridian (unit: degree).
#' @param solar_const Solar constant (unit: W/m2).
#' @param sbeta_min Minimum solar elevation angle (unit: degree).
#'   If the calculated solar elevation angle is less than this threshold,
#'   the corresponding TOA global radiation becomes zero.
#' @examples
#'   calc_global_radiation_toa(seq(as.POSIXct("2025/01/01"),
#'                                 as.POSIXct("2025/01/02"), by = "30 min"),
#'                             4.33, 133.83, 120)
#'
#' @export

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


#' Obtain the number of data points without missing values
#'
#' @description `n_valid()` retrieves the number of data points without missing
#'   values.
#'
#' @param vctr Vector to be evaluated
#' @param label_err Error value label representing missing value

n_valid <-
  function(vctr, label_err = -9999) {
    vctr <- ifelse(vctr == label_err, NA, vctr)
    return(length(stats::na.omit(vctr)))
  }


#' Check small signal dent by peak position
#'
#' @description `check_small_dent()` detects small signal dent periods
#'   by peak position.
#'
#' @param vctr_time Vector to be evaluated
#' @param vctr_dt_z Vector to be evaluated
#' @param vctr_dt_avg Vector to be evaluated
#' @param vctr_dt_sd Vector to be evaluated
#' @param wndw_size_dent Integer determining the moving window size
#' @param inv_sigma Numerical value for Gaussian window
#' @param label_err Error value label representing missing value

check_small_dent <-
  function(vctr_time, vctr_dt_z, vctr_dt_avg, vctr_dt_sd,
           wndw_size_dent = 720, inv_sigma = 0.01, label_err = -9999) {
    ## avoid "No visible binding for global variable" notes
    time <- NULL
    dT_z <- NULL
    dT_sd <- NULL
    dT_avg <- NULL
    dT_avg_smth <- NULL
    dT_avg_smth_diff1 <- NULL
    flag_dT_avg_peak <- NULL
    dT_sd_smth <- NULL
    dT_sd_smth_diff1 <- NULL
    flag_dT_sd_peak <- NULL
    dT_avg_smth_diff2 <- NULL
    dT_sd_smth_diff2 <- NULL
    dT_sd_head <- NULL
    . <- NULL
    dT_sd_tail <- NULL
    dT_sd_peak <- NULL

    vctr_time_start <- NULL
    vctr_time_peak <- NULL
    vctr_time_end <- NULL
    vctr_ratio <- NULL

    data <-
      data.frame(time = vctr_time,
                 dT_z = vctr_dt_z,
                 dT_avg = vctr_dt_avg,
                 dT_sd = vctr_dt_sd) %>%
      dplyr::na_if(label_err)

    time_mea_start <-
      data %>%
      dplyr::filter(!is.na(dT_z)) %>%
      dplyr::slice_head() %>%
      dplyr::pull(time)

    time_mea_end <-
      data %>%
      dplyr::filter(!is.na(dT_z)) %>%
      dplyr::slice_tail() %>%
      dplyr::pull(time)

    interval_time <- vctr_time %>% diff() %>% as.numeric() %>% max()

    ## Gaussian filter
    g_filter <- gsignal::gaussian(wndw_size_dent, inv_sigma)
    g_filter <- g_filter / sum(g_filter)

    data <-
      data %>%
      dplyr::mutate(dT_avg_smth = ifelse(!is.na(dT_z), dT_avg, NA),
                    dT_avg_smth = zoo::na.approx(dT_avg_smth, na.rm = FALSE),
                    dT_sd_smth = ifelse(!is.na(dT_z), dT_sd, NA),
                    dT_sd_smth = zoo::na.approx(dT_sd_smth, na.rm = FALSE)) %>%
      tidyr::fill(c(dT_avg_smth, dT_sd_smth), .direction = "updown") %>%
      dplyr::mutate(dT_avg_smth = gsignal::conv(dT_avg_smth, g_filter,
                                                shape = "same"),
                    dT_avg_smth_diff1 = dT_avg_smth - stats::lag(dT_avg_smth),
                    dT_avg_smth_diff2 = dT_avg_smth_diff1 -
                      stats::lag(dT_avg_smth_diff1),
                    # flag_dT_avg_peak = ifelse(dT_avg_smth_diff1 == 0, 1, 0),
                    # flag_dT_avg_peak =
                    #   ifelse(dT_avg_smth_diff1 *
                    #            dplyr::lead(dT_avg_smth_diff1) < 0,
                    #          1, flag_dT_avg_peak),
                    flag_dT_avg_peak =
                      ifelse(dT_avg_smth_diff1 == 0 &
                               dT_avg_smth_diff1 *
                               dplyr::lead(dT_avg_smth_diff1) < 0, 1, 0),
                    dT_sd_smth = gsignal::conv(dT_sd_smth, g_filter,
                                               shape = "same"),
                    dT_sd_smth_diff1 = dT_sd_smth - stats::lag(dT_sd_smth),
                    dT_sd_smth_diff2 = dT_sd_smth_diff1 -
                      stats::lag(dT_sd_smth_diff1),
                    # flag_dT_sd_peak = ifelse(dT_sd_smth_diff1 == 0, 1, 0),
                    # flag_dT_sd_peak =
                    #   ifelse(dT_sd_smth_diff1 *
                    #            dplyr::lead(dT_sd_smth_diff1) < 0,
                    #          1, flag_dT_sd_peak),
                    flag_dT_sd_peak =
                      ifelse(dT_sd_smth_diff1 == 0 &
                               dT_sd_smth_diff1 *
                               dplyr::lead(dT_sd_smth_diff1) < 0, 1, 0))

    ## detect peak position
    vctr_time_avg_upeak <-
      data %>%
      dplyr::filter(flag_dT_avg_peak == 1 & dT_avg_smth_diff2 < 0) %>%
      dplyr::pull(time)

    vctr_time_sd_peak <-
      data %>%
      dplyr::filter(flag_dT_sd_peak == 1) %>%
      dplyr:: pull(time)

    vctr_time_sd_dpeak <-
      data %>%
      dplyr::filter(flag_dT_sd_peak == 1 & dT_sd_smth_diff2 > 0) %>%
      dplyr::pull(time)

    if(length(vctr_time_sd_dpeak) <= 1) {
      message("--- There are less than two peaks. Skip dent detection.")
      data.frame(time_start = vctr_time_start,
                 time_peak = vctr_time_peak,
                 time_end = vctr_time_end,
                 ratio = vctr_ratio) %>%
        return()
    } else {
      ## detect possible damping periods
      for (i in 1:length(vctr_time_sd_dpeak)) {
        target_time_sd_dpeak <- vctr_time_sd_dpeak[i]
        num_time_sd_peak <- which(vctr_time_sd_peak == target_time_sd_dpeak)

      if(num_time_sd_peak == 1) {
          damp_head <- time_mea_start + interval_time
          damp_tail <- vctr_time_sd_peak[num_time_sd_peak + 1] - interval_time
        } else if(num_time_sd_peak == length(vctr_time_sd_peak)) {
          damp_head <- vctr_time_sd_peak[num_time_sd_peak - 1] + interval_time
          damp_tail <- time_mea_end - interval_time
        } else {
          damp_head <- vctr_time_sd_peak[num_time_sd_peak - 1] + interval_time
          damp_tail <- vctr_time_sd_peak[num_time_sd_peak + 1] - interval_time
        }

        num_time_avg_peak <-
          which(vctr_time_avg_upeak >= damp_head &
                  vctr_time_avg_upeak <= damp_tail)

        df_damp <-
          data %>%
          dplyr::filter(time >= damp_head & time <= damp_tail)

        dT_sd_head <- dplyr::slice_head(df_damp) %>% dplyr::pull(dT_sd_smth)
        dT_sd_tail <- dplyr::slice_tail(df_damp) %>% dplyr::pull(dT_sd_smth)
        dT_sd_peak <-
          df_damp %>%
          dplyr::filter(., time == target_time_sd_dpeak) %>%
          dplyr::pull(dT_sd_smth)

        dT_sd_peak_ratio <-
          round((dT_sd_peak / dT_sd_head + dT_sd_peak / dT_sd_tail) / 2,
                digits = 5)

        if(length(num_time_avg_peak) >= 1) {
          vctr_time_start <- c(vctr_time_start, damp_head)
          vctr_time_peak <- c(vctr_time_peak, target_time_sd_dpeak)
          vctr_time_end <- c(vctr_time_end, damp_tail)
          vctr_ratio <- c(vctr_ratio, dT_sd_peak_ratio)
        }
      }

      ## convert integer to POSIX
      attributes(vctr_time_start) <- attributes(target_time_sd_dpeak)
      attributes(vctr_time_peak) <- attributes(target_time_sd_dpeak)
      attributes(vctr_time_end) <- attributes(target_time_sd_dpeak)

      data.frame(time_start = vctr_time_start,
                 time_peak = vctr_time_peak,
                 time_end = vctr_time_end,
                 ratio = vctr_ratio) %>%
        return()
    }
  }


