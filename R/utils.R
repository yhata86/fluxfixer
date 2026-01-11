#' Obtain time interval of input timestamp vector
#'
#' @description `get_interval()` retrieves the time interval of the input
#'  timestamp vector and checks whether the format of the time vector is
#'  acceptable for the successive process.
#'
#' @param vctr_time A timestamp vector of class POSIXct or POSIXt. The
#'  timestamps must be equally spaced and arranged chronologically.
#'
#' @returns
#' A numeric (minutes) indicating the time interval of the input timestamp
#'  vector.
#'
#' @author Yoshiaki Hata

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


#' Calculate global solar radiation time series at TOA
#'
#' @description `calc_sw_in_toa()` obtains incident global solar radiation
#'  time series at TOA (top of atmosphere) at a specific location by
#'  calculating solar elevation angle estimated from the equations of Campbell
#'  and Norman (1998).
#'
#' @inheritParams get_interval
#' @param lat A numeric value (degrees) between -90 and 90, indicating the
#'  latitude of the specific location.
#' @param lon A numeric value (degrees) between -180 and 180, indicating the
#'  longitude of the specific location.
#' @param std_meridian A numeric value (degrees) between -180 and 180,
#'  indicating the standard meridian of the specific location.
#' @param solar_const A positive value (W m-2) indicating the solar constant.
#'  Default is 1365 (W m-2).
#' @param sbeta_min A threshold value of the solar elevation angle (degrees).
#'  If the calculated solar elevation angle is less than this threshold, the
#'  corresponding global solar radiation becomes zero.
#'
#' @returns
#' A vector of the global solar radiation at TOA (W m-2). The length of the
#' vector matches that of the input timestamp vector.
#'
#' @examples
#' ## Make a timestamp vector
#' timezone <- "Etc/GMT-8"
#' time <- seq(as.POSIXct("2026/01/01", tz = timezone),
#'  as.POSIXct("2026/01/02", tz = timezone), by = "30 min")
#'
#' ## Obtain global solar radiation at Lambir Hills National Park in Malaysia
#' result <- calc_sw_in_toa(time, 4.201007, 114.039079, 120)
#'
#' @author Yoshiaki Hata
#'
#' @export

calc_sw_in_toa <-
  function(vctr_time, lat, lon, std_meridian, solar_const = 1365,
           sbeta_min = 0.001) {
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
#' @param vctr A vector to be evaluated.
#' @param label_err A numeric value representing a missing value in the input
#'  vector. Default is -9999.
#'
#' @returns
#' An integer indicating the number of the input vector elements without
#'  missing values.
#'
#' @author Yoshiaki Hata

n_valid <-
  function(vctr, label_err = -9999) {
    vctr <- ifelse(vctr == label_err, NA, vctr)
    return(length(stats::na.omit(vctr)))
  }


#' Detect periods when short-term signal attenuation occurs
#'
#' @description `check_short_attenuation()` detects periods of temporary signal
#'  attenuation by the upward (or downward) peak position of the smoothed
#'  average (or standard deviation) time series.
#'
#' @details
#' In some cases, the dT time series may yield a signal that is attenuated for
#' only a short period, for example, when rainfall continues for days, causing
#' the moving window mean (or standard deviation) to increase (or decrease).
#' In such cases, normalization will cause the Z-score time series immediately
#' before and after the rainfall to be unnaturally distorted, hindering the
#' construction of the random-forest model. This function can detect periods
#' when the moving window average has an upward peak, and the moving window
#' standard deviation has a downward peak simultaneously.
#'
#' First, the average and standard deviation time series are interpolated if
#' they contain missing values. Second, they are smoothed by convolution with a
#' Gaussian window specified by the user through the parameters
#' `wndw_size_conv` and `inv_sigma_conv`. Third, the first-order and
#' second-order differences of both smoothed time series are calculated, which
#' determine the upward peak positions of the average and the downward peak
#' positions of the standard deviation. Finally, possible signal attenuation
#' periods are output. The start and end of the periods are defined by the
#' timings when the first-order differenced standard deviation time series
#' changes its sign before and after each peak.
#'
#' @inheritParams calc_dtmax_sp
#' @inheritParams n_valid
#' @param vctr_z A vector of Z-score time series. The length of the vector must
#'  match that of the timestamp vector.
#' @param vctr_avg A vector of moving window average time series that used in
#'  transforming the original time series into the Z-score time series. The
#'  length of the vector must match that of the timestamp vector. The unit of
#'  the time series must match that of `vctr_sd`.
#' @param vctr_sd A vector of moving window standard deviation time series that
#'  used in transforming the original time series into the Z-score time series.
#'  The length of the vector must match that of the timestamp vector. The unit
#'  of the time series must match that of `vctr_avg`.
#' @param wndw_size_conv An integer indicating the number of data points
#'  included in a moving window. Default is 48 * 15, implying that the window
#'  size is 15 days if the time interval of the input timestamp is 30 minutes.
#' @param inv_sigma_conv A positive value defining a Gaussian window width. The
#'  width of the Gaussian window is inversely proportional to this parameter.
#'  Default is 0.01.
#'
#' @returns
#' A data frame with columns below:
#'
#' * The first column, `time_start`, gives the timestamp at which the detected
#'  attenuation period begins.
#'
#' * The second column, `time_peak`, gives the timestamp of the detected
#'  attenuation period peak.
#'
#' * The third column, `time_end`, gives the timestamp at which the detected
#'  attenuation period ends.
#'
#' * The fourth column, `ratio`, gives the average of the ratio of the standard
#'  deviation at the detected attenuation peak to that at the beginning and end
#'  of the attenuation period. This value can be used as a threshold for
#'  determining the final attenuation periods that require Z-score modification.
#'
#' @author Yoshiaki Hata

check_short_attenuation <-
  function(vctr_time, vctr_z, vctr_avg, vctr_sd, wndw_size_conv = 48 * 15,
           inv_sigma_conv = 0.01, label_err = -9999) {
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
                 dT_z = vctr_z,
                 dT_avg = vctr_avg,
                 dT_sd = vctr_sd) %>%
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
    g_filter <- gsignal::gaussian(wndw_size_conv, inv_sigma_conv)
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
                    flag_dT_avg_peak =
                      ifelse(dT_avg_smth_diff1 == 0 |
                               dT_avg_smth_diff1 *
                               dplyr::lead(dT_avg_smth_diff1) < 0, 1, 0),
                    dT_sd_smth = gsignal::conv(dT_sd_smth, g_filter,
                                               shape = "same"),
                    dT_sd_smth_diff1 = dT_sd_smth - stats::lag(dT_sd_smth),
                    dT_sd_smth_diff2 = dT_sd_smth_diff1 -
                      stats::lag(dT_sd_smth_diff1),
                    flag_dT_sd_peak =
                      ifelse(dT_sd_smth_diff1 == 0 |
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
      message("--- There are less than two peaks. Skip the detection.")
      data.frame(time_start = vctr_time_start,
                 time_peak = vctr_time_peak,
                 time_end = vctr_time_end,
                 ratio = vctr_ratio) %>%
        return()
    } else {
      ## detect possible attenuation periods
      for (i in 1:length(vctr_time_sd_dpeak)) {
        target_time_sd_dpeak <- vctr_time_sd_dpeak[i]
        num_time_sd_peak <- which(vctr_time_sd_peak == target_time_sd_dpeak)

      if(num_time_sd_peak == 1) {
          attn_head <- time_mea_start + interval_time
          attn_tail <- vctr_time_sd_peak[num_time_sd_peak + 1] - interval_time
        } else if(num_time_sd_peak == length(vctr_time_sd_peak)) {
          attn_head <- vctr_time_sd_peak[num_time_sd_peak - 1] + interval_time
          attn_tail <- time_mea_end - interval_time
        } else {
          attn_head <- vctr_time_sd_peak[num_time_sd_peak - 1] + interval_time
          attn_tail <- vctr_time_sd_peak[num_time_sd_peak + 1] - interval_time
        }

        num_time_avg_peak <-
          which(vctr_time_avg_upeak >= attn_head &
                  vctr_time_avg_upeak <= attn_tail)

        df_attn <-
          data %>%
          dplyr::filter(time >= attn_head & time <= attn_tail)

        dT_sd_head <- dplyr::slice_head(df_attn) %>% dplyr::pull(dT_sd_smth)
        dT_sd_tail <- dplyr::slice_tail(df_attn) %>% dplyr::pull(dT_sd_smth)
        dT_sd_peak <-
          df_attn %>%
          dplyr::filter(., time == target_time_sd_dpeak) %>%
          dplyr::pull(dT_sd_smth)

        dT_sd_peak_ratio <-
          round((dT_sd_peak / dT_sd_head + dT_sd_peak / dT_sd_tail) / 2,
                digits = 5)

        if(length(num_time_avg_peak) >= 1) {
          vctr_time_start <- c(vctr_time_start, attn_head)
          vctr_time_peak <- c(vctr_time_peak, target_time_sd_dpeak)
          vctr_time_end <- c(vctr_time_end, attn_tail)
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

