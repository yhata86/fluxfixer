#' Remove error values manually
#'
#' @description `remove_manually()` removes unreasonable values manually by
#'  indicating specific timestamps.
#'
#' @inheritParams n_valid
#' @param vctr_time A timestamp vector of class POSIXct or POSIXt.
#' @param vctr_target A vector of a targeted time series to be checked. The
#'  length of the time series must be the same as that of `vctr_time`.
#' @param vctr_time_err A timestamp vector of class POSIXct or POSIXt,
#'  indicating specific error timings.
#'
#' @returns
#' A vector of cleaned time series. The length of the time series is the same
#' as the input time series. The data points at the indicated time points by
#' `vctr_time_err` are replaced with the error label specified in `label_err`.
#'
#' @examples
#' ## Load data
#' data(dt_noisy)
#' time <- dt_noisy$time[12097:14400]
#' target <- dt_noisy$dt[12097:14400]
#' time_err <- seq(as.POSIXct("2013/06/27 18:00", tz = "Etc/GMT-8"),
#'                 as.POSIXct("2013/06/27 22:30", tz = "Etc/GMT-8"),
#'                 by = "30 min")
#'
#' ## Remove error values
#' result <-
#'  remove_manually(vctr_time = time, vctr_target = target,
#'                  vctr_time_err = time_err)
#'
#' @author Yoshiaki Hata
#'
#' @export

remove_manually <-
  function(vctr_time, vctr_target, vctr_time_err, label_err = -9999) {
    ## avoid "No visible binding for global variable" notes
    time <- NULL
    target <- NULL
    target_cleaned <- NULL

    df <- data.frame(time = vctr_time,
                     target = vctr_target)

    df %>%
      dplyr::transmute(target_cleaned = ifelse(time %in% vctr_time_err,
                                               label_err, target)) %>%
      dplyr::pull(target_cleaned) %>%
      return()
  }


#' Remove outliers by absolute limits
#'
#' @description `check_absolute_limits()` removes out-of-range values by
#'  setting lower and upper limits.
#'
#' @inheritParams n_valid
#' @param vctr_target A vector of a targeted time series to be checked.
#' @param thres_al_min A threshold value for the input time series to define
#'  the lower limit. Default is 3.0. The data points with values below the
#'  threshold are considered outliers and removed. The unit of the threshold
#'  must match that of the input time series.
#' @param thres_al_max A threshold value for the input time series to define
#'  the upper limit. Default is 50.0. The data points with values above the
#'  threshold are considered outliers and removed. The unit of the threshold
#'  must match that of the input time series.
#'
#' @returns
#' A vector of cleaned time series. The length of the time series is the same
#' as the input time series. The data points with values below `thres_al_min`
#' or above `thres_al_max` are replaced with the error label specified in
#' `label_err`.
#'
#' @examples
#' ## Load data
#' data(dt_noisy)
#' target <- dt_noisy$dt
#'
#' ## Remove out-of-range values
#' result <- check_absolute_limits(vctr_target = target)
#'
#' @author Yoshiaki Hata
#'
#' @export

check_absolute_limits <-
  function(vctr_target, thres_al_min = 3.0, thres_al_max = 50.0,
           label_err = -9999) {
    ## avoid "No visible binding for global variable" notes
    . <- NULL

    vctr_target %>%
      dplyr::na_if(label_err) %>%
      ifelse(. < thres_al_min | . > thres_al_max, label_err, .) %>%
      tidyr::replace_na(label_err) %>%
      return()
  }


#' Modify short-term drifts
#'
#' @description `modify_short_drift()` corrects short-term drifts by adjusting
#'   the 5th and 95th percentiles of the drifted time series to those of
#'   the reference time series.
#'
#' @details
#' The short-term drift correction is to correct sudden changes in the average
#' in the time series over a short period (hours to days) specified by
#' `vctr_time_drft_head` and `vctr_time_drft_tail`. Multiple short-term drifts
#' can be corrected at once using this function.This procedure uses a reference
#' period, which is defined to consist of the three days (default) before and
#' after the occurrence of the anomaly. Then, the anomalous time series is
#' standardized so that the 5th and 95th percentile values of the anomalous and
#' reference (non-anomalous) time series match over this period. These
#' percentile values are used instead of the maximum and minimum values to
#' ensure robustness against possible outliers in the original or reference
#' time series.
#'
#' @inheritParams get_interval
#' @inheritParams remove_manually
#' @param vctr_time_drft_head A timestamp vector of class POSIXct or POSIXt,
#'  indicating when each drift starts.
#' @param vctr_time_drft_tail A timestamp vector of class POSIXct or POSIXt,
#'  indicating when each drift ends. The length of the time series must be the
#'  same as that of `vctr_time_drft_head`.
#' @param n_day_ref A positive integer representing the number of days to be
#'  referenced before and after the anomaly period. Default is 3 (days).
#'
#' @returns
#' A vector of the drift-corrected time series. The length of the time series
#' is the same as the input time series.
#'
#' @examples
#' ## Load data
#' data(dt_noisy)
#' time <- dt_noisy$time[11931:12891]
#' target <- dt_noisy$dt[11931:12891]
#' time_drft_head <- time[1]
#' time_drft_tail <- time[148]
#'
#' ## Correct a short-term drift
#' result <-
#'   modify_short_drift(vctr_time = time, vctr_target = target,
#'                      vctr_time_drft_head = time_drft_head,
#'                      vctr_time_drft_tail = time_drft_tail)
#'
#' @author Yoshiaki Hata
#'
#' @include utils.R
#'
#' @importFrom rlang :=
#'
#' @export

modify_short_drift <-
  function(vctr_time, vctr_target, vctr_time_drft_head, vctr_time_drft_tail,
           n_day_ref = 3, label_err = -9999) {
    ## avoid "No visible binding for global variable" notes
    . <- NULL
    time <- NULL
    dT <- NULL
    dT_drft_mod <- NULL
    dT_mod <- NULL

    ## check input values
    interval_time <- get_interval(vctr_time)
    timezone <- lubridate::tz(vctr_time[1])

    data_dT <- data.frame(time = vctr_time,
                          dT = vctr_target,
                          dT_mod = vctr_target)

    if(length(vctr_time_drft_head) != length(vctr_time_drft_tail)) {
      stop("Drift periods must be specified by both start and end time stamps")
    }

    for (i in 1:length(vctr_time_drft_head)) {
      colname_drft <- paste0("dT_drft_", i)
      colname_ref <- paste0("dT_ref_", i)

      data_dT <-
        data_dT %>%
        dplyr::mutate(!!colname_drft :=
                        ifelse(dplyr::between(time,
                                              vctr_time_drft_head[[i]],
                                              vctr_time_drft_tail[[i]]),
                               dT, label_err),
                      !!colname_ref :=
                        ifelse(dplyr::between(time,
                                              vctr_time_drft_head[[i]] -
                                                lubridate::days(n_day_ref),
                                              vctr_time_drft_head[[i]] -
                                                interval_time) |
                                 dplyr::between(time,
                                                vctr_time_drft_tail[[i]] +
                                                  interval_time,
                                                vctr_time_drft_tail[[i]] +
                                                  lubridate::days(n_day_ref)),
                               dT, label_err)) %>%
        dplyr::na_if(label_err)

      ## calculate 5th and 95th percentile for drift correction
      vctr_drft <- data_dT %>% dplyr::pull(., !!colname_drft)
      vctr_ref <- data_dT %>% dplyr::pull(., !!colname_ref)

      q05_drft <-
        stats::quantile(vctr_drft, 0.05, na.rm = TRUE, names = FALSE)
      q95_drft <-
        stats::quantile(vctr_drft, 0.95, na.rm = TRUE, names = FALSE)
      q05_ref <- stats::quantile(vctr_ref, 0.05, na.rm = TRUE, names = FALSE)
      q95_ref <- stats::quantile(vctr_ref, 0.95, na.rm = TRUE, names = FALSE)

      ## drift correction
      data_dT <-
        data_dT %>%
        dplyr::mutate(dT_drft_mod =
                        (get(colname_drft) - q05_drft) /
                        (q95_drft - q05_drft) * (q95_ref - q05_ref) + q05_ref,
                      dT_mod =
                        ifelse(dplyr::between(time,
                                              vctr_time_drft_head[[i]],
                                              vctr_time_drft_tail[[i]]),
                               dT_drft_mod, dT_mod))
    }
    tidyr::replace_na(data_dT$dT_mod, label_err) %>% return()
  }


#' Filter high frequency noise by Gaussian filter
#'
#' @description `filter_highfreq_noise()` filters a time series with a specific
#'  period by convolving it with a Gaussian window, removing high-frequency
#'  noise.
#'
#' @inheritParams modify_short_drift
#' @param vctr_time_noise A timestamp vector of class POSIXct or POSIXt,
#'  indicating when high-frequency noise exists in the targeted time series.
#' @param wndw_size_noise A positive integer indicating the number of data
#'  points included in a moving Gaussian window for the high-frequency noise
#'  filtering. The default is 13, meaning that the window size is 6.5 hours if
#'  the time interval of the input timestamp is 30 minutes.
#' @param inv_sigma_noise A positive value defining a Gaussian window width for
#'  the high-frequency noise filtering. The width of the Gaussian window is
#'  inversely proportional to this parameter. Default is 0.01.
#'
#' @returns
#' A vector of the noise-filtered time series. The length of the time series is
#' the same as the input time series.
#'
#' @examples
#' ## Create data
#' time <- seq(as.POSIXct("2026/01/01"), length.out = 360, by = "1 day")
#' x <- seq(1, 360)
#' target <- sin(x / 180 * pi) + stats::rnorm(length(x), sd = 0.2)
#' time_noise <-
#'   seq(as.POSIXct("2026/01/01"), as.POSIXct("2026/09/01"), by = "1 day")
#'
#' ## Filter noise
#' result <-
#'   filter_highfreq_noise(vctr_time = time, vctr_target = target,
#'                         vctr_time_noise = time_noise)
#'
#' @author Yoshiaki Hata
#'
#' @importFrom rlang :=
#'
#' @export

filter_highfreq_noise <-
  function(vctr_time, vctr_target, vctr_time_noise,
           wndw_size_noise = 13, inv_sigma_noise = 0.01, label_err = -9999) {
    ## avoid "No visible binding for global variable" notes
    dT <- NULL
    dT_noise <- NULL
    time <- NULL
    dT_filtered <- NULL
    dT_mod <- NULL

    g_filter <- gsignal::gaussian(wndw_size_noise, inv_sigma_noise)
    g_filter <- g_filter / sum(g_filter)

    data_dT <- data.frame(time = vctr_time,
                          dT = vctr_target)

    ## interpolate data gaps temporarily for convolution
    data_dT <-
      data_dT %>%
      dplyr::mutate(dT_noise = ifelse(dT == label_err, NA, dT),
                    dT_noise = zoo::na.approx(dT_noise, na.rm = FALSE)) %>%
      tidyr::fill(dT_noise, .direction = "updown") %>%
      dplyr::mutate(dT_filtered = gsignal::conv(dT_noise, g_filter,
                                                shape = "same"),
                    dT_mod = ifelse(time %in% vctr_time_noise, dT_filtered, dT),
                    dT_mod = ifelse(dT == label_err, label_err, dT_mod))

    return(data_dT$dT_mod)
  }


#' Remove outliers by Z-score time series
#'
#' @description `remove_zscore_outlier()` detects and removes outlier values by
#'  converting an original time series into a Z-score time series using a
#'  moving window.
#'
#' @details
#' The input time series is normalized using a moving window, and the data
#' values are converted to Z-scores. In this step, the width of the moving
#' window is set to 15 days by default, centered on the target time point,
#' and normalization is performed individually for each time point in the time
#' series. The upper and lower limits of the Z-score (default: ±5) are set, and
#' data points outside that range are removed as outliers. After the outliers
#' have been removed, the Z-score is returned to the original value using the
#' original mean and standard deviation time series, and normalization is
#' performed again using a moving window to remove additional outliers. These
#' procedures are repeated until either no more outliers are removed or the
#' maximum number of iterations (default 10) is reached.
#'
#' Users can define sub-periods across the entire time series using
#' `vctr_time_prd_tail`, and the Z-score conversion is performed in each
#' sub-period separately. This separated conversion is useful when the input
#' time series suddenly changes its nature, such as after a sensor replacement.
#'
#' In some cases, for sap flow measurements, the input dT (the temperature
#' difference between sap flow probes) time series may yield a signal that is
#' attenuated for only a short period, for example, when rainfall continues for
#' days, causing the moving window mean (or standard deviation) to increase
#' (or decrease). In such cases, normalization will cause the Z-score time
#' series immediately before and after the rainfall to be unnaturally
#' distorted, hindering the construction of the random forest model. If
#' `modify_z` is `TRUE`, after the outlier removal, this function modifies the
#' Z-score time series for periods when the moving window average has an upward
#' peak, and the moving window standard deviation has a downward peak
#' simultaneously. First, the average and standard deviation time series are
#' interpolated if they contain missing values. Second, they are smoothed by
#' convolution with a user-specified Gaussian window, defined by the parameters
#' `wndw_size_conv` and `inv_sigma_conv`. Third, the first-order and
#' second-order differences of both smoothed time series are calculated, which
#' determine the upward peak positions of the average and the downward peak
#' positions of the standard deviation. Fourth, possible signal attenuation
#' periods are determined based on these peak positions. The start and end of
#' the periods are defined by the timings when the first-order differenced
#' standard deviation time series changes its sign before and after each peak.
#' Fifth, the final attenuation periods are selected if the average of the
#' ratio of the standard deviation at the detected attenuation peak to that at
#' the beginning and end of the attenuation period is below the threshold value
#' specified by `thres_ratio`. Optionally, users can specify the periods to be
#' modified by `vctr_time_zmod`. Sixth, the average and standard deviation time
#' series during the attenuation periods are deleted and linearly interpolated.
#' Finally, the modified Z-score time series is calculated using these average
#' and standard deviation time series.
#'
#' @inheritParams remove_manually
#' @param vctr_time_prd_tail A timestamp vector of class POSIXct or POSIXt,
#'  indicating the end timings of each sub-period. Note that users must not
#'  include the final timestamp for the entire time series. For instance, if
#'  users want to split the entire measurement period into three sub-periods,
#'  they only need to specify the end time stamps of the first two sub-periods.
#'  Default is `NULL`.
#' @param n_calc_max A positive integer indicating the maximum number of
#'  Z-score outlier detection iterations. Default is 10.
#' @param wndw_size_z A positive integer indicating the number of data points
#'  included in a moving window for the Z-score outlier removal. The default is
#'  48 * 15, meaning that the window size is 15 days if the time interval of
#'  the input timestamp is 30 minutes.
#' @param min_n_wndw_z A positive integer indicating the minimum number of data
#'  points for calculating statistics using a moving window (default is 5)  for
#'  the Z-score outlier removal. If the number of data points is less than this
#'  threshold, the statistics are not calculated in the window.
#' @param thres_z A positive threshold value for the Z-score time series to
#'  define outliers. Default is 5.0. The data points with Z-scores (absolute
#'  values) above the threshold are considered outliers and removed.
#' @param modify_z A boolean. If `TRUE`, conduct Z-score short attenuation
#'  correction; else, the correction is not applied. Default is `FALSE`.
#' @param vctr_time_zmod Only valid if `modify_z` is `TRUE`. A timestamp vector
#'  of class POSIXct or POSIXt, indicating the timings when the short-term
#'  signal attenuation correction is applied. Default is `NULL`.
#' @param wndw_size_conv Only valid if `modify_z` is `TRUE`. A positive integer
#'  indicating the number of data points included in a moving window for the
#'  short-term signal attenuation detection. The default is 48 * 15, meaning
#'  that the window size is 15 days if the time interval of the input timestamp
#'  is 30 minutes.
#' @param inv_sigma_conv Only valid if `modify_z` is `TRUE`. A positive value
#'  defining a Gaussian window width for the short-term signal attenuation
#'  detection. The width of the Gaussian window is inversely proportional to
#'  this parameter. Default is 0.01.
#' @param thres_ratio Only valid if `modify_z` is `TRUE`. A positive threshold
#'  value of the ratio for determining whether the signal attenuation
#'  correction is applied to each detected attenuation period. The ratio
#'  represents the average of the standard deviation at the detected
#'  attenuation peak relative to that at the beginning and end of the
#'  attenuation period. If the ratio is below this threshold value, the
#'  correction is applied. Default is 0.5.
#'
#' @returns
#' A data frame with columns below:
#'
#' * The first column, `time`, gives the same timestamp as `vctr_time`.
#'
#' * The second column, `cleaned`, gives the cleaned time series after replacing
#'  the detected outliers with the value specified by `label_err`.
#'
#' * The third column, `z_cleaned`, gives the Z-score of the input time series
#'  after removing the detected outliers.
#'
#' * The fourth column, `avg_cleaned`, gives the moving window average of the
#'  input time series after removing the detected outliers.
#'
#' * The fifth column, `sd_cleaned`, gives the moving window standard
#'  deviation of the input time series after removing the detected outliers.
#'
#' * The sixth column, `flag_out` gives a flag variable time series indicating
#'  the status of the cleaned time series (0: the input data point is not
#'  originally missing and not detected as an outlier; 1: the input data point
#'  is not originally missing but detected as an outlier; 2: the input data
#'  point is originally missing).
#'
#' @examples
#' ## Load data
#' data(dt_noisy)
#' time <- dt_noisy$time[12097:14400]
#' target <- dt_noisy$dt[12097:14400]
#'
#' ## Remove outliers
#' result <- remove_zscore_outlier(vctr_time = time, vctr_target = target)
#'
#' @author Yoshiaki Hata
#'
#' @include utils.R
#'
#' @export

remove_zscore_outlier <-
  function(vctr_time, vctr_target, vctr_time_prd_tail = NULL,
           wndw_size_z = 48 * 15, min_n_wndw_z = 5, thres_z = 5.0,
           n_calc_max = 10, modify_z = FALSE, vctr_time_zmod = NULL,
           wndw_size_conv = 48 * 15, inv_sigma_conv = 0.01, thres_ratio = 0.5,
           label_err = -9999) {
    ## avoid "No visible binding for global variable" notes
    time <- NULL
    dT <- NULL
    dT_mod <- NULL
    dT_avg <- NULL
    dT_sd <- NULL
    dT_z <- NULL
    flag_out_z_cum <- NULL
    dT_mod_prd <- NULL
    dT_n_prd <- NULL
    dT_avg_prd <- NULL
    dT_sd_prd <- NULL
    dT_z_prd <- NULL
    dT_n <- NULL
    dT_z_ori <- NULL
    flag_out_z <- NULL
    ratio <- NULL
    time_start <- NULL
    time_end <- NULL


    message("Z-score outlier detection started")

    data_dT <- data.frame(time = vctr_time,
                          dT = vctr_target)

    data_dT <-
      data_dT %>%
      dplyr::mutate(dT_mod = dT,
                    dT_avg = NA,
                    dT_sd = NA,
                    dT_z = NA,
                    flag_out_z_cum = ifelse(dT == label_err, 2, 0))

    n_outliers <- 10^6
    n_calc <- 0

    ## standardization for each sub-period
    while (n_outliers > 0 & n_calc <= n_calc_max) {
      n_calc <- n_calc + 1

      if(!is.null(vctr_time_prd_tail)) {
        for (i in 1:(length(vctr_time_prd_tail) + 1)) {
          if(i == 1) {
            data_dT <-
              data_dT %>%
              dplyr::mutate(dT_mod_prd = ifelse(time <= vctr_time_prd_tail[[i]],
                                                dT_mod, label_err))
          } else if(i == length(vctr_time_prd_tail) + 1) {
            data_dT <-
              data_dT %>%
              dplyr::mutate(dT_mod_prd =
                              ifelse(time > vctr_time_prd_tail[[i - 1]],
                                     dT_mod, label_err))
          } else {
            data_dT <-
              data_dT %>%
              dplyr::mutate(dT_mod_prd =
                              ifelse(time > vctr_time_prd_tail[[i - 1]] &
                                       time <= vctr_time_prd_tail[[i]],
                                     dT_mod, label_err))
          }

          data_dT <-
            data_dT %>%
            dplyr::na_if(label_err) %>%
            dplyr::mutate(dT_n_prd = zoo::rollapply(dT_mod_prd,
                                                    width = wndw_size_z,
                                                    FUN = n_valid, fill = NA,
                                                    partial = TRUE),
                          dT_avg_prd = zoo::rollapply(dT_mod_prd,
                                                      width = wndw_size_z,
                                                      FUN = mean, fill = NA,
                                                      na.rm = TRUE,
                                                      partial = TRUE),
                          dT_avg_prd = ifelse(dT_n_prd < min_n_wndw_z, NA,
                                              dT_avg_prd),
                          dT_sd_prd = zoo::rollapply(dT_mod_prd,
                                                     width = wndw_size_z,
                                                     FUN = stats::sd, fill = NA,
                                                     na.rm = TRUE,
                                                     partial = min_n_wndw_z),
                          dT_sd_prd = ifelse(dT_n_prd < min_n_wndw_z, NA,
                                             dT_sd_prd),
                          dT_z_prd = (dT_mod_prd - dT_avg_prd) / dT_sd_prd,
                          dT_avg = ifelse(!is.na(dT_mod_prd), dT_avg_prd,
                                          dT_avg),
                          dT_sd = ifelse(!is.na(dT_mod_prd), dT_sd_prd, dT_sd),
                          dT_z = ifelse(!is.na(dT_mod_prd), dT_z_prd, dT_z))
        }
      } else {
        data_dT <-
          data_dT %>%
          dplyr::na_if(label_err) %>%
          dplyr::mutate(dT_n = zoo::rollapply(dT_mod,
                                                  width = wndw_size_z,
                                                  FUN = n_valid, fill = NA,
                                                  partial = TRUE),
                        dT_avg = zoo::rollapply(dT_mod,
                                                    width = wndw_size_z,
                                                    FUN = mean, fill = NA,
                                                    na.rm = TRUE,
                                                    partial = TRUE),
                        dT_avg = ifelse(dT_n < min_n_wndw_z | is.na(dT_mod),
                                        NA, dT_avg),
                        dT_sd = zoo::rollapply(dT_mod,
                                                   width = wndw_size_z,
                                                   FUN = stats::sd, fill = NA,
                                                   na.rm = TRUE,
                                                   partial = min_n_wndw_z),
                        dT_sd = ifelse(dT_n < min_n_wndw_z | is.na(dT_mod),
                                       NA, dT_sd),
                        dT_z = (dT_mod - dT_avg) / dT_sd)
      }

      if(n_calc == 1) {
        data_dT <-
          data_dT %>%
          dplyr::mutate(dT_z_ori = dT_z)
      }

      data_dT <-
        data_dT %>%
        dplyr::mutate(flag_out_z = ifelse(abs(dT_z) > thres_z, 1, 0),
                      flag_out_z = ifelse(is.na(dT_z), 0, flag_out_z),
                      flag_out_z_cum = ifelse(flag_out_z == 1, flag_out_z,
                                              flag_out_z_cum),
                      dT_z = ifelse(flag_out_z == 1, NA, dT_z),
                      dT_mod = dT_z * dT_sd + dT_avg)

      n_outliers <- data_dT %>% dplyr::pull(flag_out_z) %>% sum()

      if(n_outliers > 1 & n_calc < n_calc_max) {
        message("--- ", n_outliers,
                " Z-score outliers were detected")
      } else if(n_outliers == 1 & n_calc < n_calc_max) {
        message("--- 1 Z-score outlier was detected")
      } else if(n_outliers > 1 & n_calc == n_calc_max) {
        message("--- Z-score outliers still exist but finish the detection")
      } else {
        message("--- No Z-score outlier was detected")
      }
    }

    ## correct Z-score time series during short attenuation periods
    if(modify_z == TRUE) {
      if(is.null(vctr_time_zmod)) {
        df_attn <-
          check_short_attenuation(data_dT$time, data_dT$dT_z, data_dT$dT_avg,
                                  data_dT$dT_sd, wndw_size_conv,
                                  inv_sigma_conv, label_err)

        n_attn <-
          df_attn %>%
          dplyr::filter(ratio < thres_ratio) %>%
          nrow()

        if(n_attn > 0) {
          vctr_time_attn_head <-
            df_attn %>%
            dplyr::filter(ratio < thres_ratio) %>%
            dplyr::pull(time_start)

          vctr_time_attn_tail <-
            df_attn %>%
            dplyr::filter(ratio < thres_ratio) %>%
            dplyr::pull(time_end)

          message("Short attenuation periods were detected at ",
                  vctr_time_attn_head)

          for (i_attn in n_attn) {
            data_dT <-
              data_dT %>%
              dplyr::mutate(dT_avg =
                              ifelse(time >= vctr_time_attn_head[[i_attn]] &
                                       time <= vctr_time_attn_tail[[i_attn]],
                                     NA, dT_avg),
                            dT_avg = zoo::na.approx(dT_avg, na.rm = FALSE),
                            dT_sd =
                              ifelse(time >= vctr_time_attn_head[[i_attn]] &
                                       time <= vctr_time_attn_tail[[i_attn]],
                                     NA, dT_sd),
                            dT_sd = zoo::na.approx(dT_sd, na.rm = FALSE),
                            dT_z = (dT_mod - dT_avg) / dT_sd)
          }
        }
      } else {
        data_dT <-
          data_dT %>%
          dplyr::mutate(dT_avg =
                          ifelse(time %in% vctr_time_zmod, NA, dT_avg),
                        dT_avg = zoo::na.approx(dT_avg, na.rm = FALSE),
                        dT_sd =
                          ifelse(time %in% vctr_time_zmod, NA, dT_sd),
                        dT_sd = zoo::na.approx(dT_sd, na.rm = FALSE),
                        dT_z = (dT_mod - dT_avg) / dT_sd)
      }
    }

    message("Z-score outlier detection finished")

    data_dT %>%
      tidyr::replace_na(replace = list(dT_mod = label_err,
                                       dT_avg = label_err,
                                       dT_sd = label_err,
                                       dT_z = label_err)) %>%
      dplyr::select(time, dT_mod, dT_z, dT_avg, dT_sd, flag_out_z_cum) %>%
      dplyr::rename(cleaned = dT_mod,
                    z_cleaned = dT_z,
                    avg_cleaned = dT_avg,
                    sd_cleaned = dT_sd,
                    flag_out = flag_out_z_cum) %>%
      return()
  }


#' Define reference values of average and standard deviation
#'
#' @description `calc_ref_stats()` determines reference values of average and
#'  standard deviation for the entire period by calculating the median of
#'  these statistical values for the first several days in each sub-period.
#'
#' @inheritParams remove_zscore_outlier
#' @param wndw_size_ref A positive integer indicating the number of data points
#'  included in calculating the average and standard deviation for their
#'  reference value determination. The default is 48 * 15, meaning that the
#'  first 15 days of each sub-period are used in the calculation when the time
#'  interval of the input timestamp is 30 minutes.
#'
#' @returns
#' A vector of two components. The first one is the reference average, and the
#' second one is the reference standard deviation. The unit of these values is
#' the same as that of the input time series.
#'
#' @examples
#' ## Load data
#' data(dt_noisy)
#' time <- dt_noisy$time[11931:12891]
#' target <- dt_noisy$dt[11931:12891]
#' time_prd_tail <- time[148]
#'
#' ## Calculate reference values of average and standard deviation
#' result <-
#'   calc_ref_stats(vctr_time = time, vctr_target = target,
#'                  vctr_time_prd_tail = time_prd_tail)
#'
#' @author Yoshiaki Hata
#'
#' @seealso retrieve_ts
#'
#' @include utils.R
#'
#' @export

calc_ref_stats <-
  function(vctr_time, vctr_target, vctr_time_prd_tail = NULL,
           wndw_size_ref = 48 * 15, label_err = -9999) {
    ## avoid "No visible binding for global variable" notes
    time <- NULL
    dT <- NULL
    dT_prd <- NULL

    ## check input values
    interval_time <- get_interval(vctr_time)
    timezone <- lubridate::tz(vctr_time[1])

    data_dT <-
      data.frame(time = vctr_time,
                 dT = vctr_target) %>%
      dplyr::mutate(dT = ifelse(dT == label_err, NA, dT))

    time_head_ref_1st <-
      data_dT %>%
      dplyr::filter(!is.na(dT)) %>%
      dplyr::slice_head() %>%
      dplyr::pull(time)

    vctr_dT_avg_ref <- rep(NA, length(vctr_time_prd_tail) + 1)
    vctr_dT_sd_ref <- rep(NA, length(vctr_time_prd_tail) + 1)

    for (i in 1:(length(vctr_time_prd_tail) + 1)) {
      if(i == 1) {
        vctr_dT_prd <-
          data_dT %>%
          dplyr::mutate(dT_prd =
                          ifelse(time >= time_head_ref_1st &
                                   time < time_head_ref_1st +
                                   lubridate::minutes(interval_time *
                                                        wndw_size_ref),
                                 dT, NA)) %>%
          dplyr::pull(dT_prd)

        vctr_dT_avg_ref[[i]] <- mean(vctr_dT_prd, na.rm = TRUE)
        vctr_dT_sd_ref[[i]] <- stats::sd(vctr_dT_prd, na.rm = TRUE)
      } else {
        vctr_dT_prd <-
          data_dT %>%
          dplyr::mutate(dT_prd =
                          ifelse(time > vctr_time_prd_tail[[i - 1]] &
                                   time <= vctr_time_prd_tail[[i - 1]] +
                                   lubridate::minutes(interval_time *
                                                        wndw_size_ref),
                                 dT, NA)) %>%
          dplyr::pull(dT_prd)

        vctr_dT_avg_ref[[i]] <- mean(vctr_dT_prd, na.rm = TRUE)
        vctr_dT_sd_ref[[i]] <- stats::sd(vctr_dT_prd, na.rm = TRUE)
      }
    }

    dT_avg_ref <- stats::median(vctr_dT_avg_ref, na.rm = TRUE)
    dT_sd_ref <- stats::median(vctr_dT_sd_ref, na.rm = TRUE)

    return(c(dT_avg_ref, dT_sd_ref))
  }


#' Retrieve time series in its original units
#'
#' @description `retrieve_ts()` converts a standardized Z-score time series
#'  into a time series in its original units using specific average and
#'  standard deviation time series.
#'
#' @details
#' Retrieving a time series with its original units is conducted by multiplying
#' a Z-score to standard deviation, following by adding average. If the average
#' and standard deviation time series are the same as those in converting the
#' original time series into the Z-score time series, the original values with
#' the original average and standard deviation are retrieved. If reference
#' values of the average and/or standard deviation are used, the output time
#' series are detrended and/or applied to signal damping correction.
#'
#' @inheritParams calc_ref_stats
#' @param vctr_target_z A vector of Z-score time series to be converted.
#'  Missing values must be gap-filled previously.
#' @param vctr_target_avg A vector of average time series. Missing values are
#'  acceptable but automatically gap-filled by interpolation during the
#'  retrieving process. The length of the vector must match that of
#'  `vctr_target_z`. The unit of the time series must match that of time series
#'  to be output. Default is `NULL`.
#' @param vctr_target_sd A vector of standard deviation time series. Missing
#'  values are acceptable but automatically gap-filled by interpolation during
#'  the retrieving process. The length of the vector must match that of
#'  `vctr_target_z`. The unit of the time series must match that of time series
#'  to be output. Default is `NULL`.
#' @param detrend A boolean. If `TRUE`, detrending is applied and the reference
#'  average specified by `avg_ref` is used to convert Z-score time series into
#'  the time series with the reference average in its original units; else, the
#'  detrending is not applied, and the average time series specified by
#'  `vctr_target_avg` is used in the conversion. Default is `FALSE`.
#' @param correct_damping A boolean. If `TRUE`, the signal damping correction
#'  is applied and the reference standard deviation specified by `sd_ref` is
#'  used to convert Z-score time series into the time series in its original
#'  units with the reference standard deviation; else, the correction is not
#'  applied, and the standard deviation time series specified by
#'  `vctr_target_sd` is used in the conversion. Default is `FALSE`.
#' @param avg_ref Only valid if `detrend` is `TRUE`. A numeric value
#'  representing the reference average. A vector of reference average time
#'  series is also acceptable, but the length of the vector must match that of
#'  `vctr_target_z`, and the unit of the time series must match that of time
#'  series to be output. Default is `NULL`.
#' @param sd_ref Only valid if `correct_damping` is `TRUE`. A positive numeric
#'  value representing the reference standard deviation. A vector of reference
#'  standard deviation time series is also acceptable, but the length of the
#'  vector must match that of `vctr_target_z`, and the unit of the time series
#'  must match that of time series to be output. Default is `NULL`.
#'
#' @returns
#' A vector of the retrieved time series. The length of the vector is the same
#' as `vctr_target_z`.
#'
#' @examples
#' ## Create data
#' target <- seq(1, 10)
#' target_avg <- rep(mean(target), 10)
#' target_sd <- rep(stats::sd(target), 10)
#' target_z <- (target - target_avg) / target_sd
#'
#' ## Retrieve time series in its original units
#' result <-
#'   retrieve_ts(vctr_target_z = target_z, vctr_target_avg = target_avg,
#'               vctr_target_sd = target_sd)
#'
#' @author Yoshiaki Hata
#'
#' @seealso calc_ref_stats
#'
#' @export

retrieve_ts <-
  function(vctr_target_z, vctr_target_avg = NULL, vctr_target_sd = NULL,
           detrend = FALSE, correct_damping = FALSE,
           avg_ref = NULL, sd_ref = NULL, label_err = -9999) {
    ## avoid "No visible binding for global variable" notes
    avg_target <- NULL
    sd_target <- NULL
    avg_target_gf <- NULL
    sd_target_gf <- NULL

    message("Time series retrieval started")

    ## check input
    if(detrend == TRUE & correct_damping == TRUE &
       (is.null(avg_ref) | is.null(sd_ref))) {
      stop("Both the reference average and the reference standard deviation \n
           must be provided to apply detrending and signal damping correction.")
    }

    if(detrend == TRUE & correct_damping == FALSE &
       (is.null(avg_ref) | is.null(vctr_target_sd))) {
      stop("Both the reference average and the original standard deviation \n
           time series must be provided to apply detrending and retrieval.")
    }

    if(detrend == FALSE & correct_damping == TRUE &
       (is.null(vctr_target_avg) | is.null(sd_ref))) {
      stop("Both the original average time series and the reference standard \n
           deviation time series must be provided to apply signal damping \n
           and retrieval.")
    }

    if(detrend == FALSE & correct_damping == FALSE &
       (is.null(vctr_target_avg) | is.null(vctr_target_sd))) {
      stop("Both the original average and standard deviation time series \n
           must be provided to apply retrieval.")
    }

    df_rt <-
      data.frame(target_z = vctr_target_z,
                 avg_target = vctr_target_avg,
                 sd_target = vctr_target_sd) %>%
      dplyr::mutate(avg_target = ifelse(avg_target == label_err, NA,
                                        avg_target),
                    sd_target = ifelse(sd_target == label_err, NA, sd_target),
                    avg_target_gf = zoo::na.approx(avg_target, na.rm = FALSE),
                    sd_target_gf = zoo::na.approx(sd_target,
                                                  na.rm = FALSE)) %>%
      tidyr::fill(c(avg_target_gf, sd_target_gf), .direction = "updown")

    if(correct_damping == TRUE) {
      vctr_target <- vctr_target_z * sd_ref
      message("--- Signal damping correction finished")
    } else {
      vctr_target <- vctr_target_z * df_rt$sd_target_gf
      message("--- Signal damping correction was not applied")
    }

    if(detrend == TRUE) {
      vctr_target <- vctr_target + avg_ref
      message("--- Detrending finished")
    } else {
      vctr_target <- vctr_target + df_rt$avg_target_gf
      message("--- Detrending was not applied")
    }

    message("Time series retrieval finished")
    return(vctr_target)
  }

