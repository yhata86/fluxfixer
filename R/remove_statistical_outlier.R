#' Remove outliers by statistical method
#'
#' @description remove_outlier() removes unreasonable values, corrects short-term
#'   drift, filtering noise, detects outliers by z-score normalization.
#'
#' @param vctr_time Timestamp vector and must be continuous (no gap permitted)
#' @param vctr_dt Delta T (temperature difference between probes) time series
#' @param list_time_prd_tail List of timestamp describing sensor replacement
#' @param list_time_err_head List of timestamp describing specific error period started
#' @param list_time_err_tail List of timestamp describing specific error period ended
#' @param list_time_drft_head List of timestamp describing specific drift period started
#' @param list_time_drft_tail List of timestamp describing specific drift period ended
#' @param thres_min_dt Minimum threshold value of acceptable delta T
#' @param thres_max_dt Maximum threshold value of acceptable delta T
#' @param wndw_size The number of data points in a moving window for z-score normalization
#' @param min_n_wndw Minimum number of data points in a moving window for calculating z-score
#' @param thres_z Threshold absolute value of z-score for detecting outliers
#' @param check_damping Logical. If true, short-term damping is considered for z-score normalization.
#' @param thres_ratio_damp Threshold value of correcting short-term damping
#' @param filter_noise Logical. If true, filtering noise by gaussian window is conducted.
#' @param wndw_size_noise Window size of gaussian window
#' @param inv_sigma_gauss_noise Parameter for gaussian window
#'
#' @examples
#' # Remove outliers
#' library(lubridate)
#'
#' time_test <- dt_noisy$time
#' dt_test <- dt_noisy$dt
#'
#' prd_tail <- list(ymd_hm("2013/05/14 13:00"))
#' err_head <- list(ymd_hm("2013/05/14 13:00"),
#'                  ymd_hm("2016/12/14 12:30"))
#' err_tail <- list(ymd_hm("2013/05/14 13:00"),
#'                  ymd_hm("2016/12/14 21:00"))
#' drft_head <- list(ymd_hm("2013/05/14 13:30"))
#' drft_tail <- list(ymd_hm("2013/05/17 15:00"))
#'
#' result <-
#'   remove_outlier(time_test, dt_test, prd_tail, err_head, err_tail,
#'                  drft_head, drft_tail)

remove_outlier <-
  function(vctr_time, vctr_dt, list_time_prd_tail,
           list_time_err_head = NULL, list_time_err_tail = NULL,
           list_time_drft_head = NULL, list_time_drft_tail = NULL,
           thres_min_dt = 3.0, thres_max_dt = 50.0,
           wndw_size = 48 * 15, min_n_wndw = 5,
           thres_z = 5.0, check_damping = TRUE, thres_ratio_damp = 0.5,
           filter_noise = FALSE, wndw_size_noise = 13,
           inv_sigma_gauss_noise = 0.01) {

    message("Outlier removal process started.")

    time_head_mea <- vctr_time[1]
    time_tail_mea <- vctr_time[length(vctr_time)]

    ## remove error values and separate drifted period
    data_dT <-
      data.frame(time = vctr_time, dT = vctr_dt) %>%
      dplyr::mutate(dT_mod1 = ifelse(dT < thres_min_dt, -9999, dT),
                    dT_mod1 = ifelse(dT > thres_max_dt, -9999, dT_mod1),
                    dT_mod2 = dT_mod1)

    message("Abnormally low dT values were removed.")

    if(!is.null(list_time_err_head)) {
      for (i in 1:length(list_time_err_head)) {
        data_dT <-
          data_dT %>%
          dplyr::mutate(dT_mod2 = ifelse(between(time, list_time_err_head[[i]],
                                                 list_time_err_tail[[i]]), -9999,
                                         dT_mod2))
      }

      message("Manually specified error values were removed.")
    }

    data_dT <- data_dT %>% dplyr::mutate(dT_mod3 = dT_mod2)

    if(!is.null(list_time_drft_head)) {
      for (i in 1:length(list_time_drft_head)) {
        colname_drft <- paste0("dT_mod2_drft_", i)
        colname_ref <- paste0("dT_mod2_ref_", i)

        data_dT <-
          data_dT %>%
          dplyr::mutate(!!colname_drft := ifelse(between(time,
                                                         list_time_drft_head[[i]],
                                                         list_time_drft_tail[[i]]),
                                                 dT_mod2, -9999),
                        !!colname_ref := ifelse(between(time,
                                                        list_time_drft_head[[i]] -
                                                          lubridate::days(3),
                                                        list_time_drft_head[[i]] -
                                                          lubridate::minutes(30)) |
                                                  between(time,
                                                          list_time_drft_tail[[i]] +
                                                            lubridate::minutes(30),
                                                          list_time_drft_tail[[i]] +
                                                            days(3)),
                                                dT_mod2, -9999)) %>%
          dplyr::na_if(-9999)

        ## calculate 5th and 95th percentile for drift correction
        data_dT %>% {
          dplyr::pull(., !!colname_drft) %>%
            quantile(0.05, na.rm = TRUE, names = FALSE) ->> q05_drft
          dplyr::pull(., !!colname_drft) %>%
            quantile(0.95, na.rm = TRUE, names = FALSE) ->> q95_drft
          dplyr::pull(., !!colname_ref) %>%
            quantile(0.05, na.rm = TRUE, names = FALSE) ->> q05_ref
          dplyr::pull(., !!colname_ref) %>%
            quantile(0.95, na.rm = TRUE, names = FALSE) ->> q95_ref
        }

        ## drift correction
        data_dT <-
          data_dT %>%
          dplyr::mutate(dT_mod2_drft_mod =
                          (get(colname_drft) - q05_drft) / (q95_drft - q05_drft) *
                          (q95_ref - q05_ref) + q05_ref,
                        dT_mod3 = ifelse(between(time, list_time_drft_head[[i]],
                                                 list_time_drft_tail[[i]]),
                                         dT_mod2_drft_mod, dT_mod3))
      }

      message("Short-term drift correction was finished.")
    }

    ## filter noise
    ## Gaussian filter for noisy time series
    if(filter_noise == TRUE) {
      g_filter <- gsignal::gaussian(wndw_size_noise, inv_sigma_gauss_noise)
      g_filter <- g_filter / sum(g_filter)

      data_dT <-
        data_dT %>%
        dplyr::mutate(dT_mod3 = gsignal::conv(dT_mod3, g_filter,
                                              shape = "same"))

      message("Filtering white noise was finished.")
    } else {
      message("Filtering white noise was skipped.")
    }

    ## separate each sub-period and normalize time series to remove outliers
    message("z-score outlier removal started.")

    n_valid <- function(vctr) { length(na.omit(vctr)) }
    data_dT <-
      data_dT %>%
      dplyr::mutate(dT_mod4 = dT_mod3,
                    hrmin = lubridate::hour(time) + lubridate::minute(time)/60,
                    dT_avg = NA,
                    dT_sd = NA,
                    dT_z = NA)

    n_outliers <- 10^6

    while (n_outliers > 0) {
      for (i in 1:(length(list_time_prd_tail) + 1)) {
        if(i == 1) {
          data_dT <-
            data_dT %>%
            dplyr::mutate(dT_mod4_prd = ifelse(time < list_time_prd_tail[[i]],
                                               dT_mod4, -9999))
        } else if(i == length(list_time_prd_tail) + 1) {
          data_dT <-
            data_dT %>%
            dplyr::mutate(dT_mod4_prd = ifelse(time > list_time_prd_tail[[i - 1]],
                                               dT_mod4, -9999))
        } else {
          data_dT <-
            data_dT %>%
            dplyr::mutate(dT_mod4_prd = ifelse(time > list_time_prd_tail[[i - 1]] &
                                                 time <= list_time_prd_tail[[i]],
                                               dT_mod4, -9999))
        }

        data_dT <-
          data_dT %>%
          dplyr::na_if(-9999) %>%
          dplyr::mutate(dT_n_prd = zoo::rollapply(dT_mod4_prd,
                                                  width = wndw_size,
                                                  FUN = n_valid, fill = NA,
                                                  partial = TRUE),
                        dT_avg_prd = zoo::rollapply(dT_mod4_prd,
                                                    width = wndw_size,
                                                    FUN = mean, fill = NA,
                                                    na.rm = TRUE,
                                                    partial = TRUE),
                        dT_avg_prd = ifelse(dT_n_prd < min_n_wndw, NA,
                                            dT_avg_prd),
                        dT_sd_prd = zoo::rollapply(dT_mod4_prd,
                                                   width = wndw_size,
                                                   FUN = sd, fill = NA,
                                                   na.rm = TRUE,
                                                   partial = min_n_wndw),
                        dT_sd_prd = ifelse(dT_n_prd < min_n_wndw, NA,
                                           dT_sd_prd),
                        dT_z_prd = (dT_mod4_prd - dT_avg_prd) / dT_sd_prd,
                        dT_avg = ifelse(!is.na(dT_mod4_prd), dT_avg_prd,
                                        dT_avg),
                        dT_sd = ifelse(!is.na(dT_mod4_prd), dT_sd_prd, dT_sd),
                        dT_z = ifelse(!is.na(dT_mod4_prd), dT_z_prd, dT_z))
      }

      data_dT <-
        data_dT %>%
        dplyr::mutate(flag_out_z = ifelse(abs(dT_z) > thres_z, 1, 0),
                      flag_out_z = ifelse(is.na(dT_z), 0, flag_out_z),
                      dT_z = ifelse(flag_out_z == 1, NA, dT_z),
                      dT_mod4 = dT_z * dT_sd + dT_avg)

      n_outliers <- data_dT %>% dplyr::pull(flag_out_z) %>% sum()

      if(n_outliers > 1) {
        message(n_outliers,
                " z-score outliers were detected. Continue this process.")
      } else if(n_outliers == 1) {
        message("1 z-score outlier was detected. Continue this process.")
      } else {
        message("No z-score outlier was detected.")
      }
    }

    data_dT <-
      data_dT %>%
      tidyr::replace_na(replace = list(dT_z = -9999))

    message("z-score outlier removal finished.")

    ## correct z-score time series during short damping periods

    ## check short signal damping by peak position
    ## use this function only in this script; this has not been generalized.
    check_short_damping <-
      function(df, window_size = 720, inv_sigma_gauss = 0.01) {
        list_time_start <- NULL
        list_time_peak <- NULL
        list_time_end <- NULL
        list_ratio <- NULL

        data <- df %>% dplyr::select(time, dT_z, dT_sd, dT_avg)

        time_mea_start <-
          data %>%
          dplyr::filter(dT_z > -9999) %>%
          dplyr::slice_head() %>%
          dplyr::pull(time)

        time_mea_end <-
          data %>%
          dplyr::filter(dT_z > -9999) %>%
          dplyr::slice_tail() %>%
          dplyr::pull(time)

        ## Gaussian filter
        g_filter <- gsignal::gaussian(window_size, inv_sigma_gauss)
        g_filter <- g_filter / sum(g_filter)

        data <-
          data %>%
          dplyr::mutate(dT_avg_smth = ifelse(dT_z > -9999, dT_avg, NA),
                        dT_avg_smth = zoo::na.approx(dT_avg_smth, na.rm = FALSE),
                        dT_avg_smth = gsignal::conv(dT_avg_smth, g_filter, shape = "same"),
                        dT_avg_smth_diff1 = dT_avg_smth - lag(dT_avg_smth),
                        dT_avg_smth_diff2 = dT_avg_smth_diff1 - lag(dT_avg_smth_diff1),
                        flag_dT_avg_peak = ifelse(dT_avg_smth_diff1 == 0, 1, 0),
                        flag_dT_avg_peak = ifelse(dT_avg_smth_diff1 * lead(dT_avg_smth_diff1) < 0,
                                                  1, flag_dT_avg_peak),
                        dT_sd_smth = ifelse(dT_z > -9999, dT_sd, NA),
                        dT_sd_smth = zoo::na.approx(dT_sd_smth, na.rm = FALSE),
                        dT_sd_smth = gsignal::conv(dT_sd_smth, g_filter, shape = "same"),
                        dT_sd_smth_diff1 = dT_sd_smth - lag(dT_sd_smth),
                        dT_sd_smth_diff2 = dT_sd_smth_diff1 - lag(dT_sd_smth_diff1),
                        flag_dT_sd_peak = ifelse(dT_sd_smth_diff1 == 0, 1, 0),
                        flag_dT_sd_peak = ifelse(dT_sd_smth_diff1 * lead(dT_sd_smth_diff1) < 0,
                                                 1, flag_dT_sd_peak))

        ## detect peak position
        list_time_avg_upeak <-
          data %>%
          dplyr::filter(flag_dT_avg_peak == 1 & dT_avg_smth_diff2 < 0) %>%
          dplyr::pull(time)

        list_time_sd_peak <-
          data %>%
          dplyr::filter(flag_dT_sd_peak == 1) %>%
          dplyr:: pull(time)

        list_time_sd_dpeak <-
          data %>%
          dplyr::filter(flag_dT_sd_peak == 1 & dT_sd_smth_diff2 > 0) %>%
          dplyr::pull(time)

        if(length(list_time_sd_peak) <= 1) {
          message("There are less than two peaks.")
          break
        }

        ## detect possible damping periods
        for (i in 1:length(list_time_sd_dpeak)) {
          target_time_sd_dpeak <- list_time_sd_dpeak[i]
          num_time_sd_peak <- which(list_time_sd_peak == target_time_sd_dpeak)

          if(num_time_sd_peak == 1) {
            damp_head <- time_mea_start + minutes(30)
            damp_tail <- list_time_sd_peak[num_time_sd_peak + 1] - minutes(30)
          } else if(num_time_sd_peak == length(list_time_sd_peak)) {
            damp_head <- list_time_sd_peak[num_time_sd_peak - 1] + minutes(30)
            damp_tail <- time_mea_end - minutes(30)
          } else {
            damp_head <- list_time_sd_peak[num_time_sd_peak - 1] + minutes(30)
            damp_tail <- list_time_sd_peak[num_time_sd_peak + 1] - minutes(30)
          }

          num_time_avg_peak <-
            which(list_time_avg_upeak >= damp_head & list_time_avg_upeak <= damp_tail)

          data %>%
            dplyr::filter(time >= damp_head & time <= damp_tail) %>% {
              dplyr::slice_head(.) %>% dplyr::pull(dT_sd_smth) ->> dT_sd_head
              dplyr::slice_tail(.) %>% dplyr::pull(dT_sd_smth) ->> dT_sd_tail
              dplyr::filter(., time == target_time_sd_dpeak) %>%
                dplyr::pull(dT_sd_smth) ->> dT_sd_peak
            }

          dT_sd_peak_ratio <-
            round((dT_sd_peak / dT_sd_head + dT_sd_peak / dT_sd_tail) / 2, digits = 5)

          if(length(num_time_avg_peak) >= 1) {
            list_time_start <- c(list_time_start, damp_head)
            list_time_peak <- c(list_time_peak, target_time_sd_dpeak)
            list_time_end <- c(list_time_end, damp_tail)
            list_ratio <- c(list_ratio, dT_sd_peak_ratio)
          }
        }

        ## convert integer to POSIX
        attributes(list_time_start) <- attributes(target_time_sd_dpeak)
        attributes(list_time_peak) <- attributes(target_time_sd_dpeak)
        attributes(list_time_end) <- attributes(target_time_sd_dpeak)

        data.frame(time_start = list_time_start,
                   time_peak = list_time_peak,
                   time_end = list_time_end,
                   ratio = list_ratio) %>%
          return()
      }

    if(check_damping == TRUE) {
      prd_damp <- check_short_damping(data_dT, window_size = wndw_size)
      n_damp <-
        prd_damp %>%
        dplyr::filter(ratio < thres_ratio_damp) %>%
        nrow()

      if(n_damp > 0) {
        list_time_damp_head <-
          prd_damp %>%
          dplyr::filter(ratio < thres_ratio_damp) %>%
          pull(time_start)

        list_time_damp_tail <-
          prd_damp %>%
          dplyr::filter(ratio < thres_ratio_damp) %>%
          pull(time_end)

        message("Short damping periods were detected at ",
                     list_time_damp_head))

        for (i_damp in n_damp) {
          data_dT <-
            data_dT %>%
            dplyr::mutate(dT_avg = ifelse(time >= list_time_damp_head[[i_damp]] &
                                            time <= list_time_damp_tail[[i_damp]],
                                          NA, dT_avg),
                          dT_avg = zoo::na.approx(dT_avg, na.rm = FALSE),
                          dT_sd = ifelse(time >= list_time_damp_head[[i_damp]] &
                                           time <= list_time_damp_tail[[i_damp]],
                                         NA, dT_sd),
                          dT_sd = zoo::na.approx(dT_sd, na.rm = FALSE),
                          dT_z = (dT_mod4 - dT_avg) / dT_sd)
        }
      }
    }

    ## define reference values of dT average and standard deviation
    time_head_ref_1st <-
      data_dT %>%
      dplyr::filter(!is.na(dT_mod4)) %>%
      dplyr::slice_head() %>%
      dplyr::pull(time)

    list_dT_avg_ref <- rep(NA, length(list_time_prd_tail) + 1)
    list_dT_sd_ref <- rep(NA, length(list_time_prd_tail) + 1)

    for (i in 1:(length(list_time_prd_tail) + 1)) {
      if(i == 1) {
        data_dT %>%
          dplyr::mutate(dT_mod4_prd = ifelse(time >= time_head_ref_1st &
                                               time < time_head_ref_1st +
                                               lubridate::minutes(30 * wndw_size),
                                             dT_mod4, NA)) %>%
          dplyr::pull(dT_mod4_prd) %>% {
            mean(., na.rm = TRUE) ->> list_dT_avg_ref[[i]]
            sd(., na.rm = TRUE) ->> list_dT_sd_ref[[i]]
          }

      } else {
        data_dT %>%
          dplyr::mutate(dT_mod4_prd = ifelse(time > list_time_prd_tail[[i - 1]] &
                                               time <= list_time_prd_tail[[i - 1]] +
                                               lubridate::minutes(30 * wndw_size),
                                             dT_mod4, NA)) %>%
          dplyr::pull(dT_mod4_prd) %>% {
            mean(., na.rm = TRUE) ->> list_dT_avg_ref[[i]]
            sd(., na.rm = TRUE) ->> list_dT_sd_ref[[i]]
          }
      }
    }

    dT_avg_ref <- median(list_dT_avg_ref, na.rm = TRUE)
    dT_sd_ref <- median(list_dT_sd_ref, na.rm = TRUE)

    message("Refenrence values for correcting long-term shifting were determined.")
    message("Refenrence value for dT average: ", round(dT_avg_ref, digits = 5))
    message("Refenrence value for dT standard deviation: ",
            round(dT_sd_ref, digits = 5))

    message("Outlier removal process was finished successfully.")
    return(data_dT)
  }





## test (Not Run)
# library(dplyr)
# library(tidyr)
# library(lubridate)
# library(ggplot2)
# time_test <- dt_noisy$time
# dt_test <- dt_noisy$dt
#
# err_head <- list(ymd_hm("2013/05/14 13:00"),
#                  ymd_hm("2016/12/14 12:30"))
# err_tail <- list(ymd_hm("2013/05/14 13:00"),
#                  ymd_hm("2016/12/14 21:00"))
#
# drft_head <- list(ymd_hm("2013/05/14 13:30"),
#                   ymd_hm("2015/06/02 10:00"),
#                   ymd_hm("2016/08/06 15:00"))
# drft_tail <- list(ymd_hm("2013/05/17 15:00"),
#                   ymd_hm("2015/06/09 09:30"),
#                   ymd_hm("2016/08/09 11:00"))
#
# prd_tail <- list(ymd_hm("2013/05/14 13:00"),
#                  ymd_hm("2014/05/13 15:30"),
#                  ymd_hm("2015/06/02 09:30"),
#                  ymd_hm("2016/05/25 09:00"))
#
# drft_head <- list(ymd_hm("2013/05/14 13:30"))
# drft_tail <- list(ymd_hm("2013/05/17 15:00"))
# prd_tail <- list(ymd_hm("2013/05/14 13:00"))
#
# rslt_test_1 <- remove_outlier(time_test, dt_test, prd_tail,
#                               err_head, err_tail, drft_head, drft_tail,
#                               filter_noise = FALSE)
#
# rslt_test_1 %>%
#   ggplot()+
#   scale_x_datetime(limits = c(ymd_hm("2013/05/01 00:30"), ymd_hm("2013/06/01 00:00")))+
#   geom_point(aes(time, dT), col = "black", size = 1)+
#   geom_point(aes(time, dT_mod3), col = "red3", size = 1, alpha = 0.3)
#
# rslt_test_1 %>%
#   ggplot()+
#   scale_y_continuous(limits = c(-5, 20))+
#   geom_point(aes(time, dT), col = "gray", size = 1)+
#   geom_point(aes(time, dT_avg), col = "red3", size = 1, alpha = 0.3)+
#   geom_point(aes(time, dT_sd), col = "black", size = 1, alpha = 0.3)+
#   geom_point(aes(time, dT_z), col = "blue", size = 1, alpha = 0.3)





