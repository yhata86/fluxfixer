#' Calculate dTmax by the predawn method and environmental dependent method
#'
#' @description calc_dtmax_pd_ed() calculates daily maximum temperature
#'   difference between granier-type sap flow probes (dTmax) by the predawn
#'   method and environmental dependent method.
#'
#' @param vctr_time Timestamp vector
#' @param vctr_dt Delta T (temperature difference between probes) time series
#' @param vctr_rs Incident short-wave radiation time series
#' @param vctr_vpd Water vapor pressure deficit time series
#' @param thres_rs Threshold value of predawn short-wave radiation
#' @param thres_vpd Threshold value of predawn short-wave radiation
#' @param thres_cv Threshold value of coefficient of variation
#' @param thres_hour Threshold value of predawn hour
#' @param min_n_wndw Threshold value of minimum data points in moving window
#'
#' @examples
#' time_test <- dt_noisy$time
#' dt_test <- dt_noisy$dt
#' rs_test <- dt_noisy$rs
#' vpd_test <- dt_noisy$vpd
#'
#' # Calculate dTmax from data
#' result <-
#'   calc_dtmax_pd_ed(time_test, dt_test, rs_test, vpd_test)

calc_dtmax_pd_ed <-
  function(vctr_time, vctr_dt, vctr_rs, vctr_vpd,
           thres_rs = 100, thres_vpd = 6.0, thres_cv = 0.005,
           thres_hour = 8, min_n_wndw = 3) {
    ## settings
    vctr_time_2330 <-
      vctr_time %>%
      data.frame(time = .) %>%
      dplyr::filter(lubridate::hour(time) == 23 &
                      lubridate::minute(time) == 30) %>%
      dplyr::pull(time)

    n_point <- length(vctr_time)
    n_day <- length(vctr_time_2330)

    time_start <- vctr_time[1]
    wndw_head <- time_start - lubridate::days(1)
    pb <- txtProgressBar(min = 1, max = n_day, style = 3)

    list_dTmax_PD <- rep(NA, n_day)
    list_dTmax_ED <- rep(NA, n_day)

    all_data <-
      data.frame(time = vctr_time,
                 dT = vctr_dt,
                 Rs_in = vctr_rs,
                 VPD = vctr_vpd) %>%
      dplyr::mutate(time_lag = time - lubridate::minutes(30)) %>%
      dplyr::na_if(-9999)

    for (i in 1:n_day) {
      setTxtProgressBar(pb, i)

      wndw_head <- wndw_head + lubridate::days(1)
      wndw_tail <- wndw_head + lubridate::days(1) - lubridate::minutes(30)

      wndw <-
        all_data %>%
        dplyr::filter(time >= wndw_head & time <= wndw_tail & !is.na(dT) &
                        lubridate::hour(time_lag) < thres_hour &
                        Rs_in < thres_rs)

      if(nrow(wndw) < min_n_wndw) next

      ## PD dTmax calculation
      wndw %>%
        arrange(dT) %>%
        slice_tail() %>% {
          pull(., dT) ->> list_dTmax_PD[i]
          pull(., time) ->> time_dTmax_PD
        }

      ## ED dTmax calculation
      wndw_2h <-
        all_data %>%
        dplyr::filter(time >= time_dTmax_PD - lubridate::hours(1) - lubridate::minutes(30) &
                        time <= time_dTmax_PD &
                        !is.na(dT))

      if(nrow(wndw_2h) < min_n_wndw) next
      if(mean(wndw_2h$VPD, na.rm = TRUE) >= thres_vpd) next
      if(sd(wndw_2h$dT) / mean(wndw_2h$dT) >= thres_cv) next

      list_dTmax_ED[i] <- list_dTmax_PD[i]
    }
    data.frame(time = vctr_time_2330,
               dTmax_PD = list_dTmax_PD,
               dTmax_ED = list_dTmax_ED) %>%
      mutate(dTmax_PD = zoo::na.approx(dTmax_PD, na.rm = FALSE),
             dTmax_ED = zoo::na.approx(dTmax_ED, na.rm = FALSE)) %>%
      return()
  }


#' Calculate dTmax by the moving window method, double regression method,
#' and double average method
#'
#' @description calc_dtmax_mw_da_dr() calculates daily maximum temperature
#'   difference between granier-type sap flow probes (dTmax) by moving window
#'   method, double average method, and double regression method.
#'
#' @param vctr_time Timestamp vector
#' @param vctr_dtmax_pd Delta Tmax time series calculated by predawn method
#' @param window_size The number of data points in a moving window
#' @param min_n_wndw Threshold value of minimum data points in moving window
#'
#' @examples
#' # Calculate dTmax from data
#' time_test <- dt_noisy$time
#' dtmax_pd_test <- dtmax$dtmax_PD
#'
#' result <-
#'   calc_dtmax_mw_da_dr(time_test, dtmax_pd_test)

calc_dtmax_mw_da_dr <-
  function(vctr_time, vctr_dtmax_pd, wndw_size = 11, min_n_wndw = 3) {
    n_day <- length(vctr_time)
    list_dTmax_MW <- rep(NA, n_day)
    list_dTmax_DA <- rep(NA, n_day)
    list_dTmax_DR <- rep(NA, n_day)
    time_start <- vctr_time[1]
    time_end <- vctr_time[n_day]
    pb <- txtProgressBar(min = 1, max = n_day, style = 3)

    all_data <-
      data.frame(time = vctr_time,
                 dTmax_PD = vctr_dtmax_pd) %>%
      dplyr::mutate(rownum = rownames(.) %>% as.numeric()) %>%
      dplyr::na_if(-9999)

    for (i in 1:n_day) {
      setTxtProgressBar(pb, i)

      time_now <- vctr_time[i]
      wndw_head <- time_now - lubridate::days((wndw_size - 1)/2)
      wndw_tail <- time_now + lubridate::days((wndw_size - 1)/2)

      wndw <-
        all_data %>%
        dplyr::filter(time >= wndw_head & time <= wndw_tail & !is.na(dTmax_PD))

      if(nrow(wndw) < min_n_wndw) next

      ## estimate dTmax by moving window method
      list_dTmax_MW[i] <- wndw %>% dplyr::pull(dTmax_PD) %>% max()

      ## estimate dTmax by double average method
      ave_wndw <- wndw %>% dplyr::pull(dTmax_PD) %>% mean()
      list_dTmax_DA[i] <-
        wndw %>%
        dplyr::filter(dTmax_PD >= ave_wndw) %>%
        dplyr::pull(dTmax_PD) %>%
        mean()

      ## estimate dTmax by double regression method
      lm_dTmax_1st <- lm(dTmax_PD ~ rownum, data = wndw)

      wndw %<>%
        mutate(pred_1st = predict(lm_dTmax_1st) %>% as.numeric(),
               dTmax_PD_filtered = ifelse(dTmax_PD < pred_1st, NA, dTmax_PD)) %>%
        dplyr::filter(!is.na(dTmax_PD_filtered))

      if(nrow(wndw) < min_n_wndw) next

      lm_dTmax_2nd <- lm(dTmax_PD_filtered ~ rownum, data = wndw)

      lm_dTmax_2nd %>% summary() %>% .$coef %>% {
        .[1, 1] ->> init_lm_dTmax_2nd
        .[2, 1] ->> slp_lm_dTmax_2nd
      }

      list_dTmax_DR[i] <- slp_lm_dTmax_2nd * i + init_lm_dTmax_2nd
    }
    data.frame(dTmax_MW = list_dTmax_MW,
               dTmax_DA = list_dTmax_DA,
               dTmax_DR = list_dTmax_DR) %>%
      mutate(dTmax_MW = zoo::na.approx(dTmax_MW, na.rm = FALSE),
             dTmax_DA = zoo::na.approx(dTmax_DA, na.rm = FALSE),
             dTmax_DR = zoo::na.approx(dTmax_DR, na.rm = FALSE)) %>%
      return()
  }


#' Calculate dTmax from half-hourly dT time series by six different methods
#'
#' @description calc_dtmax_30min() calculates daily maximum temperature
#'   difference between granier-type sap flow probes (dTmax) by the predawn
#'   method, predawn slide method, moving window method, double average method,
#'   double regression method, and environmental dependent method.
#'
#' @param vctr_time Timestamp vector
#' @param vctr_dt Delta T time series
#' @param vctr_rs Incident short-wave radiation time series
#' @param vctr_vpd Vapor pressure deficit time series
#' @param prd_all Target period for dTmax estimation
#'
#' @examples
#' time_test <- dt_noisy$time
#' dt_test <- dt_noisy$dt
#' rs_test <- dt_noisy$rs
#' vpd_test <- dt_noisy$vpd
#'
#' # Calculate dTmax from data
#' result <-
#'   calc_dtmax_30min(time_test, dt_test, rs_test, vpd_test,
#'                    prd_all = "2012-09-08/2013-09-07")


calc_dtmax_30min <-
  function(vctr_time, vctr_dt, vctr_rs, vctr_vpd, prd_all) {
    timezone <- lubridate::tz(vctr_time[1])

    data <- data.frame(time = vctr_time,
                       dt = vctr_dt,
                       Rs_in = vctr_rs,
                       VPD = vctr_vpd)

    ## FV dTmax calculation
    print("FV dTmax calculaton started.")

    daily_dTmax_FV <-
      data %>%
      dplyr::transmute(time_0500 = time - lubridate::hours(5) -
                         lubridate::minutes(30),
                       dt = dt) %>%
      zoo::read.zoo() %>%
      xts::as.xts(tzone = timezone) %>%
      xts::apply.daily(function(x) sapply(x, max, na.rm = TRUE)) %>%
      dplyr::na_if(-Inf) %>%
      .[prd_all] %>%
      zoo::fortify.zoo() %>%
      dplyr::rename(time = Index, dtmax_fv = ".") %>%
      dplyr::mutate(dtmax_fv = zoo::na.approx(dtmax_fv, na.rm = FALSE))

    print("FV dTmax calculaton finished.")

    ## PD and ED dTmax calculation
    print("PD and ED dTmax calculaton started.")

    daily_dTmax_PD_ED <-
      calc_dtmax_pd_ed(data$time, data$dt, data$Rs_in, data$VPD)

    print("PD and ED dTmax calculaton finished.")

    ## MW, DA, and DR dTmax calculations
    print("MW, DA, and DR dTmax calculaton started.")

    daily_dTmax_MW_DA_DR <-
      calc_dtmax_mw_da_dr(daily_dTmax_PD_ED$time, daily_dTmax_PD_ED$dTmax_PD)

    print("MW, DA, and DR dTmax calculatons finished.")

    ## aggregate estimated values
    print("dTmax time series aggregation started.")
    daily_dTmax_PD_MW_DA_DR_ED <-
      data.frame(time_lag = daily_dTmax_PD_ED$time,
                 dtmax_pd = daily_dTmax_PD_ED$dTmax_PD,
                 dtmax_mw = daily_dTmax_MW_DA_DR$dTmax_MW,
                 dtmax_da = daily_dTmax_MW_DA_DR$dTmax_DA,
                 dtmax_dr = daily_dTmax_MW_DA_DR$dTmax_DR,
                 dtmax_ed = daily_dTmax_PD_ED$dTmax_ED)

    dTmax <-
      data %>%
      dplyr::select(time, dt) %>%
      dplyr::mutate(time_0500 = time - lubridate::hours(5) - lubridate::minutes(30),
                    time_lag = time - lubridate::minutes(30)) %>%
      merge.data.frame(., daily_dTmax_FV, by.x = "time_0500", by.y = "time",
                       all = TRUE) %>%
      merge.data.frame(., daily_dTmax_PD_MW_DA_DR_ED,
                       by = "time_lag", all = TRUE) %>%
      tidyr::fill(c(dtmax_fv, dtmax_pd, dtmax_mw, dtmax_da, dtmax_dr, dtmax_ed),
                  .direction = "updown") %>%
      dplyr::select(time, dt,
                    dtmax_fv, dtmax_pd, dtmax_mw, dtmax_da, dtmax_dr, dtmax_ed)

    print("dTmax time series aggregation finished.")

    ## output
    return(dTmax)
  }


#' Calculate Fd from six different dTmax time series
#'
#' @description calc_fd_30min() calculates sap flow rate using Granier equation
#'   from dTmax time series estimated by the predawn method,
#'   predawn slide method, moving window method, double average method,
#'   double regression method, and environmental dependent method.
#'
#' @param df_dtmax Dataframe outputted from calc_dtmax_30min()
#' @param alpha Parameter in Granier equation (m/s)
#' @param beta Parameter in Granier equation
#' @param do_heartwood_correction Logical. Do Heartwood correction described in Clearwater et al. (1999)
#' @param ratio_conductive Ratio of conductive versus non-conductive length
#'
#' @examples
#' # Calculate Fd from data
#' data_dtmax <- dtmax
#' result <-
#'   calc_fd_30min(data_dtmax)

calc_fd_30min <-
  function(df_dtmax, alpha = 1.19 * 10^{-4}, beta = 1.231,
           do_heartwood_correction = FALSE, ratio_conductive = NULL) {
    if(do_heartwood_correction) {
      df_fd <-
        df_dtmax %>%
        dplyr::mutate(dt_sw_fv = (dt - (1 - ratio_conductive) * dtmax_fv) /
                        ratio_conductive,
                      dt_sw_pd = (dt - (1 - ratio_conductive) * dtmax_pd) /
                        ratio_conductive,
                      dt_sw_mw = (dt - (1 - ratio_conductive) * dtmax_mw) /
                        ratio_conductive,
                      dt_sw_da = (dt - (1 - ratio_conductive) * dtmax_da) /
                        ratio_conductive,
                      dt_sw_dr = (dt - (1 - ratio_conductive) * dtmax_dr) /
                        ratio_conductive,
                      dt_sw_ed = (dt - (1 - ratio_conductive) * dtmax_ed) /
                        ratio_conductive,
                      fd_fv = ifelse(dtmax_fv - dt_sw_fv >= 0,
                                     alpha * ((dtmax_fv - dt_sw_fv) / dt_sw_fv)^
                                       beta, 0),
                      fd_pd = ifelse(dtmax_pd - dt_sw_pd >= 0,
                                     alpha * ((dtmax_pd - dt_sw_pd) / dt_sw_pd)^
                                       beta, 0),
                      fd_mw = ifelse(dtmax_mw - dt_sw_mw >= 0,
                                     alpha * ((dtmax_mw - dt_sw_mw) / dt_sw_mw)^
                                       beta, 0),
                      fd_da = ifelse(dtmax_da - dt_sw_da >= 0,
                                     alpha * ((dtmax_da - dt_sw_da) / dt_sw_da)^
                                       beta, 0),
                      fd_dr = ifelse(dtmax_dr - dt_sw_dr >= 0,
                                     alpha * ((dtmax_dr - dt_sw_dr) / dt_sw_dr)^
                                       beta, 0),
                      fd_ed = ifelse(dtmax_ed - dt_sw_ed >= 0,
                                     alpha * ((dtmax_ed - dt_sw_ed) / dt_sw_ed)
                                     ^beta, 0))
    } else {
      df_fd <-
        df_dtmax %>%
        dplyr::mutate(fd_fv = ifelse(dtmax_fv - dt >= 0,
                                     alpha * ((dtmax_fv - dt) / dt)^beta, 0),
                      fd_pd = ifelse(dtmax_pd - dt >= 0,
                                     alpha * ((dtmax_pd - dt) / dt)^beta, 0),
                      fd_mw = ifelse(dtmax_mw - dt >= 0,
                                     alpha * ((dtmax_mw - dt) / dt)^beta, 0),
                      fd_da = ifelse(dtmax_da - dt >= 0,
                                     alpha * ((dtmax_da - dt) / dt)^beta, 0),
                      fd_dr = ifelse(dtmax_dr - dt >= 0,
                                     alpha * ((dtmax_dr - dt) / dt)^beta, 0),
                      fd_ed = ifelse(dtmax_ed - dt >= 0,
                                     alpha * ((dtmax_ed - dt) / dt)^beta, 0)) %>%
        select(time, tidyr::starts_with("fd"))
    }

    return(df_fd)
  }

## test
# library(dplyr)
# library(lubridate)
# library(magrittr)
# library(zoo)
# library(xts)
# library(tidyr)
# library(ggplot2)
# time_test <- dt_gf$time
# dt_test <- dt_gf$dt
# rs_test <- dt_gf$rs
# vpd_test <- dt_gf$vpd
# rslt_test_1 <- calc_dtmax_pd_ed(time_test, dt_test, rs_test, vpd_test)
#
# dTmax_PD_test <- rslt_test_1$dTmax_PD
# dTmax_ED_test <- rslt_test_1$dTmax_ED
# rslt_test_2 <- calc_dtmax_mw_da_dr(rslt_test_1$time, dTmax_PD_test)
# dTmax_MW_test <- rslt_test_2$dTmax_MW
# dTmax_DA_test <- rslt_test_2$dTmax_DA
# dTmax_DR_test <- rslt_test_2$dTmax_DR
#
# dTmax_PD_ref <-
#   dtmax %>%
#   filter(hour(time) == 23 & minute(time) == 30) %>%
#   pull(dTmax_PD_La1_01)
#
# dTmax_ED_ref <-
#   dtmax %>%
#   filter(hour(time) == 23 & minute(time) == 30) %>%
#   pull(dTmax_ED_La1_01)
#
# dTmax_MW_ref <-
#   dtmax %>%
#   filter(hour(time) == 23 & minute(time) == 30) %>%
#   pull(dTmax_MW_La1_01)
#
# dTmax_DA_ref <-
#   dtmax %>%
#   filter(hour(time) == 23 & minute(time) == 30) %>%
#   pull(dTmax_DA_La1_01)
#
# dTmax_DR_ref <-
#   dtmax %>%
#   filter(hour(time) == 23 & minute(time) == 30) %>%
#   pull(dTmax_DR_La1_01)
#
# summary(dTmax_PD_ref - dTmax_PD_test)
# summary(dTmax_ED_ref - dTmax_ED_test)
# summary(dTmax_MW_ref - dTmax_MW_test)
# summary(dTmax_DA_ref - dTmax_DA_test)
# summary(dTmax_DR_ref - dTmax_DR_test)
#
# rslt_test_3 <- calc_dtmax_30min(time_test, dt_test, rs_test, vpd_test)
# rslt_test_3 %>%
#   select(-dt) %>%
#   pivot_longer(cols = -time, names_to = "method", values_to = "dtmax") %>%
#   ggplot()+
#   scale_x_datetime(limits = c(ymd_hm("2012/09/08 00:30"), ymd_hm("2012/10/08 00:00")))+
#   geom_point(data = dt_gf, aes(time, dt), col = "black", size = 1)+
#   geom_line(aes(time, dtmax, col = method))
# fd_test <- calc_fd_30min(rslt_test_3)
# fd_test %>%
#   pivot_longer(cols = -time, names_to = "method", values_to = "fd") %>%
#   ggplot()+
#   scale_x_datetime(limits = c(ymd_hm("2012/09/08 00:30"), ymd_hm("2012/10/08 00:00")))+
#   geom_line(aes(time, fd, col = method))

