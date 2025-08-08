#' Calculate dTmax by the successive predawn method
#'
#' @description `calc_dtmax_sp()` calculates the daily time series of dTmax
#'   (the maximum temperature difference between sap flow probes under
#'   zero-flow conditions) using the successive predawn method.
#'
#' @details The successive predawn method is one of the methods determining
#'   the maximum temperature difference between sap flow probes under
#'   zero-flow conditions. This method assumes that...(to be filled)
#'
#' @param vctr_time Timestamp vector of class POSIXct or POSIXt. This vector
#'   indicates the timings of the end of each measurement in local time.
#'   Any interval (typically 15 to 60 min) is allowed, but the timestamps must
#'   be equally spaced and arranged chronologically.
#' @param vctr_dt Vector of dT (the temperature difference between sap flow
#'   probes) time series. Missing values must be gap-filled previously.
#' @param thres_hour Numerical. The threshold hour of the day which defines
#'   the start of predawn in local time. Non-negative and less than 24
#'   (default = 5).
#' @param output_daily Logical. If `TRUE`, returns dTmax time series in daily
#'   steps; else, returns dTmax in the original time steps. Default is `FALSE`.
#'
#' @returns
#' A data frame with n columns.
#'
#' * The first column, `time`, gives timestamp of the measurements.
#'   If `output_daily` is `FALSE` (default), this column is the same as input
#'   timestamp, `vctr_time`. If `output_daily` is `TRUE`, timestamps in daily
#'   steps returns.
#' * The second column, `dt
#' `, gives ...
#'
#' @examples
#' library(dplyr)
#' library(lubridate)
#' library(zoo)
#' library(xts)
#' library(tidyr)
#'
#' data(dt_gf)
#' time <- dt_gf$time[seq(1, 48 * 10)]
#' dt <- dt_gf$dt[seq(1, 48 * 10)]
#'
#' # Calculate dTmax from gap-filled dT time series
#' result <- calc_dtmax_sp(time, dt)
#'
#' @author Yoshiaki Hata
#'
#' @seealso `calc_dtmax_all`, `calc_dtmax_pd`, `calc_dtmax_mw`, `calc_dtmax_dr`,
#'   `calc_dtmax_ed`
#'
#' @include utils.R
#' @export

calc_dtmax_sp <-
  function(vctr_time, vctr_dt, thres_hour = 5, output_daily = FALSE) {
    ## avoid "No visible binding for global variable" notes
    time <- NULL
    dt <- NULL
    Index <- NULL
    dtmax_sp <- NULL
    . <- NULL

    ## check input values
    interval_time <- get_interval(vctr_time)
    timezone <- lubridate::tz(vctr_time[1])

    data_all <- data.frame(time = vctr_time,
                           dt = vctr_dt)

    ## SP dTmax calculation
    message("dTmax calculation by the SP method started")

    daily_dTmax <-
      data_all %>%
      dplyr::transmute(time_shifted = time - lubridate::hours(thres_hour) -
                         lubridate::minutes(interval_time),
                       dt = dt) %>%
      zoo::read.zoo() %>%
      xts::as.xts(tzone = timezone) %>%
      xts::apply.daily(function(x) sapply(x, max, na.rm = TRUE)) %>%
      dplyr::na_if(-Inf) %>%
      zoo::fortify.zoo() %>%
      dplyr::rename(time = Index, dtmax_sp = ".") %>%
      dplyr::filter(time >= vctr_time[1] &
                      time <= vctr_time[length(vctr_time)]) %>%
      dplyr::mutate(dtmax_sp = zoo::na.approx(dtmax_sp, na.rm = FALSE))

    message("dTmax calculation by the SP method finished")

    ## output
    if(output_daily) {
      return(daily_dTmax)
    } else {
      data_all %>%
        dplyr::select(time, dt) %>%
        dplyr::mutate(time_shifted = time - lubridate::hours(thres_hour) -
                        lubridate::minutes(interval_time)) %>%
        merge.data.frame(., daily_dTmax, by.x = "time_shifted", by.y = "time",
                         all = TRUE) %>%
        tidyr::fill(dtmax_sp, .direction = "updown") %>%
        dplyr::select(time, dt, dtmax_sp) %>%
        return()
    }
  }


#' Calculate dTmax by the predawn method
#'
#' @description calc_dtmax_pd() calculates daily maximum temperature
#'   difference between granier-type sap flow probes (dTmax) by the predawn
#'   method.
#'
#' @param vctr_time Timestamp vector
#' @param vctr_dt Delta T (temperature difference between probes) time series
#' @param vctr_radi Incident radiation time series
#' @param thres_radi Threshold value of predawn short-wave radiation
#' @param thres_hour Threshold value of predawn hour
#' @param min_n_wndw Threshold value of minimum data points in moving window
#' @param err_label Error value label representing missing value
#' @param output_daily Logical. If true, return daily dTmax; else, return dTmax
#'          with the original time resolution
#'
#' @examples
#' time_test <- dt_gf$time[seq(1, 48 * 10)]
#' dt_test <- dt_gf$dt[seq(1, 48 * 10)]
#' radi_test <- dt_gf$rs[seq(1, 48 * 10)]
#'
#' # Calculate dTmax from cleaned data
#' result <- calc_dtmax_pd(time_test, dt_test, radi_test)
#'
#' @include utils.R
#' @export

calc_dtmax_pd <-
  function(vctr_time, vctr_dt, vctr_radi, thres_radi = 100, thres_hour = 8,
           min_n_wndw = 3, err_label = -9999, output_daily = FALSE) {
    ## avoid "No visible binding for global variable" notes
    time <- NULL
    txtProgressBar <- NULL
    setTxtProgressBar <- NULL
    dt <- NULL
    time_lag <- NULL
    radi <- NULL
    . <- NULL
    dtmax_pd <- NULL

    ## check input values
    interval_time <- get_interval(vctr_time)

    ## create time vector for output
    time_head_output <-
      vctr_time[1] %>%
      lubridate::ceiling_date(unit = "day") %>%
      - lubridate::minutes(interval_time)

    time_tail_output <-
      vctr_time[length(vctr_time)] %>%
      lubridate::ceiling_date(unit = "day") %>%
      - lubridate::minutes(interval_time)

    vctr_time_output <-
      seq(time_head_output, time_tail_output, by = "1 day")

    vctr_time_output[length(vctr_time_output)] <- vctr_time[length(vctr_time)]

    n_point <- length(vctr_time)
    n_day <- length(vctr_time_output)

    time_start <- vctr_time[1]
    wndw_head <- time_start - lubridate::days(1)
    pb <- txtProgressBar(min = 1, max = n_day, style = 3)

    list_dTmax_PD <- rep(NA, n_day)

    data_all <-
      data.frame(time = vctr_time,
                 dt = vctr_dt,
                 radi = vctr_radi) %>%
      dplyr::mutate(time_lag = time - lubridate::minutes(interval_time)) %>%
      dplyr::na_if(err_label)

    message("dTmax calculation by the PD method started")

    for (i in 1:n_day) {
      setTxtProgressBar(pb, i)

      wndw_head <- wndw_head + lubridate::days(1)
      wndw_tail <-
        wndw_head + lubridate::days(1) - lubridate::minutes(interval_time)

      wndw <-
        data_all %>%
        dplyr::filter(time >= wndw_head & time <= wndw_tail & !is.na(dt) &
                        lubridate::hour(time_lag) < thres_hour &
                        radi < thres_radi)

      if(nrow(wndw) < min_n_wndw) next

      ## PD dTmax calculation
      wndw %>%
        dplyr::arrange(dt) %>%
        dplyr::slice_tail() %>% {
          dplyr::pull(., dt) ->> list_dTmax_PD[i]
        }
    }

    message("\ndTmax calculation by the PD method finished")

    daily_dTmax <-
      data.frame(time = vctr_time_output,
                 dtmax_pd = list_dTmax_PD) %>%
      dplyr::mutate(dtmax_pd = zoo::na.approx(dtmax_pd, na.rm = FALSE))

    if(output_daily) {
      return(daily_dTmax)
    } else {
      data_all %>%
        dplyr::select(time, dt) %>%
        dplyr::mutate(time_lag = time - lubridate::minutes(interval_time)) %>%
        merge.data.frame(., daily_dTmax, by.x = "time_lag", by.y = "time",
                         all = TRUE) %>%
        tidyr::fill(dtmax_pd, .direction = "updown") %>%
        dplyr::select(time, dt, dtmax_pd) %>%
        return()
    }
  }


#' Calculate dTmax by the environmental dependent method
#'
#' @description calc_dtmax_ed() calculates daily maximum temperature
#'   difference between granier-type sap flow probes (dTmax) by the
#'   environmental dependent method.
#'
#' @param vctr_time Timestamp vector
#' @param vctr_dt Delta T (temperature difference between probes) time series
#' @param vctr_radi Incident radiation time series
#' @param vctr_vpd Water vapor pressure deficit time series
#' @param thres_radi Threshold value of predawn short-wave radiation (W/m2)
#' @param thres_vpd Threshold value of vapor pressure deficit (hPa)
#' @param thres_cv Threshold value of coefficient of variation
#' @param thres_hour Threshold value of predawn hour
#' @param min_n_wndw Threshold value of minimum data points in moving window
#' @param err_label Error value label representing missing value
#' @param output_daily Logical. If true, return daily dTmax; else, return dTmax
#'          with the original time resolution
#'
#' @examples
#' time_test <- dt_gf$time[seq(1, 48 * 10)]
#' dt_test <- dt_gf$dt[seq(1, 48 * 10)]
#' radi_test <- dt_gf$rs[seq(1, 48 * 10)]
#' vpd_test <- dt_gf$vpd[seq(1, 48 * 10)]
#'
#' # Calculate dTmax from cleaned data
#' result <-
#'  calc_dtmax_ed(time_test, dt_test, radi_test, vpd_test, thres_vpd = 6.0)
#'
#' @include utils.R
#' @export

calc_dtmax_ed <-
  function(vctr_time, vctr_dt, vctr_radi, vctr_vpd,
           thres_radi = 100, thres_vpd = 1.0, thres_cv = 0.005,
           thres_hour = 8, min_n_wndw = 3, err_label = -9999,
           output_daily = FALSE) {
    ## avoid "No visible binding for global variable" notes
    txtProgressBar <- NULL
    time <- NULL
    setTxtProgressBar <- NULL
    dt <- NULL
    time_lag <- NULL
    radi <- NULL
    . <- NULL
    time_dTmax_PD <- NULL
    sd <- NULL
    dtmax_ed <- NULL

    ## check input values
    interval_time <- get_interval(vctr_time)

    if(anyNA(vctr_radi) == TRUE) {
      stop("one or more NA values exist in the input radiation time series")
    }
    if(anyNA(vctr_vpd) == TRUE) {
      stop("one or more NA values exist in the input VPD time series")
    }
    if(min(vctr_vpd) < 0) {
      stop("negative VPD values are not allowed")
    }

    ## create time vector for output
    time_head_output <-
      vctr_time[1] %>%
      lubridate::ceiling_date(unit = "day") %>%
      - lubridate::minutes(interval_time)

    time_tail_output <-
      vctr_time[length(vctr_time)] %>%
      lubridate::ceiling_date(unit = "day") %>%
      - lubridate::minutes(interval_time)

    vctr_time_output <-
      seq(time_head_output, time_tail_output, by = "1 day")

    n_point <- length(vctr_time)
    n_day <- length(vctr_time_output)

    time_start <- vctr_time[1]
    wndw_head <- time_start - lubridate::days(1)
    pb <- txtProgressBar(min = 1, max = n_day, style = 3)

    list_dTmax_PD <- rep(NA, n_day)
    list_dTmax_ED <- rep(NA, n_day)

    data_all <-
      data.frame(time = vctr_time,
                 dt = vctr_dt,
                 radi = vctr_radi,
                 VPD = vctr_vpd) %>%
      dplyr::mutate(time_lag = time - lubridate::minutes(interval_time)) %>%
      dplyr::na_if(err_label)

    message("dTmax calculation by the ED method started")

    for (i in 1:n_day) {
      setTxtProgressBar(pb, i)

      wndw_head <- wndw_head + lubridate::days(1)
      wndw_tail <-
        wndw_head + lubridate::days(1) - lubridate::minutes(interval_time)

      wndw <-
        data_all %>%
        dplyr::filter(time >= wndw_head & time <= wndw_tail & !is.na(dt) &
                        lubridate::hour(time_lag) < thres_hour &
                        radi < thres_radi)

      if(nrow(wndw) < min_n_wndw) next

      ## PD dTmax calculation
      wndw %>%
        dplyr::arrange(dt) %>%
        dplyr::slice_tail() %>% {
          dplyr::pull(., dt) ->> list_dTmax_PD[i]
          dplyr::pull(., time) ->> time_dTmax_PD
        }

      ## ED dTmax calculation
      wndw_2h <-
        data_all %>%
        dplyr::filter(time >= time_dTmax_PD - lubridate::hours(2) +
                        lubridate::minutes(interval_time) &
                        time <= time_dTmax_PD &
                        !is.na(dt))

      if(nrow(wndw_2h) < min_n_wndw) next
      if(mean(wndw_2h$VPD, na.rm = TRUE) >= thres_vpd) next
      if(sd(wndw_2h$dt) / mean(wndw_2h$dt) >= thres_cv) next

      list_dTmax_ED[i] <- list_dTmax_PD[i]
    }

    message("\ndTmax calculation by the ED method finished")

    daily_dTmax <-
      data.frame(time = vctr_time_output,
                 dtmax_ed = list_dTmax_ED) %>%
      dplyr::mutate(dtmax_ed = zoo::na.approx(dtmax_ed, na.rm = FALSE))

    if(output_daily) {
      return(daily_dTmax)
    } else {
      data_all %>%
        dplyr::select(time, dt) %>%
        dplyr::mutate(time_lag = time - lubridate::minutes(interval_time)) %>%
        merge.data.frame(., daily_dTmax, by.x = "time_lag", by.y = "time",
                         all = TRUE) %>%
        tidyr::fill(dtmax_ed, .direction = "updown") %>%
        dplyr::select(time, dt, dtmax_ed) %>%
        return()
    }
  }


#' Calculate dTmax by the predawn method and environmental dependent method
#'
#' @description calc_dtmax_pd_ed() calculates daily maximum temperature
#'   difference between granier-type sap flow probes (dTmax) by the predawn
#'   method and environmental dependent method.
#'
#' @param vctr_time Timestamp vector
#' @param vctr_dt Delta T (temperature difference between probes) time series
#' @param vctr_radi Incident radiation time series
#' @param vctr_vpd Water vapor pressure deficit time series
#' @param thres_radi Threshold value of predawn short-wave radiation
#' @param thres_vpd Threshold value of predawn short-wave radiation
#' @param thres_cv Threshold value of coefficient of variation
#' @param thres_hour Threshold value of predawn hour
#' @param min_n_wndw Threshold value of minimum data points in moving window
#' @param err_label Error value label representing missing value
#' @param output_daily Logical. If true, return daily dTmax; else, return dTmax
#'          with the original time resolution
#'
#' @include utils.R

calc_dtmax_pd_ed <-
  function(vctr_time, vctr_dt, vctr_radi, vctr_vpd,
           thres_radi = 100, thres_vpd = 1.0, thres_cv = 0.005,
           thres_hour = 8, min_n_wndw = 3, err_label = -9999,
           output_daily = FALSE) {
    ## avoid "No visible binding for global variable" notes
    txtProgressBar <- NULL
    time <- NULL
    setTxtProgressBar <- NULL
    dt <- NULL
    time_lag <- NULL
    radi <- NULL
    . <- NULL
    time_dTmax_PD <- NULL
    sd <- NULL
    dtmax_pd <- NULL
    dtmax_ed <- NULL

    ## check input values
    interval_time <- get_interval(vctr_time)

    if(anyNA(vctr_radi) == TRUE) {
      stop("one or more NA values exist in the input radiation time series")
    }
    if(anyNA(vctr_vpd) == TRUE) {
      stop("one or more NA values exist in the input VPD time series")
    }
    if(min(vctr_vpd) < 0) {
      stop("negative VPD values are not allowed")
    }

    ## create time vector for output
    time_head_output <-
      vctr_time[1] %>%
      lubridate::ceiling_date(unit = "day") %>%
      - lubridate::minutes(interval_time)

    time_tail_output <-
      vctr_time[length(vctr_time)] %>%
      lubridate::ceiling_date(unit = "day") %>%
      - lubridate::minutes(interval_time)

    vctr_time_output <-
      seq(time_head_output, time_tail_output, by = "1 day")

    n_point <- length(vctr_time)
    n_day <- length(vctr_time_output)

    time_start <- vctr_time[1]
    wndw_head <- time_start - lubridate::days(1)
    pb <- txtProgressBar(min = 1, max = n_day, style = 3)

    list_dTmax_PD <- rep(NA, n_day)
    list_dTmax_ED <- rep(NA, n_day)

    data_all <-
      data.frame(time = vctr_time,
                 dt = vctr_dt,
                 radi = vctr_radi,
                 VPD = vctr_vpd) %>%
      dplyr::mutate(time_lag = time - lubridate::minutes(interval_time)) %>%
      dplyr::na_if(err_label)

    message("dTmax calculation by the PD and ED methods started")

    for (i in 1:n_day) {
      setTxtProgressBar(pb, i)

      wndw_head <- wndw_head + lubridate::days(1)
      wndw_tail <-
        wndw_head + lubridate::days(1) - lubridate::minutes(interval_time)

      wndw <-
        data_all %>%
        dplyr::filter(time >= wndw_head & time <= wndw_tail & !is.na(dt) &
                        lubridate::hour(time_lag) < thres_hour &
                        radi < thres_radi)

      if(nrow(wndw) < min_n_wndw) next

      ## PD dTmax calculation
      wndw %>%
        dplyr::arrange(dt) %>%
        dplyr::slice_tail() %>% {
          dplyr::pull(., dt) ->> list_dTmax_PD[i]
          dplyr::pull(., time) ->> time_dTmax_PD
        }

      ## ED dTmax calculation
      wndw_2h <-
        data_all %>%
        dplyr::filter(time >= time_dTmax_PD - lubridate::hours(2) +
                        lubridate::minutes(interval_time) &
                        time <= time_dTmax_PD &
                        !is.na(dt))

      if(nrow(wndw_2h) < min_n_wndw) next
      if(mean(wndw_2h$VPD, na.rm = TRUE) >= thres_vpd) next
      if(sd(wndw_2h$dt) / mean(wndw_2h$dt) >= thres_cv) next

      list_dTmax_ED[i] <- list_dTmax_PD[i]
    }

    message("\ndTmax calculation by the PD and ED methods finished")

    daily_dTmax <-
      data.frame(time = vctr_time_output,
                 dtmax_pd = list_dTmax_PD,
                 dtmax_ed = list_dTmax_ED) %>%
      dplyr::mutate(dtmax_pd = zoo::na.approx(dtmax_pd, na.rm = FALSE),
                    dtmax_ed = zoo::na.approx(dtmax_ed, na.rm = FALSE))

    if(output_daily) {
      return(daily_dTmax)
    } else {
      data_all %>%
        dplyr::select(time, dt) %>%
        dplyr::mutate(time_lag = time - lubridate::minutes(interval_time)) %>%
        merge.data.frame(., daily_dTmax, by.x = "time_lag", by.y = "time",
                         all = TRUE) %>%
        tidyr::fill(c(dtmax_pd, dtmax_ed), .direction = "updown") %>%
        dplyr::select(time, dt, dtmax_pd, dtmax_ed) %>%
        return()
    }
  }


#' Calculate dTmax by the moving window method
#'
#' @description calc_dtmax_mw() calculates daily maximum temperature
#'   difference between granier-type sap flow probes (dTmax) by moving window
#'   method.
#'
#' @param vctr_time_daily Daily timestamp vector
#' @param vctr_dtmax_pd Delta Tmax time series calculated by predawn method
#' @param wndw_size The number of data points in a moving window
#' @param min_n_wndw Threshold value of minimum data points in moving window
#' @param err_label Error value label representing missing value
#' @param output_daily Logical. If true, return daily dTmax; else, return dTmax
#'          with the original time resolution
#' @param vctr_time Timestamp vector
#' @param vctr_dt Delta T (temperature difference between probes) time series
#' @param vctr_radi Incident radiation time series
#' @param thres_radi Threshold value of predawn short-wave radiation
#' @param thres_hour Threshold value of predawn hour
#'
#' @examples
#' # Calculate dTmax from data
#' data(dt_gf)
#' result_pd <- calc_dtmax_pd(dt_gf$time[seq(1, 48 * 10)], dt_gf$dt[seq(1, 48 * 10)],
#'                            dt_gf$rs[seq(1, 48 * 10)])
#'
#' time_test <- result_pd$time
#' dtmax_pd_test <- result_pd$dtmax_pd
#'
#' result <- calc_dtmax_mw(time_test, dtmax_pd_test, output_daily = TRUE)
#'
#' @include utils.R
#' @export

calc_dtmax_mw <-
  function(vctr_time_daily = NULL, vctr_dtmax_pd = NULL, wndw_size = 11,
           min_n_wndw = 3, err_label = -9999, output_daily = FALSE,
           vctr_time = NULL, vctr_dt = NULL, vctr_radi = NULL, thres_radi = 100,
           thres_hour = 8) {
    ## avoid "No visible binding for global variable" notes
    . <- NULL
    txtProgressBar <- NULL
    setTxtProgressBar <- NULL
    time <- NULL
    dTmax_PD <- NULL
    dtmax_mw <- NULL
    dt <- NULL

    if(!is.null(vctr_dtmax_pd)) {
      if(anyNA(vctr_dtmax_pd) == TRUE) {
        stop("one or more NA values exist in the input dTmax time series")
      }
      if(is.null(vctr_time_daily)) {
        stop("time vector corresponding to dTmax time series must be provided")
      }
      if(length(vctr_dtmax_pd) != length(vctr_time_daily)) {
        stop("both the input dTmax and time vectors must have the same length")
      }

      data_all <-
        data.frame(time = vctr_time_daily,
                   dTmax_PD = vctr_dtmax_pd) %>%
        dplyr::mutate(rownum = rownames(.) %>% as.numeric()) %>%
        dplyr::na_if(err_label)
    } else {
      if(is.null(vctr_time)) {
        stop("input time vector must be provided to calculate dTmax")
      }
      if(is.null(vctr_dt)) {
        stop("input dT time series must be provided to calculate dTmax")
      }
      if(is.null(vctr_radi)) {
        stop("input radiation time series must be provided to calculate dTmax")
      }
      daily_dTmax_PD <-
        calc_dtmax_pd(vctr_time, vctr_dt, vctr_radi, thres_radi = thres_radi,
                      thres_hour = thres_hour, min_n_wndw = min_n_wndw,
                      err_label = err_label, output_daily = TRUE)

      data_all <-
        data.frame(time = daily_dTmax_PD$time,
                   dTmax_PD = daily_dTmax_PD$dtmax_pd) %>%
        dplyr::mutate(rownum = rownames(.) %>% as.numeric()) %>%
        dplyr::na_if(err_label)
    }

    n_day <- length(data_all$time)
    list_dTmax_MW <- rep(NA, n_day)
    time_start <- data_all$time[1]
    time_end <- data_all$time[n_day]
    pb <- txtProgressBar(min = 1, max = n_day, style = 3)

    message("dTmax calculation by the MW method started")

    for (i in 1:n_day) {
      setTxtProgressBar(pb, i)

      time_now <- data_all$time[i]
      wndw_head <- time_now - lubridate::days((wndw_size - 1)/2)
      wndw_tail <- time_now + lubridate::days((wndw_size - 1)/2)

      wndw <-
        data_all %>%
        dplyr::filter(time >= wndw_head & time <= wndw_tail & !is.na(dTmax_PD))

      if(nrow(wndw) < min_n_wndw) next

      ## estimate dTmax by moving window method
      list_dTmax_MW[i] <- wndw %>% dplyr::pull(dTmax_PD) %>% max()
    }

    message("\ndTmax calculation by the MW method finished")

    daily_dTmax <-
      data.frame(time = data_all$time,
                 dtmax_mw = list_dTmax_MW) %>%
      dplyr::mutate(dtmax_mw = zoo::na.approx(dtmax_mw, na.rm = FALSE))

    if(output_daily) {
      return(daily_dTmax)
    } else {
      if(is.null(vctr_time)) {
        stop("original time vector must be provided to output the result")
      }
      if(is.null(vctr_dt)) {
        stop("input dT time series must be provided to output the result")
      }
      interval_time <- get_interval(vctr_time)

      data.frame(time = vctr_time,
                 dt = vctr_dt) %>%
        dplyr::mutate(time_lag = time - lubridate::minutes(interval_time)) %>%
        merge.data.frame(., daily_dTmax, by.x = "time_lag", by.y = "time",
                         all = TRUE) %>%
        tidyr::fill(dtmax_mw, .direction = "updown") %>%
        dplyr::select(time, dt, dtmax_mw) %>%
        return()
    }
  }


#' Calculate dTmax by the double regression method
#'
#' @description calc_dtmax_dr() calculates daily maximum temperature
#'   difference between granier-type sap flow probes (dTmax) by moving window
#'   method and double regression method.
#'
#' @param vctr_time_daily Daily timestamp vector
#' @param vctr_dtmax_pd Delta Tmax time series calculated by predawn method
#' @param wndw_size The number of data points in a moving window
#' @param min_n_wndw Threshold value of minimum data points in moving window
#' @param err_label Error value label representing missing value
#' @param output_daily Logical. If true, return daily dTmax; else, return dTmax
#'          with the original time resolution
#' @param vctr_time Timestamp vector
#' @param vctr_dt Delta T (temperature difference between probes) time series
#' @param vctr_radi Incident radiation time series
#' @param thres_radi Threshold value of predawn short-wave radiation
#' @param thres_hour Threshold value of predawn hour
#'
#' @examples
#' # Calculate dTmax from data
#' data(dt_gf)
#' result_pd <- calc_dtmax_pd(dt_gf$time[seq(1, 48 * 10)],
#'                            dt_gf$dt[seq(1, 48 * 10)],
#'                            dt_gf$rs[seq(1, 48 * 10)])
#'
#' time_test <- result_pd$time
#' dtmax_pd_test <- result_pd$dtmax_pd
#'
#' result <- calc_dtmax_dr(time_test, dtmax_pd_test, output_daily = TRUE)
#'
#' @include utils.R
#' @export

calc_dtmax_dr <-
  function(vctr_time_daily = NULL, vctr_dtmax_pd = NULL, wndw_size = 11,
           min_n_wndw = 3, err_label = -9999, output_daily = FALSE,
           vctr_time = NULL, vctr_dt = NULL, vctr_radi = NULL, thres_radi = 100,
           thres_hour = 8) {
    ## avoid "No visible binding for global variable" notes
    . <- NULL
    txtProgressBar <- NULL
    setTxtProgressBar <- NULL
    time <- NULL
    dTmax_PD <- NULL
    dtmax_dr <- NULL
    dt <- NULL

    if(!is.null(vctr_dtmax_pd)) {
      if(anyNA(vctr_dtmax_pd) == TRUE) {
        stop("one or more NA values exist in the input dTmax time series")
      }
      if(is.null(vctr_time_daily)) {
        stop("time vector corresponding to dTmax time series must be provided")
      }
      if(length(vctr_dtmax_pd) != length(vctr_time_daily)) {
        stop("both the input dTmax and time vectors must have the same length")
      }

      data_all <-
        data.frame(time = vctr_time_daily,
                   dTmax_PD = vctr_dtmax_pd) %>%
        dplyr::mutate(rownum = rownames(.) %>% as.numeric()) %>%
        dplyr::na_if(err_label)
    } else {
      if(is.null(vctr_time)) {
        stop("input time vector must be provided to calculate dTmax")
      }
      if(is.null(vctr_dt)) {
        stop("input dT time series must be provided to calculate dTmax")
      }
      if(is.null(vctr_radi)) {
        stop("input radiation time series must be provided to calculate dTmax")
      }
      daily_dTmax_PD <-
        calc_dtmax_pd(vctr_time, vctr_dt, vctr_radi, thres_radi = thres_radi,
                      thres_hour = thres_hour, min_n_wndw = min_n_wndw,
                      err_label = err_label, output_daily = TRUE)

      data_all <-
        data.frame(time = daily_dTmax_PD$time,
                   dTmax_PD = daily_dTmax_PD$dtmax_pd) %>%
        dplyr::mutate(rownum = rownames(.) %>% as.numeric()) %>%
        dplyr::na_if(err_label)
    }

    n_day <- length(data_all$time)
    list_dTmax_DR <- rep(NA, n_day)
    time_start <- data_all$time[1]
    time_end <- data_all$time[n_day]
    pb <- txtProgressBar(min = 1, max = n_day, style = 3)

    message("dTmax calculation by the DR method started")

    for (i in 1:n_day) {
      setTxtProgressBar(pb, i)

      time_now <- data_all$time[i]
      wndw_head <- time_now - lubridate::days((wndw_size - 1)/2)
      wndw_tail <- time_now + lubridate::days((wndw_size - 1)/2)

      wndw <-
        data_all %>%
        dplyr::filter(time >= wndw_head & time <= wndw_tail & !is.na(dTmax_PD))

      if(nrow(wndw) < min_n_wndw) next

      ## estimate dTmax by double regression method
      ave_wndw <- wndw %>% dplyr::pull(dTmax_PD) %>% mean()
      list_dTmax_DR[i] <-
        wndw %>%
        dplyr::filter(dTmax_PD >= ave_wndw) %>%
        dplyr::pull(dTmax_PD) %>%
        mean()
    }

    message("\ndTmax calculation by the DR method finished")

    daily_dTmax <-
      data.frame(time = data_all$time,
                 dtmax_dr = list_dTmax_DR) %>%
      dplyr::mutate(dtmax_dr = zoo::na.approx(dtmax_dr, na.rm = FALSE))

    if(output_daily) {
      return(daily_dTmax)
    } else {
      if(is.null(vctr_time)) {
        stop("original time vector must be provided to output the result")
      }
      if(is.null(vctr_dt)) {
        stop("input dT time series must be provided to output the result")
      }
      interval_time <- get_interval(vctr_time)

      data.frame(time = vctr_time,
                 dt = vctr_dt) %>%
        dplyr::mutate(time_lag = time - lubridate::minutes(interval_time)) %>%
        merge.data.frame(., daily_dTmax, by.x = "time_lag", by.y = "time",
                         all = TRUE) %>%
        tidyr::fill(dtmax_dr, .direction = "updown") %>%
        dplyr::select(time, dt, dtmax_dr) %>%
        return()
    }
  }


#' Calculate dTmax by the moving window method and double regression method
#'
#' @description calc_dtmax_mw_dr() calculates daily maximum temperature
#'   difference between granier-type sap flow probes (dTmax) by moving window
#'   method and double regression method.
#'
#' @param vctr_time_daily Daily timestamp vector
#' @param vctr_dtmax_pd Delta Tmax time series calculated by predawn method
#' @param wndw_size The number of data points in a moving window
#' @param min_n_wndw Threshold value of minimum data points in moving window
#' @param err_label Error value label representing missing value
#' @param output_daily Logical. If true, return daily dTmax; else, return dTmax
#'          with the original time resolution
#' @param vctr_time Timestamp vector
#' @param vctr_dt Delta T (temperature difference between probes) time series
#' @param vctr_radi Incident radiation time series
#' @param thres_radi Threshold value of predawn short-wave radiation
#' @param thres_hour Threshold value of predawn hour
#'
#' @include utils.R

calc_dtmax_mw_dr <-
  function(vctr_time_daily = NULL, vctr_dtmax_pd = NULL, wndw_size = 11,
           min_n_wndw = 3, err_label = -9999, output_daily = FALSE,
           vctr_time = NULL, vctr_dt = NULL, vctr_radi = NULL, thres_radi = 100,
           thres_hour = 8) {
    ## avoid "No visible binding for global variable" notes
    . <- NULL
    txtProgressBar <- NULL
    setTxtProgressBar <- NULL
    time <- NULL
    dTmax_PD <- NULL
    dtmax_mw <- NULL
    dtmax_dr <- NULL
    dt <- NULL

    if(!is.null(vctr_dtmax_pd)) {
      if(anyNA(vctr_dtmax_pd) == TRUE) {
        stop("one or more NA values exist in the input dTmax time series")
      }
      if(is.null(vctr_time_daily)) {
        stop("time vector corresponding to dTmax time series must be provided")
      }
      if(length(vctr_dtmax_pd) != length(vctr_time_daily)) {
        stop("both the input dTmax and time vectors must have the same length")
      }

      data_all <-
        data.frame(time = vctr_time_daily,
                   dTmax_PD = vctr_dtmax_pd) %>%
        dplyr::mutate(rownum = rownames(.) %>% as.numeric()) %>%
        dplyr::na_if(err_label)
    } else {
      if(is.null(vctr_time)) {
        stop("input time vector must be provided to calculate dTmax")
      }
      if(is.null(vctr_dt)) {
        stop("input dT time series must be provided to calculate dTmax")
      }
      if(is.null(vctr_radi)) {
        stop("input radiation time series must be provided to calculate dTmax")
      }
      daily_dTmax_PD <-
        calc_dtmax_pd(vctr_time, vctr_dt, vctr_radi, thres_radi = thres_radi,
                      thres_hour = thres_hour, min_n_wndw = min_n_wndw,
                      err_label = err_label, output_daily = TRUE)

      data_all <-
        data.frame(time = daily_dTmax_PD$time,
                   dTmax_PD = daily_dTmax_PD$dtmax_pd) %>%
        dplyr::mutate(rownum = rownames(.) %>% as.numeric()) %>%
        dplyr::na_if(err_label)
    }

    n_day <- length(data_all$time)
    list_dTmax_MW <- rep(NA, n_day)
    list_dTmax_DR <- rep(NA, n_day)
    time_start <- data_all$time[1]
    time_end <- data_all$time[n_day]
    pb <- txtProgressBar(min = 1, max = n_day, style = 3)

    message("dTmax calculation by the MW and DR methods started")

    for (i in 1:n_day) {
      setTxtProgressBar(pb, i)

      time_now <- data_all$time[i]
      wndw_head <- time_now - lubridate::days((wndw_size - 1)/2)
      wndw_tail <- time_now + lubridate::days((wndw_size - 1)/2)

      wndw <-
        data_all %>%
        dplyr::filter(time >= wndw_head & time <= wndw_tail & !is.na(dTmax_PD))

      if(nrow(wndw) < min_n_wndw) next

      ## estimate dTmax by moving window method
      list_dTmax_MW[i] <- wndw %>% dplyr::pull(dTmax_PD) %>% max()

      ## estimate dTmax by double regression method
      ave_wndw <- wndw %>% dplyr::pull(dTmax_PD) %>% mean()
      list_dTmax_DR[i] <-
        wndw %>%
        dplyr::filter(dTmax_PD >= ave_wndw) %>%
        dplyr::pull(dTmax_PD) %>%
        mean()
    }

    message("\ndTmax calculation by the MW and DR methods finished")

    daily_dTmax <-
      data.frame(time = data_all$time,
                 dtmax_mw = list_dTmax_MW,
                 dtmax_dr = list_dTmax_DR) %>%
      dplyr::mutate(dtmax_mw = zoo::na.approx(dtmax_mw, na.rm = FALSE),
                    dtmax_dr = zoo::na.approx(dtmax_dr, na.rm = FALSE))

    if(output_daily) {
      return(daily_dTmax)
    } else {
      if(is.null(vctr_time)) {
        stop("original time vector must be provided to output the result")
      }
      if(is.null(vctr_dt)) {
        stop("input dT time series must be provided to output the result")
      }
      interval_time <- get_interval(vctr_time)

      data.frame(time = vctr_time,
                 dt = vctr_dt) %>%
        dplyr::mutate(time_lag = time - lubridate::minutes(interval_time)) %>%
        merge.data.frame(., daily_dTmax, by.x = "time_lag", by.y = "time",
                         all = TRUE) %>%
        tidyr::fill(c(dtmax_mw, dtmax_dr), .direction = "updown") %>%
        dplyr::select(time, dt, dtmax_mw, dtmax_dr) %>%
        return()
    }
  }


#' Calculate dTmax by five different methods
#'
#' @description calc_dtmax_all() calculates daily maximum temperature
#'   difference between granier-type sap flow probes (dTmax) by the predawn
#'   method, successive predawn method, moving window method,
#'   double regression method, and environmental dependent method.
#'
#' @param vctr_time Timestamp vector
#' @param vctr_dt Delta T time series
#' @param vctr_radi Incident short-wave radiation time series
#' @param vctr_vpd Vapor pressure deficit time series
#' @param method Method name list for dTmax estimation
#' @param thres_hour_sp to be filled!
#' @param thres_radi to be filled!
#' @param thres_vpd to be filled!
#' @param thres_cv to be filled!
#' @param thres_hour_pd to be filled!
#' @param min_n_wndw to be filled!
#' @param err_label to be filled!
#' @param wndw_size to be filled!
#' @param output_daily to be filled!
#'
#' @examples
#' data(dt_gf)
#' time_test <- dt_gf$time[seq(1, 48 * 10)]
#' dt_test <- dt_gf$dt[seq(1, 48 * 10)]
#' rs_test <- dt_gf$rs[seq(1, 48 * 10)]
#' vpd_test <- dt_gf$vpd[seq(1, 48 * 10)]
#'
#' # Calculate dTmax from data
#' result <- calc_dtmax_all(time_test, dt_test, rs_test, vpd_test)
#'
#' @include utils.R
#' @export

calc_dtmax_all <-
  function(vctr_time, vctr_dt, vctr_radi = NULL, vctr_vpd = NULL,
           method = c("sp", "pd", "mw", "dr", "ed"),
           thres_hour_sp = 5,
           thres_radi = 100, thres_vpd = 1.0, thres_cv = 0.005,
           thres_hour_pd = 8, min_n_wndw = 3, err_label = -9999,
           wndw_size = 11,
           output_daily = FALSE) {
    ## avoid "No visible binding for global variable" notes
    time_lag <- NULL
    time <- NULL
    dt <- NULL

    ## check input values
    interval_time <- get_interval(vctr_time)
    timezone <- lubridate::tz(vctr_time[1])

    if(length(vctr_time) != length(vctr_dt)) {
      stop("input timestamp and dT time series must be the same length")
    }

    if(!is.null(vctr_radi)) {
      if(length(vctr_time) != length(vctr_radi)) {
        stop("input timestamp and radiation time series must be the same length")
      }

      if(anyNA(vctr_radi) == TRUE) {
        stop("one or more NA values exist in the input radiation time series")
      }
    }

    if(!is.null(vctr_vpd)) {
      if(length(vctr_time) != length(vctr_vpd)) {
        stop("input timestamp and VPD time series must be the same length")
      }

      if(anyNA(vctr_vpd) == TRUE) {
        stop("one or more NA values exist in the input VPD time series")
      }

      if(min(vctr_vpd) < 0) {
        stop("negative VPD values are not allowed")
      }
    }

    ## select methods
    do_sp = ifelse(length(method[method %in% "sp"]) > 0, TRUE, FALSE)
    do_pd = ifelse(length(method[method %in% "pd"]) > 0, TRUE, FALSE)
    do_mw = ifelse(length(method[method %in% "mw"]) > 0, TRUE, FALSE)
    do_dr = ifelse(length(method[method %in% "dr"]) > 0, TRUE, FALSE)
    do_ed = ifelse(length(method[method %in% "ed"]) > 0, TRUE, FALSE)

    ## dTmax calculation by the SP method
    if(do_sp) {
      if(output_daily) {
        daily_dTmax_SP <-
          calc_dtmax_sp(vctr_time, vctr_dt, thres_hour = thres_hour_sp,
                        output_daily = TRUE)
      } else {
        dTmax_SP <-
          calc_dtmax_sp(vctr_time, vctr_dt, thres_hour = thres_hour_sp,
                        output_daily = FALSE)
      }
    }

    ## dTmax calculation by the PD and/or ED methods
    if(do_pd & do_ed) {
      daily_dTmax_PD_ED <-
        calc_dtmax_pd_ed(vctr_time, vctr_dt, vctr_radi, vctr_vpd,
                         thres_radi = thres_radi, thres_vpd = thres_vpd,
                         thres_cv = thres_cv, thres_hour = thres_hour_pd,
                         min_n_wndw = min_n_wndw, err_label = err_label,
                         output_daily = TRUE)
    } else if(do_pd) {
      daily_dTmax_PD_ED <-
        calc_dtmax_pd(vctr_time, vctr_dt, vctr_radi,
                      thres_radi = thres_radi, thres_hour = thres_hour_pd,
                      min_n_wndw = min_n_wndw, err_label = err_label,
                      output_daily = TRUE)
    } else if(do_ed) {
      daily_dTmax_PD_ED <-
        calc_dtmax_ed(vctr_time, vctr_dt, vctr_radi, vctr_vpd,
                      thres_radi = thres_radi, thres_vpd = thres_vpd,
                      thres_cv = thres_cv, thres_hour = thres_hour_pd,
                      min_n_wndw = min_n_wndw, err_label = err_label,
                      output_daily = TRUE)
    }

    ## dTmax calculation by the MW and/or DR methods
    if(do_pd) {
      if(do_mw & do_dr) {
        daily_dTmax_MW_DR <-
          calc_dtmax_mw_dr(daily_dTmax_PD_ED$time, daily_dTmax_PD_ED$dtmax_pd,
                           wndw_size = wndw_size, min_n_wndw = min_n_wndw,
                           err_label = err_label, output_daily = TRUE)
      } else if(do_mw) {
        daily_dTmax_MW_DR <-
          calc_dtmax_mw(daily_dTmax_PD_ED$time, daily_dTmax_PD_ED$dtmax_pd,
                        wndw_size = wndw_size, min_n_wndw = min_n_wndw,
                        err_label = err_label, output_daily = TRUE)
      } else if(do_dr) {
        daily_dTmax_MW_DR <-
          calc_dtmax_dr(daily_dTmax_PD_ED$time, daily_dTmax_PD_ED$dtmax_pd,
                        wndw_size = wndw_size, min_n_wndw = min_n_wndw,
                        err_label = err_label, output_daily = TRUE)
      }
    } else if(do_mw & do_dr) {
      daily_dTmax_MW_DR <-
        calc_dtmax_mw_dr(vctr_time = vctr_time, vctr_dt = vctr_dt,
                         vctr_radi = vctr_radi, thres_radi = thres_radi,
                         thres_hour = thres_hour_pd, wndw_size = wndw_size,
                         min_n_wndw = min_n_wndw, err_label = err_label,
                         output_daily = TRUE)
    } else if(do_mw) {
      daily_dTmax_MW_DR <-
        calc_dtmax_mw(vctr_time = vctr_time, vctr_dt = vctr_dt,
                      vctr_radi = vctr_radi, thres_radi = thres_radi,
                      thres_hour = thres_hour_pd, wndw_size = wndw_size,
                      min_n_wndw = min_n_wndw, err_label = err_label,
                      output_daily = TRUE)
    } else if(do_dr) {
      daily_dTmax_MW_DR <-
        calc_dtmax_dr(vctr_time = vctr_time, vctr_dt = vctr_dt,
                      vctr_radi = vctr_radi, thres_radi = thres_radi,
                      thres_hour = thres_hour_pd, wndw_size = wndw_size,
                      min_n_wndw = min_n_wndw, err_label = err_label,
                      output_daily = TRUE)
    }

    ## aggregate calculated values
    message("dTmax time series aggregation started")

    daily_dTmax <- NULL

    if(do_pd) {
      daily_dTmax <-
        data.frame(time_lag = daily_dTmax_PD_ED$time,
                   dtmax_pd = daily_dTmax_PD_ED$dtmax_pd)
    }

    if(do_mw) {
      if(!do_pd) {
        daily_dTmax <-
          data.frame(time_lag = daily_dTmax_MW_DR$time,
                     dtmax_mw = daily_dTmax_MW_DR$dtmax_mw)
      } else {
        daily_dTmax$dtmax_mw <- daily_dTmax_MW_DR$dtmax_mw
      }
    }

    if(do_dr) {
      if(!do_pd & !do_mw) {
        daily_dTmax <-
          data.frame(time_lag = daily_dTmax_MW_DR$time,
                     dtmax_dr = daily_dTmax_MW_DR$dtmax_dr)
      } else {
        daily_dTmax$dtmax_dr <- daily_dTmax_MW_DR$dtmax_dr
      }
    }

    if(do_ed) {
      if(!do_pd & !do_mw & !do_dr) {
        daily_dTmax <-
          data.frame(time_lag = daily_dTmax_PD_ED$time,
                     dtmax_ed = daily_dTmax_PD_ED$dtmax_ed)
      } else {
        daily_dTmax$dtmax_ed <- daily_dTmax_PD_ED$dtmax_ed
      }
    }

    if(output_daily) {
      if(is.null(daily_dTmax)) {
        daily_dTmax <- daily_dTmax_SP
      } else if(do_sp) {
        daily_dTmax <-
          daily_dTmax %>%
          dplyr::mutate(dtmax_sp = daily_dTmax_SP$dtmax_sp) %>%
          dplyr::rename(time = time_lag)
      }

      daily_dTmax <-
        daily_dTmax %>%
        tidyr::fill(c(dplyr::contains("dtmax")), .direction = "updown")

      message("dTmax time series aggregation finished")

      return(daily_dTmax)
    } else {
      ## avoid NA row addition to the end of output data frame
      vctr_time_temp <-
        c(vctr_time,
          vctr_time[length(vctr_time)] + lubridate::minutes(interval_time))

      vctr_dT_temp <- c(vctr_dt, NA)

      dTmax <-
        data.frame(time = vctr_time_temp,
                   dt = vctr_dT_temp) %>%
        dplyr::mutate(time_lag = time - lubridate::minutes(interval_time))

      if(do_sp) dTmax$dtmax_sp <- c(dTmax_SP$dtmax_sp, NA)

      if(!is.null(daily_dTmax)) {
        dTmax <-
          merge.data.frame(dTmax, daily_dTmax, by = "time_lag", all = TRUE) %>%
          tidyr::fill(c(dplyr::contains("dtmax")), .direction = "updown") %>%
          dplyr::select(time, dt, dplyr::contains("dtmax"))
      }

      dTmax <-
        dTmax %>%
        dplyr::filter(time <= vctr_time[length(vctr_time)])

      message("dTmax time series aggregation finished")

      return(dTmax)
    }
  }



## test
# library(dplyr)
# library(lubridate)
# library(magrittr)
# library(zoo)
# library(xts)
# library(tidyr)
#
# data("dt_gf")
# data("dtmax")
# dTmax_SP_ref = dtmax$dtmax_sp
# dTmax_PD_ref = dtmax$dtmax_pd
# dTmax_ED_ref = dtmax$dtmax_ed
# dTmax_MW_ref = dtmax$dtmax_mw
# dTmax_DR_ref = dtmax$dtmax_dr
# dTmax_SP_ref <-
#   dtmax %>%
#   filter(hour(time) == 23 & minute(time) == 30) %>%
#   pull(dtmax_sp)
# dTmax_PD_ref <-
#   dtmax %>%
#   filter(hour(time) == 23 & minute(time) == 30) %>%
#   pull(dtmax_pd)
# dTmax_MW_ref <-
#   dtmax %>%
#   filter(hour(time) == 23 & minute(time) == 30) %>%
#   pull(dtmax_mw)
# dTmax_DR_ref <-
#   dtmax %>%
#   filter(hour(time) == 23 & minute(time) == 30) %>%
#   pull(dtmax_dr)
# dTmax_ED_ref <-
#   dtmax %>%
#   filter(hour(time) == 23 & minute(time) == 30) %>%
#   pull(dtmax_ed)
#
# time_test <- dt_gf$time
# dt_test <- dt_gf$dt
# rs_test <- dt_gf$rs
# vpd_test <- dt_gf$vpd
#
# rslt_test <-
#   calc_dtmax_all(time_test, dt_test, rs_test, vpd_test,
#                  method = c("sp", "pd", "mw", "dr", "ed"),
#                  thres_vpd = 6.0, output_daily = TRUE)
#
# rslt_test <-
#   calc_dtmax_all(time_test, dt_test, rs_test, vpd_test,
#                  method = c("pd", "mw", "dr", "ed"),
#                  thres_vpd = 6.0, output_daily = TRUE)

# rslt_test <-
#   calc_dtmax_all(time_test, dt_test, rs_test, vpd_test,
#                  method = c("sp", "mw", "dr", "ed"),
#                  thres_vpd = 6.0, output_daily = TRUE)
#
# rslt_test <-
#   calc_dtmax_all(time_test, dt_test, rs_test, vpd_test,
#                  method = c("sp", "pd", "dr", "ed"),
#                  thres_vpd = 6.0, output_daily = TRUE)
#
# rslt_test <-
#   calc_dtmax_all(time_test, dt_test, rs_test, vpd_test,
#                  method = c("sp", "pd", "mw", "ed"),
#                  thres_vpd = 6.0, output_daily = TRUE)
#
# rslt_test <-
#   calc_dtmax_all(time_test, dt_test, rs_test, vpd_test,
#                  method = c("sp", "pd", "mw", "dr"),
#                  thres_vpd = 6.0, output_daily = TRUE)
#
# rslt_test <-
#   calc_dtmax_all(time_test, dt_test, rs_test, vpd_test,
#                  method = c("mw", "dr", "ed"),
#                  thres_vpd = 6.0, output_daily = TRUE)
#
# rslt_test <-
#   calc_dtmax_all(time_test, dt_test, rs_test, vpd_test,
#                  method = c("pd", "dr", "ed"),
#                  thres_vpd = 6.0, output_daily = TRUE)
#
# rslt_test <-
#   calc_dtmax_all(time_test, dt_test, rs_test, vpd_test,
#                  method = c("pd", "mw", "ed"),
#                  thres_vpd = 6.0, output_daily = TRUE)
#
# rslt_test <-
#   calc_dtmax_all(time_test, dt_test, rs_test, vpd_test,
#                  method = c("pd", "mw", "dr"),
#                  thres_vpd = 6.0, output_daily = TRUE)
#
# rslt_test <-
#   calc_dtmax_all(time_test, dt_test, rs_test, vpd_test,
#                  method = c("sp", "dr", "ed"),
#                  thres_vpd = 6.0, output_daily = TRUE)
#
# rslt_test <-
#   calc_dtmax_all(time_test, dt_test, rs_test, vpd_test,
#                  method = c("sp", "mw", "ed"),
#                  thres_vpd = 6.0, output_daily = TRUE)
#
# rslt_test <-
#   calc_dtmax_all(time_test, dt_test, rs_test, vpd_test,
#                  method = c("sp", "mw", "dr"),
#                  thres_vpd = 6.0, output_daily = TRUE)
#
# rslt_test <-
#   calc_dtmax_all(time_test, dt_test, rs_test, vpd_test,
#                  method = c("sp", "pd", "ed"),
#                  thres_vpd = 6.0, output_daily = TRUE)
#
# rslt_test <-
#   calc_dtmax_all(time_test, dt_test, rs_test, vpd_test,
#                  method = c("sp", "pd", "dr"),
#                  thres_vpd = 6.0, output_daily = TRUE)
#
# rslt_test <-
#   calc_dtmax_all(time_test, dt_test, rs_test, vpd_test,
#                  method = c("sp", "pd", "mw"),
#                  thres_vpd = 6.0, output_daily = TRUE)
#
# rslt_test <-
#   calc_dtmax_all(time_test, dt_test, rs_test, vpd_test,
#                  method = c("dr", "ed"),
#                  thres_vpd = 6.0, output_daily = TRUE)
#
# rslt_test <-
#   calc_dtmax_all(time_test, dt_test, rs_test, vpd_test,
#                  method = c("mw", "ed"),
#                  thres_vpd = 6.0, output_daily = TRUE)
#
# rslt_test <-
#   calc_dtmax_all(time_test, dt_test, rs_test, vpd_test,
#                  method = c("mw", "dr"),
#                  thres_vpd = 6.0, output_daily = TRUE)
#
# rslt_test <-
#   calc_dtmax_all(time_test, dt_test, rs_test, vpd_test,
#                  method = c("pd", "ed"),
#                  thres_vpd = 6.0, output_daily = TRUE)
#
# rslt_test <-
#   calc_dtmax_all(time_test, dt_test, rs_test, vpd_test,
#                  method = c("pd", "dr"),
#                  thres_vpd = 6.0, output_daily = TRUE)
#
# rslt_test <-
#   calc_dtmax_all(time_test, dt_test, rs_test, vpd_test,
#                  method = c("pd", "mw"),
#                  thres_vpd = 6.0, output_daily = TRUE)
#
# rslt_test <-
#   calc_dtmax_all(time_test, dt_test, rs_test, vpd_test,
#                  method = c("sp", "ed"),
#                  thres_vpd = 6.0, output_daily = TRUE)
#
# rslt_test <-
#   calc_dtmax_all(time_test, dt_test, rs_test, vpd_test,
#                  method = c("sp", "dr"),
#                  thres_vpd = 6.0, output_daily = TRUE)
#
# rslt_test <-
#   calc_dtmax_all(time_test, dt_test, rs_test, vpd_test,
#                  method = c("sp", "mw"),
#                  thres_vpd = 6.0, output_daily = TRUE)
#
# rslt_test <-
#   calc_dtmax_all(time_test, dt_test, rs_test, vpd_test,
#                  method = c("sp", "pd"),
#                  thres_vpd = 6.0, output_daily = TRUE)
#
# rslt_test <-
#   calc_dtmax_all(time_test, dt_test, rs_test, vpd_test,
#                  method = c("ed"),
#                  thres_vpd = 6.0, output_daily = TRUE)
#
# rslt_test <-
#   calc_dtmax_all(time_test, dt_test, rs_test, vpd_test,
#                  method = c("dr"),
#                  thres_vpd = 6.0, output_daily = TRUE)
#
# rslt_test <-
#   calc_dtmax_all(time_test, dt_test, rs_test, vpd_test,
#                  method = c("mw"),
#                  thres_vpd = 6.0, output_daily = TRUE)
#
# rslt_test <-
#   calc_dtmax_all(time_test, dt_test, rs_test, vpd_test,
#                  method = c("pd"),
#                  thres_vpd = 6.0, output_daily = TRUE)
#
# rslt_test <-
#   calc_dtmax_all(time_test, dt_test, rs_test, vpd_test,
#                  method = c("sp"),
#                  thres_vpd = 6.0, output_daily = TRUE)
#
# summary(rslt_test)
# summary(dTmax_SP_ref - rslt_test$dtmax_sp)
# summary(dTmax_PD_ref - rslt_test$dtmax_pd)
# summary(dTmax_MW_ref - rslt_test$dtmax_mw)
# summary(dTmax_DR_ref - rslt_test$dtmax_dr)
# summary(dTmax_ED_ref - rslt_test$dtmax_ed)
#
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

