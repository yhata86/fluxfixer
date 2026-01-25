#' Calculate dTmax by the successive predawn method
#'
#' @description `calc_dtmax_sp()` calculates the time series of dTmax (the
#'  maximum temperature difference between sap flow probes under zero-flow
#'  conditions) using the successive predawn method.
#'
#' @details The successive predawn method is one of the methods for determining
#'  the maximum temperature difference between sap flow probes under zero-flow
#'  conditions. This method defines the dTmax for a day as the maximum dT (the
#'  temperature difference between sap flow probes) within a 24-hour period
#'  that begins at 5:00 a.m. (default; just before daybreak in temperate
#'  zones). In other words, the day starts at predawn, not midnight, and the
#'  maximum value for that period is assumed to be dTmax. This method has the
#'  advantage of being able to calculate dTmax quickly while minimizing the
#'  effect of nocturnal transpiration on dTmax estimation.
#'
#' @param vctr_time A timestamp vector of class POSIXct or POSIXt. This vector
#'  indicates the timings of the end of each measurement in local time. Any
#'  interval (typically 15 to 60 min) is allowed, but the timestamps must be
#'  equally spaced and arranged chronologically.
#' @param vctr_dt A vector of dT (the temperature difference between sap flow
#'  probes, in degrees Celsius) time series. The length of the vector must
#'  match that of the timestamp vector. Missing values must be gap-filled
#'  previously.
#' @param thres_hour_sp An integer from 0 to 23. The threshold hour of the day
#'  which defines the start of predawn in local time (default is 5).
#' @param output_daily A boolean. If `TRUE`, returns dTmax time series in daily
#'  steps; else, returns dTmax in the original time steps. Default is `FALSE`.
#'
#' @returns
#' A data frame with columns below:
#'
#' * The first column, `time`, gives the timestamp of the measurements. If
#'  `output_daily` is `FALSE` (default), this column is the same as the input
#'  timestamp, `vctr_time`. If `output_daily` is `TRUE`, the timestamp in daily
#'  steps is returned.
#'
#' * The second column, `dt`, gives the input dT (the temperature difference
#'  between sap flow probes, degrees Celsius) time series. If `output_daily` is
#'  `TRUE`, dT is returned in daily steps. If `output_daily` is `FALSE`
#'  (default), this column is not output.
#'
#' * The third column, `dtmax_sp`, gives the estimated dTmax by the successive
#'  predawn method. If `output_daily` is `FALSE` (default), this column has
#'  the same time step as the input timestamp. If `output_daily` is `TRUE`,
#'  the dTmax is returned in daily steps.
#'
#' @author Yoshiaki Hata
#'
#' @seealso `calc_dtmax`, `calc_dtmax_pd`, `calc_dtmax_mw`, `calc_dtmax_dr`,
#'  `calc_dtmax_ed`
#'
#' @include utils.R

calc_dtmax_sp <-
  function(vctr_time, vctr_dt, thres_hour_sp = 5, output_daily = FALSE) {
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

    ## create time vector for daily output
    time_head_output <-
      vctr_time[1] %>%
      - lubridate::minutes(interval_time) %>%
      lubridate::floor_date(unit = "day")

    time_tail_output <-
      vctr_time[length(vctr_time)] %>%
      - lubridate::minutes(interval_time) %>%
      lubridate::floor_date(unit = "day")

    vctr_time_output <-
      seq(time_head_output, time_tail_output, by = "1 day")


    ## SP dTmax calculation
    message("--- dTmax calculation by the SP method started")

    daily_dTmax <-
      data_all %>%
      dplyr::transmute(time_shifted = time - lubridate::hours(thres_hour_sp) -
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
      dplyr::mutate(dtmax_sp = zoo::na.approx(dtmax_sp, na.rm = FALSE)) %>%
      tidyr::fill(dtmax_sp, .direction = "updown")

    message("--- dTmax calculation by the SP method finished")

    ## output
    if(output_daily) {
      daily_dTmax %>%
        dplyr::mutate(time = vctr_time_output) %>%
      return()
    } else {
      data_all %>%
        dplyr::select(time, dt) %>%
        dplyr::mutate(time_shifted = time - lubridate::hours(thres_hour_sp) -
                        lubridate::minutes(interval_time)) %>%
        merge.data.frame(., daily_dTmax, by.x = "time_shifted", by.y = "time",
                         all = TRUE) %>%
        tidyr::fill(dtmax_sp, .direction = "updown") %>%
        dplyr::select(time, dt, dtmax_sp) %>%
        return()
    }
  }


#' Calculate dTmax by the daily predawn method
#'
#' @description `calc_dtmax_pd()` calculates the time series of dTmax (the
#'  maximum temperature difference between sap flow probes under zero-flow
#'  conditions) using the daily predawn method.
#'
#' @details The daily predawn method is one of the methods for determining
#'  dTmax. This method defines the dTmax for a day as the maximum dT (the
#'  temperature difference between sap flow probes) between midnight and the
#'  morning (8:00 a.m. in local time) when the global solar radiation is below
#'  the threshold value (100 W m-2). See more details in Peters et al. (2018;
#'  New Phytologist).
#'
#' @inheritParams calc_dtmax_sp
#' @param vctr_radi A vector of global solar radiation or a similar radiative
#'  variable time series. The length of the vector must match that of the
#'  timestamp vector. Missing values must be gap-filled previously. The unit of
#'  the time series must match that of `thres_radi`.
#' @param thres_radi A threshold value of the radiation to define daytime.
#'  Default is 100 (W m-2). The data points with radiation values above the
#'  threshold are considered daytime values. The unit of the threshold must
#'  match that of the input radiation time series.
#' @param thres_hour_pd An integer from 0 to 23. The threshold hour of the day
#'  which defines the end of predawn in local time (default is 8).
#' @param min_n_wndw_dtmax A positive integer indicating the minimum number of
#'  data points for calculating statistics using a moving window (default is
#'  3). If the number of data points is less than this threshold, the
#'  statistics are not calculated in the window.
#'
#' @returns
#' A data frame with columns below:
#'
#' * The first column, `time`, gives the timestamp of the measurements. If
#'  `output_daily` is `FALSE` (default), this column is the same as the input
#'  timestamp, `vctr_time`. If `output_daily` is `TRUE`, the timestamp in daily
#'  steps is returned.
#'
#' * The second column, `dt`, gives the input dT (the temperature difference
#'  between sap flow probes, degrees Celsius) time series. If `output_daily` is
#'  `TRUE`, dT is returned in daily steps. If `output_daily` is `FALSE`
#'  (default), this column is not output.
#'
#' * The third column, `dtmax_pd`, gives the estimated dTmax by the daily
#'  predawn method. If `output_daily` is `FALSE` (default), this column has
#'  the same time step as the input timestamp. If `output_daily` is `TRUE`,
#'  the dTmax is returned in daily steps.
#'
#' @author Yoshiaki Hata
#'
#' @seealso `calc_dtmax`, `calc_dtmax_sp`, `calc_dtmax_mw`, `calc_dtmax_dr`,
#'  `calc_dtmax_ed`
#'
#' @include utils.R

calc_dtmax_pd <-
  function(vctr_time, vctr_dt, vctr_radi, thres_radi = 100, thres_hour_pd = 8,
           min_n_wndw_dtmax = 3, output_daily = FALSE) {
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

    if(anyNA(vctr_radi) == TRUE) {
      stop("One or more NA values exist in the input radiation time series")
    }

    if(is.null(vctr_radi) == TRUE) {
      stop("Input radiation time series must be provided to calculate dTmax")
    }

    ## create time vector for daily output
    time_head_output <-
      vctr_time[1] %>%
      - lubridate::minutes(interval_time) %>%
      lubridate::floor_date(unit = "day")

    time_tail_output <-
      vctr_time[length(vctr_time)] %>%
      - lubridate::minutes(interval_time) %>%
      lubridate::floor_date(unit = "day")

    vctr_time_output <-
      seq(time_head_output, time_tail_output, by = "1 day")

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
      dplyr::mutate(time_lag = time - lubridate::minutes(interval_time))

    message("--- dTmax calculation by the PD method started")

    for (i in 1:n_day) {
      setTxtProgressBar(pb, i)

      wndw_head <- wndw_head + lubridate::days(1)
      wndw_tail <-
        wndw_head + lubridate::days(1) - lubridate::minutes(interval_time)

      wndw <-
        data_all %>%
        dplyr::filter(time >= wndw_head & time <= wndw_tail & !is.na(dt) &
                        lubridate::hour(time_lag) < thres_hour_pd &
                        radi < thres_radi)

      if(nrow(wndw) < min_n_wndw_dtmax) next

      ## PD dTmax calculation
      wndw %>%
        dplyr::arrange(dt) %>%
        dplyr::slice_tail() %>% {
          dplyr::pull(., dt) ->> list_dTmax_PD[i]
        }
    }

    message("\n--- dTmax calculation by the PD method finished")

    daily_dTmax <-
      data.frame(time = vctr_time_output,
                 dtmax_pd = list_dTmax_PD) %>%
      dplyr::mutate(dtmax_pd = zoo::na.approx(dtmax_pd, na.rm = FALSE)) %>%
      tidyr::fill(dtmax_pd, .direction = "updown")

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
#' @description `calc_dtmax_ed()` calculates the time series of dTmax (the
#'  maximum temperature difference between sap flow probes under zero-flow
#'  conditions) using the environmental dependent method.
#'
#' @details The environmental dependent method is one of the methods for
#'  determining dTmax. This method filters the dTmax, estimated by the daily
#'  predawn method, using the environmental conditions when plants let their
#'  sap flow nearly zero. A stable dT, with a low coefficient of variation,
#'  and low air temperature or vapor pressure deficit over a two-hour period,
#'  characterizes these zero-flow conditions. See more details in Oishi et al.
#'  (2016; SoftwareX) and Peters et al. (2018; New Phytologist). After the
#'  filtering, the daily dTmax is interpolated if necessary.
#'
#' @inheritParams calc_dtmax_pd
#' @param vctr_vpd A vector of vapor pressure deficit (VPD, in hPa) time
#'  series. The length of the vector must match that of the timestamp vector.
#'  Missing values must be gap-filled previously. The unit of the time series
#'  must match that of `thres_vpd`.
#' @param vctr_ta A vector of air temperature (degrees Celsius) time series.
#'  The length of the vector must match that of the timestamp vector. Missing
#'  values must be gap-filled previously. The unit of the time series must
#'  match that of `thres_ta`.
#' @param thres_vpd A threshold value of the VPD to define predawn. Default is
#'  1.0 (hPa). The dTmax, estimated by the PD method, with VPD values below the
#'  threshold, is selected as a candidate for the final dTmax. The unit of the
#'  threshold must match that of the input VPD time series.
#' @param thres_ta A threshold value of the air temperature to define predawn.
#'  Default is 1.0 (degrees Celsius). The dTmax, estimated by the PD method,
#'  with air temperature values below the threshold, is selected as a candidate
#'  for the final dTmax. The unit of the threshold must match that of the input
#'  air temperature time series.
#' @param thres_cv A threshold value of the coefficient of variation (CV) to
#'  define predawn. Default is 0.005. The dTmax, estimated by the PD method,
#'  with CV values below the threshold, is selected as a candidate for the
#'  final dTmax.
#'
#' @returns
#' A data frame with columns below:
#'
#' * The first column, `time`, gives the timestamp of the measurements. If
#'  `output_daily` is `FALSE` (default), this column is the same as the input
#'  timestamp, `vctr_time`. If `output_daily` is `TRUE`, the timestamp in daily
#'  steps is returned.
#'
#' * The second column, `dt`, gives the input dT (the temperature difference
#'  between sap flow probes, degrees Celsius) time series. If `output_daily` is
#'  `TRUE`, dT is returned in daily steps. If `output_daily` is `FALSE`
#'  (default), this column is not output.
#'
#' * The third column, `dtmax_ed`, gives the estimated dTmax by the
#'  environmental dependent method. If `output_daily` is `FALSE` (default),
#'  this column has the same time step as the input timestamp. If
#'  `output_daily` is `TRUE`, the dTmax is returned in daily steps.
#'
#' @author Yoshiaki Hata
#'
#' @seealso `calc_dtmax`, `calc_dtmax_sp`, `calc_dtmax_pd`, `calc_dtmax_mw`,
#'  `calc_dtmax_dr`,
#'
#' @include utils.R

calc_dtmax_ed <-
  function(vctr_time, vctr_dt, vctr_radi, vctr_ta, vctr_vpd,
           thres_radi = 100, thres_ta = 1.0, thres_vpd = 1.0, thres_cv = 0.005,
           thres_hour_pd = 8, min_n_wndw_dtmax = 3, output_daily = FALSE) {
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

    if(is.null(vctr_radi) == TRUE) {
      stop("Input radiation time series must be provided to calculate dTmax")
    }

    if(is.null(vctr_ta) == TRUE) {
      stop("Input air temp. time series must be provided to calculate dTmax")
    }

    if(is.null(vctr_vpd) == TRUE) {
      stop("Input VPD time series must be provided to calculate dTmax")
    }

    if(anyNA(vctr_radi) == TRUE) {
      stop("One or more NA values exist in the input radiation time series")
    }

    if(anyNA(vctr_ta) == TRUE) {
      stop("One or more NA values exist in the input air temp. time series")
    }

    if(anyNA(vctr_vpd) == TRUE) {
      stop("One or more NA values exist in the input VPD time series")
    }

    if(min(vctr_vpd) < 0) {
      stop("Negative VPD values are not allowed")
    }

    ## create time vector for daily output
    time_head_output <-
      vctr_time[1] %>%
      - lubridate::minutes(interval_time) %>%
      lubridate::floor_date(unit = "day")

    time_tail_output <-
      vctr_time[length(vctr_time)] %>%
      - lubridate::minutes(interval_time) %>%
      lubridate::floor_date(unit = "day")

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
                 Ta = vctr_ta,
                 VPD = vctr_vpd) %>%
      dplyr::mutate(time_lag = time - lubridate::minutes(interval_time))

    message("--- dTmax calculation by the ED method started")

    for (i in 1:n_day) {
      setTxtProgressBar(pb, i)

      wndw_head <- wndw_head + lubridate::days(1)
      wndw_tail <-
        wndw_head + lubridate::days(1) - lubridate::minutes(interval_time)

      wndw <-
        data_all %>%
        dplyr::filter(time >= wndw_head & time <= wndw_tail & !is.na(dt) &
                        lubridate::hour(time_lag) < thres_hour_pd &
                        radi < thres_radi)

      if(nrow(wndw) < min_n_wndw_dtmax) next

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

      if(nrow(wndw_2h) < min_n_wndw_dtmax) next
      if(mean(wndw_2h$Ta, na.rm = TRUE) >= thres_ta &
         mean(wndw_2h$VPD, na.rm = TRUE) >= thres_vpd) next
      if(sd(wndw_2h$dt) / mean(wndw_2h$dt) >= thres_cv) next

      list_dTmax_ED[i] <- list_dTmax_PD[i]
    }

    message("\n--- dTmax calculation by the ED method finished")

    daily_dTmax <-
      data.frame(time = vctr_time_output,
                 dtmax_ed = list_dTmax_ED) %>%
      dplyr::mutate(dtmax_ed = zoo::na.approx(dtmax_ed, na.rm = FALSE)) %>%
      tidyr::fill(dtmax_ed, .direction = "updown")

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


#' Calculate dTmax by the daily predawn and environmental dependent methods
#'
#' @description `calc_dtmax_pd_ed()` calculates the time series of dTmax (the
#'  maximum temperature difference between sap flow probes under zero-flow
#'  conditions) using the daily predawn and the environmental dependent methods.
#'
#' @inheritParams calc_dtmax_pd
#' @inheritParams calc_dtmax_ed
#'
#' @author Yoshiaki Hata
#'
#' @include utils.R

calc_dtmax_pd_ed <-
  function(vctr_time, vctr_dt, vctr_radi, vctr_ta, vctr_vpd,
           thres_radi = 100, thres_ta = 1.0, thres_vpd = 1.0, thres_cv = 0.005,
           thres_hour_pd = 8, min_n_wndw_dtmax = 3, output_daily = FALSE) {
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

    if(is.null(vctr_radi) == TRUE) {
      stop("Input radiation time series must be provided to calculate dTmax")
    }

    if(is.null(vctr_vpd) == TRUE) {
      stop("Input VPD time series must be provided to calculate dTmax")
    }

    if(is.null(vctr_ta) == TRUE) {
      stop("Input air temp. time series must be provided to calculate dTmax")
    }

    if(is.null(vctr_vpd) == TRUE) {
      stop("Input VPD time series must be provided to calculate dTmax")
    }

    if(anyNA(vctr_radi) == TRUE) {
      stop("One or more NA values exist in the input radiation time series")
    }

    if(anyNA(vctr_ta) == TRUE) {
      stop("One or more NA values exist in the input air temp. time series")
    }

    if(anyNA(vctr_vpd) == TRUE) {
      stop("One or more NA values exist in the input VPD time series")
    }

    if(min(vctr_vpd) < 0) {
      stop("Negative VPD values are not allowed")
    }

    ## create time vector for daily output
    time_head_output <-
      vctr_time[1] %>%
      - lubridate::minutes(interval_time) %>%
      lubridate::floor_date(unit = "day")

    time_tail_output <-
      vctr_time[length(vctr_time)] %>%
      - lubridate::minutes(interval_time) %>%
      lubridate::floor_date(unit = "day")

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
                 Ta = vctr_ta,
                 VPD = vctr_vpd) %>%
      dplyr::mutate(time_lag = time - lubridate::minutes(interval_time))

    message("--- dTmax calculation by the PD and ED methods started")

    for (i in 1:n_day) {
      setTxtProgressBar(pb, i)

      wndw_head <- wndw_head + lubridate::days(1)
      wndw_tail <-
        wndw_head + lubridate::days(1) - lubridate::minutes(interval_time)

      wndw <-
        data_all %>%
        dplyr::filter(time >= wndw_head & time <= wndw_tail & !is.na(dt) &
                        lubridate::hour(time_lag) < thres_hour_pd &
                        radi < thres_radi)

      if(nrow(wndw) < min_n_wndw_dtmax) next

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

      if(nrow(wndw_2h) < min_n_wndw_dtmax) next
      if(mean(wndw_2h$Ta, na.rm = TRUE) >= thres_ta &
         mean(wndw_2h$VPD, na.rm = TRUE) >= thres_vpd) next
      if(sd(wndw_2h$dt) / mean(wndw_2h$dt) >= thres_cv) next

      list_dTmax_ED[i] <- list_dTmax_PD[i]
    }

    message("\n--- dTmax calculation by the PD and ED methods finished")

    daily_dTmax <-
      data.frame(time = vctr_time_output,
                 dtmax_pd = list_dTmax_PD,
                 dtmax_ed = list_dTmax_ED) %>%
      dplyr::mutate(dtmax_pd = zoo::na.approx(dtmax_pd, na.rm = FALSE),
                    dtmax_ed = zoo::na.approx(dtmax_ed, na.rm = FALSE)) %>%
      tidyr::fill(c(dtmax_pd, dtmax_ed), .direction = "updown")

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
#' @description `calc_dtmax_mw()` calculates the time series of dTmax (the
#'  maximum temperature difference between sap flow probes under zero-flow
#'  conditions) using the moving window method.
#'
#' @details The moving window method is one of the methods for determining
#'  dTmax. This method selects the maximum value of dTmax, estimated by the
#'  daily predawn method, using a moving window with an eleven-day length. The
#'  selected dTmax is considered to be the final dTmax. See more details in
#'  Peters et al. (2018; New Phytologist).
#'
#' @inheritParams calc_dtmax_pd
#' @param vctr_time_daily A timestamp vector of class POSIXct or POSIXt in
#'  daily steps. This vector indicates the start and end dates of the
#'  measurement, and is assumed to be output from the `calc_dtmax_pd()`
#'  function. The timestamps must be equally spaced and arranged
#'  chronologically. If this argument is `NULL` (default), `vctr_time`,
#'  `vctr_dt`, and `vctr_radi` must be provided to conduct the daily predawn
#'  method previously.
#' @param vctr_dtmax_pd A vector of dTmax estimated by the daily predawn method
#'  in daily steps. This vector is assumed to be output from the
#'  `calc_dtmax_pd()` function. The length of the vector must match that of the
#'  `vctr_time_daily`. If this argument is `NULL` (default), `vctr_time`,
#'  `vctr_dt`, and `vctr_radi` must be provided to conduct the daily predawn
#'  method previously.
#' @param wndw_size_dtmax A positive integer indicating the window size (days)
#'  for determining moving window maximum values of dTmax. Default is 11
#'  (days).
#' @param vctr_time Only valid when `vctr_dtmax_pd` is `NULL`. A timestamp
#'  vector of class POSIXct or POSIXt. This vector indicates the timings of the
#'  end of each measurement in local time. Any interval (typically 15 to 60
#'  min) is allowed, but the timestamps must be equally spaced and arranged
#'  chronologically. Default is `NULL`.
#' @param vctr_dt Only valid when `vctr_dtmax_pd` is `NULL`. A vector of dT
#'  (the temperature difference between sap flow probes, in degrees Celsius)
#'  time series. The length of the vector must match that of the `vctr_time`.
#'  Missing values must be gap-filled previously. Default is `NULL`.
#' @param vctr_radi Only valid when `vctr_dtmax_pd` is `NULL`. A vector of
#'  global solar radiation or a similar radiative variable time series. The
#'  length of the vector must match that of the `vctr_time`. Missing values
#'  must be gap-filled previously. The unit of the time series must match that
#'  of `thres_radi`. Default is `NULL`.
#'
#' @returns
#' A data frame with columns below:
#'
#' * The first column, `time`, gives the timestamp of the measurements. If
#'  `output_daily` is `FALSE` (default), this column is the same as the input
#'  timestamp, `vctr_time`. If `output_daily` is `TRUE`, the timestamp in daily
#'  steps is returned.
#'
#' * The second column, `dt`, gives the input dT (the temperature difference
#'  between sap flow probes, degrees Celsius) time series. If `output_daily` is
#'  `TRUE`, dT is returned in daily steps. If `output_daily` is `FALSE`
#'  (default), this column is not output.
#'
#' * The third column, `dtmax_mw`, gives the estimated dTmax by the moving
#'  window method. If `output_daily` is `FALSE` (default), this column has the
#'  same time step as the input timestamp. If `output_daily` is `TRUE`,
#'  the dTmax is returned in daily steps.
#'
#' @author Yoshiaki Hata
#'
#' @seealso `calc_dtmax`, `calc_dtmax_sp`, `calc_dtmax_pd`, `calc_dtmax_dr`,
#'  `calc_dtmax_ed`
#'
#' @include utils.R

calc_dtmax_mw <-
  function(vctr_time_daily = NULL, vctr_dtmax_pd = NULL, wndw_size_dtmax = 11,
           min_n_wndw_dtmax = 3, output_daily = FALSE,
           vctr_time = NULL, vctr_dt = NULL, vctr_radi = NULL, thres_radi = 100,
           thres_hour_pd = 8) {
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
        stop("One or more NA values exist in the input dTmax time series")
      }
      if(is.null(vctr_time_daily)) {
        stop("Time vector corresponding to dTmax time series must be provided")
      }
      if(length(vctr_dtmax_pd) != length(vctr_time_daily)) {
        stop("Both the input dTmax and time vectors must have the same length")
      }

      data_all <-
        data.frame(time = vctr_time_daily,
                   dTmax_PD = vctr_dtmax_pd) %>%
        dplyr::mutate(rownum = rownames(.) %>% as.numeric())
    } else {
      if(is.null(vctr_time)) {
        stop("Input time vector must be provided to calculate dTmax")
      }
      if(is.null(vctr_dt)) {
        stop("Input dT time series must be provided to calculate dTmax")
      }
      if(is.null(vctr_radi)) {
        stop("Input radiation time series must be provided to calculate dTmax")
      }
      daily_dTmax_PD <-
        calc_dtmax_pd(vctr_time, vctr_dt, vctr_radi, thres_radi,
                      thres_hour_pd, min_n_wndw_dtmax, output_daily = TRUE)

      data_all <-
        data.frame(time = daily_dTmax_PD$time,
                   dTmax_PD = daily_dTmax_PD$dtmax_pd) %>%
        dplyr::mutate(rownum = rownames(.) %>% as.numeric())
    }

    n_day <- length(data_all$time)
    list_dTmax_MW <- rep(NA, n_day)
    time_start <- data_all$time[1]
    time_end <- data_all$time[n_day]
    pb <- txtProgressBar(min = 1, max = n_day, style = 3)

    message("--- dTmax calculation by the MW method started")

    for (i in 1:n_day) {
      setTxtProgressBar(pb, i)

      time_now <- data_all$time[i]
      wndw_head <- time_now - lubridate::days((wndw_size_dtmax - 1)/2)
      wndw_tail <- time_now + lubridate::days((wndw_size_dtmax - 1)/2)

      wndw <-
        data_all %>%
        dplyr::filter(time >= wndw_head & time <= wndw_tail & !is.na(dTmax_PD))

      if(nrow(wndw) < min_n_wndw_dtmax) next

      ## estimate dTmax by moving window method
      list_dTmax_MW[i] <- wndw %>% dplyr::pull(dTmax_PD) %>% max()
    }

    message("\n--- dTmax calculation by the MW method finished")

    daily_dTmax <-
      data.frame(time = data_all$time,
                 dtmax_mw = list_dTmax_MW) %>%
      dplyr::mutate(dtmax_mw = zoo::na.approx(dtmax_mw, na.rm = FALSE)) %>%
      tidyr::fill(dtmax_mw, .direction = "updown")

    if(output_daily) {
      return(daily_dTmax)
    } else {
      if(is.null(vctr_time)) {
        stop("Original time vector must be provided to output the result")
      }
      if(is.null(vctr_dt)) {
        stop("Input dT time series must be provided to output the result")
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
#' @description `calc_dtmax_dr()` calculates the time series of dTmax (the
#'  maximum temperature difference between sap flow probes under zero-flow
#'  conditions) using the double regression method.
#'
#' @details The double regression method is one of the methods for determining
#'  dTmax. This method first calculates the moving window mean value of dTmax,
#'  estimated by the daily predawn method, with an eleven-day length. The dTmax
#'  that is lower than the mean is omitted, and then the moving window mean is
#'  recalculated as the final dTmax. See more details in Peters et al. (2018;
#'  New Phytologist).
#'
#' @inheritParams calc_dtmax_mw
#'
#' @returns
#' A data frame with columns below:
#'
#' * The first column, `time`, gives the timestamp of the measurements. If
#'  `output_daily` is `FALSE` (default), this column is the same as the input
#'  timestamp, `vctr_time`. If `output_daily` is `TRUE`, the timestamp in daily
#'  steps is returned.
#'
#' * The second column, `dt`, gives the input dT (the temperature difference
#'  between sap flow probes, degrees Celsius) time series. If `output_daily`
#'  is `TRUE`, dT is returned in daily steps. If `output_daily` is `FALSE`
#'  (default), this column is not output.
#'
#' * The third column, `dtmax_dr`, gives the estimated dTmax by the double
#'  regression method. If `output_daily` is `FALSE` (default), this column has
#'  the same time step as the input timestamp. If `output_daily` is `TRUE`,
#'  the dTmax is returned in daily steps.
#'
#' @author Yoshiaki Hata
#'
#' @seealso `calc_dtmax`, `calc_dtmax_sp`, `calc_dtmax_pd`, `calc_dtmax_mw`,
#'  `calc_dtmax_ed`
#'
#' @include utils.R

calc_dtmax_dr <-
  function(vctr_time_daily = NULL, vctr_dtmax_pd = NULL, wndw_size_dtmax = 11,
           min_n_wndw_dtmax = 3, output_daily = FALSE, vctr_time = NULL,
           vctr_dt = NULL, vctr_radi = NULL, thres_radi = 100,
           thres_hour_pd = 8) {
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
        stop("One or more NA values exist in the input dTmax time series")
      }
      if(is.null(vctr_time_daily)) {
        stop("Time vector corresponding to dTmax time series must be provided")
      }
      if(length(vctr_dtmax_pd) != length(vctr_time_daily)) {
        stop("Both the input dTmax and time vectors must have the same length")
      }

      data_all <-
        data.frame(time = vctr_time_daily,
                   dTmax_PD = vctr_dtmax_pd) %>%
        dplyr::mutate(rownum = rownames(.) %>% as.numeric())
    } else {
      if(is.null(vctr_time)) {
        stop("Input time vector must be provided to calculate dTmax")
      }
      if(is.null(vctr_dt)) {
        stop("Input dT time series must be provided to calculate dTmax")
      }
      if(is.null(vctr_radi)) {
        stop("Input radiation time series must be provided to calculate dTmax")
      }
      daily_dTmax_PD <-
        calc_dtmax_pd(vctr_time, vctr_dt, vctr_radi, thres_radi, thres_hour_pd,
                      min_n_wndw_dtmax, output_daily = TRUE)

      data_all <-
        data.frame(time = daily_dTmax_PD$time,
                   dTmax_PD = daily_dTmax_PD$dtmax_pd) %>%
        dplyr::mutate(rownum = rownames(.) %>% as.numeric())
    }

    n_day <- length(data_all$time)
    list_dTmax_DR <- rep(NA, n_day)
    time_start <- data_all$time[1]
    time_end <- data_all$time[n_day]
    pb <- txtProgressBar(min = 1, max = n_day, style = 3)

    message("--- dTmax calculation by the DR method started")

    for (i in 1:n_day) {
      setTxtProgressBar(pb, i)

      time_now <- data_all$time[i]
      wndw_head <- time_now - lubridate::days((wndw_size_dtmax - 1)/2)
      wndw_tail <- time_now + lubridate::days((wndw_size_dtmax - 1)/2)

      wndw <-
        data_all %>%
        dplyr::filter(time >= wndw_head & time <= wndw_tail & !is.na(dTmax_PD))

      if(nrow(wndw) < min_n_wndw_dtmax) next

      ## estimate dTmax by double regression method
      ave_wndw <- wndw %>% dplyr::pull(dTmax_PD) %>% mean()
      list_dTmax_DR[i] <-
        wndw %>%
        dplyr::filter(dTmax_PD >= ave_wndw) %>%
        dplyr::pull(dTmax_PD) %>%
        mean()
    }

    message("\n--- dTmax calculation by the DR method finished")

    daily_dTmax <-
      data.frame(time = data_all$time,
                 dtmax_dr = list_dTmax_DR) %>%
      dplyr::mutate(dtmax_dr = zoo::na.approx(dtmax_dr, na.rm = FALSE)) %>%
      tidyr::fill(dtmax_dr, .direction = "updown")

    if(output_daily) {
      return(daily_dTmax)
    } else {
      if(is.null(vctr_time)) {
        stop("Original time vector must be provided to output the result")
      }
      if(is.null(vctr_dt)) {
        stop("Input dT time series must be provided to output the result")
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


#' Calculate dTmax by the moving window and double regression methods
#'
#' @description `calc_dtmax_mw_dr()` calculates the time series of dTmax (the
#'  maximum temperature difference between sap flow probes under zero-flow
#'  conditions) using the moving window and the double regression methods.
#'
#' @inheritParams calc_dtmax_mw
#'
#' @author Yoshiaki Hata
#'
#' @include utils.R

calc_dtmax_mw_dr <-
  function(vctr_time_daily = NULL, vctr_dtmax_pd = NULL, wndw_size_dtmax = 11,
           min_n_wndw_dtmax = 3, output_daily = FALSE,
           vctr_time = NULL, vctr_dt = NULL, vctr_radi = NULL, thres_radi = 100,
           thres_hour_pd = 8) {
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
        stop("One or more NA values exist in the input dTmax time series")
      }
      if(is.null(vctr_time_daily)) {
        stop("Time vector corresponding to dTmax time series must be provided")
      }
      if(length(vctr_dtmax_pd) != length(vctr_time_daily)) {
        stop("Both the input dTmax and time vectors must have the same length")
      }

      data_all <-
        data.frame(time = vctr_time_daily,
                   dTmax_PD = vctr_dtmax_pd) %>%
        dplyr::mutate(rownum = rownames(.) %>% as.numeric())
    } else {
      if(is.null(vctr_time)) {
        stop("Input time vector must be provided to calculate dTmax")
      }
      if(is.null(vctr_dt)) {
        stop("Input dT time series must be provided to calculate dTmax")
      }
      if(is.null(vctr_radi)) {
        stop("Input radiation time series must be provided to calculate dTmax")
      }
      daily_dTmax_PD <-
        calc_dtmax_pd(vctr_time, vctr_dt, vctr_radi, thres_radi, thres_hour_pd,
                      min_n_wndw_dtmax, output_daily = TRUE)

      data_all <-
        data.frame(time = daily_dTmax_PD$time,
                   dTmax_PD = daily_dTmax_PD$dtmax_pd) %>%
        dplyr::mutate(rownum = rownames(.) %>% as.numeric())
    }

    n_day <- length(data_all$time)
    list_dTmax_MW <- rep(NA, n_day)
    list_dTmax_DR <- rep(NA, n_day)
    time_start <- data_all$time[1]
    time_end <- data_all$time[n_day]
    pb <- txtProgressBar(min = 1, max = n_day, style = 3)

    message("--- dTmax calculation by the MW and DR methods started")

    for (i in 1:n_day) {
      setTxtProgressBar(pb, i)

      time_now <- data_all$time[i]
      wndw_head <- time_now - lubridate::days((wndw_size_dtmax - 1)/2)
      wndw_tail <- time_now + lubridate::days((wndw_size_dtmax - 1)/2)

      wndw <-
        data_all %>%
        dplyr::filter(time >= wndw_head & time <= wndw_tail & !is.na(dTmax_PD))

      if(nrow(wndw) < min_n_wndw_dtmax) next

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

    message("\n--- dTmax calculation by the MW and DR methods finished")

    daily_dTmax <-
      data.frame(time = data_all$time,
                 dtmax_mw = list_dTmax_MW,
                 dtmax_dr = list_dTmax_DR) %>%
      dplyr::mutate(dtmax_mw = zoo::na.approx(dtmax_mw, na.rm = FALSE),
                    dtmax_dr = zoo::na.approx(dtmax_dr, na.rm = FALSE)) %>%
      tidyr::fill(c(dtmax_mw, dtmax_dr), .direction = "updown")

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


#' Calculate dTmax by various methods
#'
#' @description `calc_dtmax()` calculates the time series of dTmax (the maximum
#'  temperature difference between sap flow probes under zero-flow conditions)
#'  using multiple methods.
#'
#' @details This function provides multiple dTmax time series estimated by
#'  different methods below:
#'
#'  * The successive predawn (SP) method defines the dTmax for a day as the
#'   maximum dT (the temperature difference between sap flow probes) within a
#'   24-hour period that begins at 5:00 a.m. (default; just before daybreak in
#'   temperate zones). In other words, the day starts at predawn, not midnight,
#'   and the maximum value for that period is assumed to be dTmax. This method
#'   has the advantage of being able to calculate dTmax quickly while
#'   minimizing the effect of nocturnal transpiration on dTmax estimation.
#'
#'  * The daily predawn (PD) method defines the dTmax for a day as the maximum
#'   dT between midnight and the morning (8:00 a.m. in local time) when the
#'   global solar radiation is below the threshold value (100 W m-2). See more
#'   details in Peters et al. (2018; New Phytologist).
#'
#'  * The moving window (MW) method selects the maximum value of dTmax,
#'   estimated by the daily predawn method, using a moving window with an
#'   eleven-day length. The selected dTmax is considered to be the final dTmax.
#'   See more details in Peters et al. (2018; New Phytologist).
#'
#'  * The double regression (DR) method first calculates the moving window mean
#'   value of dTmax, estimated by the daily predawn method, with an eleven-day
#'   length. The dTmax that is lower than the mean is omitted, and then the
#'   moving window mean is recalculated as the final dTmax. See more details in
#'   Peters et al. (2018; New Phytologist).
#'
#'  * The environmental dependent (ED) method filters the dTmax, estimated by
#'   the daily predawn method, using the environmental conditions when plants
#'   let their sap flow nearly zero. A stable dT, with a low coefficient of
#'   variation, and low air temperature or vapor pressure deficit over a
#'   two-hour period, characterizes these zero-flow conditions. See more
#'   details in Oishi et al. (2016; SoftwareX) and Peters et al. (2018; New
#'   Phytologist). After the filtering, the daily dTmax is interpolated if
#'   necessary.
#'
#' @inheritParams calc_dtmax_sp
#' @inheritParams calc_dtmax_pd_ed
#' @inheritParams calc_dtmax_mw_dr
#' @param vctr_radi A vector of global solar radiation or a similar radiative
#'  variable time series. Default is `NULL`, but this vector must be provided
#'  when `method` includes `pd`, `mw`, `dr`, or `ed`. The length of the vector
#'  must match that of the timestamp vector. Missing values must be gap-filled
#'  previously. The unit of the time series must match that of `thres_radi`.
#' @param vctr_ta A vector of air temperature (degrees Celsius) time series.
#'  Default is `NULL`, but this vector must be provided when `method` includes
#'  `ed`. The length of the vector must match that of the timestamp vector.
#'  Missing values must be gap-filled previously. The unit of the time series
#'  must match that of `thres_ta`.
#' @param vctr_vpd A vector of vapor pressure deficit (VPD, in hPa) time
#'  series. Default is `NULL`, but this vector must be provided when `method`
#'  includes `ed`. The length of the vector must match that of the timestamp
#'  vector. Missing values must be gap-filled previously. The unit of the time
#'  series must match that of `thres_vpd`.
#' @param method A vector of characters indicating the dTmax estimation
#'  methods. "sp", "pd", "mw", "dr", and "ed" represent the successive predawn,
#'  daily predawn, moving window, double regression, and environmental
#'  dependent method, respectively. Default is
#'  `c("sp")`.
#'
#' @returns
#' A data frame with columns below:
#'
#' * The first column, `time`, gives the timestamp of the measurements. If
#'  `output_daily` is `FALSE` (default), this column is the same as the input
#'  timestamp, `vctr_time`. If `output_daily` is `TRUE`, the timestamp in daily
#'  steps is returned.
#'
#' * The second column, `dt`, gives the input dT time series. If `output_daily`
#'  is `TRUE`, dT is returned in daily steps. If `output_daily` is `FALSE`
#'  (default), this column is not output.
#'
#' * The other columns, which have the prefix "dtmax_", provide the dTmax
#'  calculated by the methods specified in `method`. The last two letters of
#'  the column name represent the name of the dTmax estimation method. "sp",
#'  "pd", "mw", "dr", and "ed" represent the successive predawn, daily predawn,
#'  moving window, double regression, and environmental dependent method,
#'  respectively. If `output_daily` is `FALSE` (default), this column has the
#'  same time step as the input timestamp. If `output_daily` is `TRUE`, the
#'  dTmax is returned in daily steps."
#'
#' @examples
#' ## Load data
#' data(dt_gf)
#' time <- dt_gf$time[1:480]
#' dt <- dt_gf$dt[1:480]
#' radi <- dt_gf$sw_in[1:480]
#' ta <- dt_gf$ta[1:480]
#' vpd <- dt_gf$vpd[1:480]
#'
#' ## Calculate dTmax from gap-filled dT time series
#' result <-
#'  calc_dtmax(vctr_time = time, vctr_dt = dt, vctr_radi = radi, vctr_ta = ta,
#'             vctr_vpd = vpd, method = c("sp", "pd", "mw", "dr", "ed"),
#'             thres_vpd = 6.0)
#'
#' @include utils.R
#'
#' @export

calc_dtmax <-
  function(vctr_time, vctr_dt, vctr_radi = NULL, vctr_ta = NULL,
           vctr_vpd = NULL, method = c("sp"),
           thres_hour_sp = 5, thres_radi = 100, thres_ta = 1.0,
           thres_vpd = 1.0, thres_cv = 0.005, thres_hour_pd = 8,
           min_n_wndw_dtmax = 3, wndw_size_dtmax = 11,
           output_daily = FALSE) {
    ## avoid "No visible binding for global variable" notes
    time_lag <- NULL
    time <- NULL
    dt <- NULL

    message("Zero-flow condition estimation started")

    ## check input values
    interval_time <- get_interval(vctr_time)
    timezone <- lubridate::tz(vctr_time[1])

    if(length(vctr_time) != length(vctr_dt)) {
      stop("input timestamp and dT time series must be the same length")
    }

    if(!is.null(vctr_radi)) {
      if(length(vctr_time) != length(vctr_radi)) {
        stop(paste("input timestamp and radiation time series must be the",
                   "same length"))
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

    if(!is.null(vctr_ta)) {
      if(length(vctr_time) != length(vctr_ta)) {
        stop(paste("input timestamp and air temp. time series must be the",
                   "same length"))
      }

      if(anyNA(vctr_ta) == TRUE) {
        stop("one or more NA values exist in the input air temp. time series")
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
          calc_dtmax_sp(vctr_time, vctr_dt, thres_hour_sp,
                        output_daily = TRUE)
      } else {
        dTmax_SP <-
          calc_dtmax_sp(vctr_time, vctr_dt, thres_hour_sp,
                        output_daily = FALSE)
      }
    }

    ## dTmax calculation by the PD and/or ED methods
    if(do_pd & do_ed) {
      daily_dTmax_PD_ED <-
        calc_dtmax_pd_ed(vctr_time, vctr_dt, vctr_radi, vctr_ta, vctr_vpd,
                         thres_radi, thres_ta, thres_vpd, thres_cv,
                         thres_hour_pd, min_n_wndw_dtmax, output_daily = TRUE)
    } else if(do_pd) {
      daily_dTmax_PD_ED <-
        calc_dtmax_pd(vctr_time, vctr_dt, vctr_radi, thres_radi, thres_hour_pd,
                      min_n_wndw_dtmax, output_daily = TRUE)
    } else if(do_ed) {
      daily_dTmax_PD_ED <-
        calc_dtmax_ed(vctr_time, vctr_dt, vctr_radi, vctr_ta, vctr_vpd,
                      thres_radi, thres_ta, thres_vpd, thres_cv, thres_hour_pd,
                      min_n_wndw_dtmax, output_daily = TRUE)
    }

    ## dTmax calculation by the MW and/or DR methods
    if(do_pd) {
      if(do_mw & do_dr) {
        daily_dTmax_MW_DR <-
          calc_dtmax_mw_dr(daily_dTmax_PD_ED$time, daily_dTmax_PD_ED$dtmax_pd,
                           wndw_size_dtmax, min_n_wndw_dtmax,
                           output_daily = TRUE)
      } else if(do_mw) {
        daily_dTmax_MW_DR <-
          calc_dtmax_mw(daily_dTmax_PD_ED$time, daily_dTmax_PD_ED$dtmax_pd,
                        wndw_size_dtmax, min_n_wndw_dtmax, output_daily = TRUE)
      } else if(do_dr) {
        daily_dTmax_MW_DR <-
          calc_dtmax_dr(daily_dTmax_PD_ED$time, daily_dTmax_PD_ED$dtmax_pd,
                        wndw_size_dtmax, min_n_wndw_dtmax, output_daily = TRUE)
      }
    } else if(do_mw & do_dr) {
      daily_dTmax_MW_DR <-
        calc_dtmax_mw_dr(vctr_time, vctr_dt, vctr_radi, thres_radi,
                         thres_hour_pd, wndw_size_dtmax, min_n_wndw_dtmax,
                         output_daily = TRUE)
    } else if(do_mw) {
      daily_dTmax_MW_DR <-
        calc_dtmax_mw(vctr_time, vctr_dt, vctr_radi, thres_radi,
                      thres_hour_pd, wndw_size_dtmax, min_n_wndw_dtmax,
                      output_daily = TRUE)
    } else if(do_dr) {
      daily_dTmax_MW_DR <-
        calc_dtmax_dr(vctr_time, vctr_dt, vctr_radi, thres_radi,
                      thres_hour_pd, wndw_size_dtmax, min_n_wndw_dtmax,
                      output_daily = TRUE)
    }

    ## aggregate calculated values
    message("--- dTmax time series aggregation started")

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

      message("--- dTmax time series aggregation finished")

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
        vctr_time_daily <-
          daily_dTmax$time_lag + lubridate::days(1) -
            lubridate::minutes(interval_time)

        vctr_time_daily[length(vctr_time_daily)] <- vctr_time[length(vctr_time)]

        daily_dTmax <-
          daily_dTmax %>%
          dplyr::mutate(time_lag = vctr_time_daily)

        dTmax <-
          merge.data.frame(dTmax, daily_dTmax, by = "time_lag", all = TRUE) %>%
          tidyr::fill(c(dplyr::contains("dtmax")), .direction = "updown") %>%
          dplyr::select(time, dt, dplyr::contains("dtmax"))
      }

      dTmax <-
        dTmax %>%
        dplyr::filter(time <= vctr_time[length(vctr_time)])

      message("--- dTmax time series aggregation finished")
      message("Zero-flow condition estimation finished")

      return(dTmax)
    }
  }

