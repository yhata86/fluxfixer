#' Run all quality control processes automatically
#'
#' @description `run_fluxfixer()` provides a sophisticated protocol for
#'  post-processing raw time series, which can be applied not only to thermal
#'  dissipation sap flow data but also to other noisy time series, using
#'  classic statistical and machine-learning methods. In the sap flow data
#'  processing, users can select multiple methods to estimate the dTmax (the
#'  maximum temperature difference between sap flow probes under zero-flow
#'  conditions) and Fd (sap flux density) time series.
#'
#' @details
#' This function executes a series of quality-control processes on the input
#' time series. The protocol includes the absolute limit test, short-term drift
#' correction, high-frequency noise filtering, outlier removal by Z-score and a
#' random forest model, gap-filling using the data-driven model, detrending,
#' signal attenuation correction, as well as zero-flow condition estimation,
#' heartwood correction, and sap flux density calculation for thermal
#' dissipation sap flow data. See more details in the vignettes:
#' `browseVignettes("fluxfixer")`, or in each step-by-step function help page.
#'
#' Here are some tips helpful for users:
#'
#' * If users want to do only quality control unrelated to sap flow
#'  calculation, set `skip_sapflow_calc` as `TRUE`. Estimation of the zero-flow
#'  condition and calculation of sap flux density are skipped in this setting.
#'
#' * If the input time series has sudden shifts of average and/or standard
#'  deviation due to various reasons, including sensor replacement, specify the
#'  end timings of these events in `vctr_time_prd_tail`. The Z-score
#'  transformation gets applied to each sub-period defined by these timestamps,
#'  calculating a more reasonable Z-score time series.
#'
#' * When processing sap flow data, it is highly recommended that users include
#'  a time series of vapor pressure deficit into the input data frame, as it
#'  typically controls stomatal opening. If the sap flow measurement is
#'  conducted in forests with high seasonality, such as deciduous forests, it
#'  is also recommended to include a time series for the amount of forest leaf
#'  (leaf area index).
#'
#' @inheritParams remove_manually
#' @inheritParams check_absolute_limits
#' @inheritParams modify_short_drift
#' @inheritParams filter_highfreq_noise
#' @inheritParams remove_zscore_outlier
#' @inheritParams remove_rf_outlier
#' @inheritParams fill_gaps
#' @inheritParams calc_ref_stats
#' @inheritParams retrieve_ts
#' @inheritParams calc_dtmax
#' @inheritParams calc_fd
#' @param df A data frame including evenly spaced time stamps in local time and
#'  the corresponding raw time series to be post-processed. For thermal
#'  dissipation sap flow data, the targeted time series must be a dT (the
#'  temperature difference between sap flow probes) time series. To conduct
#'  outlier removal and gap-filling using a random forest model, any time
#'  series of meteorological, soil environment, and ecophysiological factors
#'  can be included. If users conduct the zero-flow condition estimation, the
#'  gap-filled time series of global solar radiation or a similar radiative
#'  variable (for the PD, MW, DR, and ED methods) and air temperature and vapor
#'  pressure deficit (for the ED method) must be included.
#' @param colname_time A character representing the name of the column in the
#'  input data frame for the timestamp time series. This column indicates the
#'  timings of the end of each measurement in local time. Any interval
#'  (typically 15 to 60 min) is allowed, but the timestamps must be equally
#'  spaced and arranged chronologically.
#' @param colname_target A character representing the name of the column in the
#'  input data frame for a targeted time series to be post-processed. For
#'  thermal dissipation sap flow data, the targeted time series must be a dT
#'  (the temperature difference between sap flow probes) time series.
#' @param detrend A boolean. If `TRUE`, detrending is applied and the reference
#'  average is used to convert the Z-score time series to the time series in
#'  its original units; else, detrending is not applied, and the moving window
#'  average time series is used for the conversion. Default is `FALSE`.
#' @param correct_damping A boolean. If `TRUE`, the signal damping correction
#'  is applied, and the reference standard deviation is used to convert the
#'  Z-score time series into the time series in its original units; else, the
#'  correction is not applied, and the moving window standard deviation time
#'  series is used in the conversion. Default is `FALSE`.
#' @param skip_sapflow_calc A boolean. If `TRUE`, zero-flow condition
#'  estimation and sap flux density calculation are skipped in the whole
#'  process. This setting is useful when users want to post-process a time
#'  series unrelated to sap flow measurements. Default is `FALSE`.
#' @param colname_radi A character representing the name of the column in the
#'  input data frame for global solar radiation or a similar radiative variable
#'  time series. Default is `NULL`, but this name must be provided when
#'  `method` includes `pd`, `mw`, `dr`, or `ed`. Missing values in the column
#'  must be gap-filled previously. The unit of the time series must match that
#'  of `thres_radi`.
#' @param colname_ta A character representing the name of the column in the
#'  input data frame for air temperature (degrees Celsius) time series. Default
#'  is `NULL`, but this name must be provided when `method` includes `ed`.
#'  Missing values must be gap-filled previously. The unit of the time series
#'  must match that of `thres_ta`.
#' @param colname_vpd A character representing the name of the column in the
#'  input data frame for vapor pressure deficit (VPD, in hPa) time series.
#'  Default is `NULL`, but this name must be provided when `method` includes
#'  `ed`. Missing values must be gap-filled previously. The unit of the time
#'  series must match that of `thres_vpd`.
#'
#' @returns
#' * The first column, `time`, gives the same timestamp as the input timestamp
#'  specified by `colname_time`.
#'
#' * The second column, `raw`, gives the same targeted time series specified by
#'  `colname_target`.
#'
#' * The third column, `processed`, gives the post-processed targeted time
#'  series.
#'
#' * The fourth column, `qc`, gives a quality-control (QC) flag time series
#'  indicating the history of modifications to each data point. The flag is
#'  originally represented as 10-bit binary numbers, but is converted to
#'  decimal before being output. Each bit is set to 1 if the corresponding
#'  process is applied to the data point. From right to left, the number
#'  represents the process of initial missing value detection, manual error
#'  value removal, outlier removal by absolute limit, short-term drift
#'  correction, high-frequency noise filtering, Z-score outlier removal,
#'  random forest outlier removal, gap-filling, detrending, and signal damping
#'  correction.
#'
#' If `skip_sapflow_calc` is `FALSE`, the columns below are also output.
#'
#' * The columns which have the prefix "dtmax_" provide the dTmax calculated by
#'  the methods specified in `method`. The last two letters of the column name
#'  represent the name of the dTmax estimation method. "sp", "pd", "mw", "dr",
#'  and "ed" represent the successive predawn, daily predawn, moving window,
#'  double regression, and environmental dependent method, respectively.
#'
#' * The columns which have the prefix "fd_" provide the Fd calculated using
#'  the dTmax estimated by the methods specified in `method`. The last two
#'  letters of the column name represent the name of the dTmax estimation
#'  method.
#'
#' @examples
#' ## Load data
#' data("dt_noisy")
#'
#' ## Specify timestamps for the short-term drift correction
#' time_drft_head <- as.POSIXct("2013/05/14 13:30", tz = "Etc/GMT-8")
#' time_drft_tail <- as.POSIXct("2013/05/17 15:00", tz = "Etc/GMT-8")
#'
#' ## Specify the sensor replacement timing if necessary
#' time_prd_tail <- as.POSIXct("2013/05/14 13:00", tz = "Etc/GMT-8")
#'
#' ## Run all processes automatically
#' ## Note: It takes about 1 minute to complete all processes
#' result <-
#'   run_fluxfixer(df = dt_noisy, colname_time = "time", colname_target = "dt",
#'                 vctr_time_drft_head = time_drft_head,
#'                 vctr_time_drft_tail = time_drft_tail,
#'                 vctr_time_prd_tail = time_prd_tail,
#'                 detrend = TRUE)
#'
#' @author Yoshiaki Hata
#'
#' @seealso `remove_manually`, `check_absolute_limits`, `modify_short_drift`,
#'  `filter_highfreq_noise`, `remove_zscore_outlier`, `remove_rf_outlier`,
#'  `calc_ref_stats`, `fill_gaps`, `retrieve_ts`, `calc_dtmax`, `calc_fd`
#'
#' @export

run_fluxfixer <-
  function(df, colname_time, colname_target,
           vctr_time_err = NULL, label_err = -9999,
           thres_al_min = 3.0, thres_al_max = 50.0,
           vctr_time_drft_head = NULL, vctr_time_drft_tail = NULL,
           n_day_ref = 3, vctr_time_noise = NULL, wndw_size_noise = 13,
           inv_sigma_noise = 0.01, vctr_time_prd_tail = NULL,
           wndw_size_z = 48 * 15, min_n_wndw_z = 5, thres_z = 5.0,
           n_calc_max = 10, modify_z = FALSE, vctr_time_zmod = NULL,
           wndw_size_conv = 48 * 15, inv_sigma_conv = 0.01, thres_ratio = 0.5,
           vctr_colname_feature = NULL, vctr_min_nodesize = c(5),
           vctr_m_try = NULL, vctr_subsample_outlier = c(0.1),
           vctr_subsample_gf = c(1), frac_train = 0.75, n_tree = 500,
           ran_seed = 12345, coef_iqr = 1.5, wndw_size_ref = 48 * 15,
           detrend = FALSE, correct_damping = FALSE, skip_sapflow_calc = FALSE,
           colname_radi = NULL, colname_ta = NULL, colname_vpd = NULL,
           method = c("sp"), thres_hour_sp = 5, thres_radi = 100,
           thres_ta = 1.0, thres_vpd = 1.0, thres_cv = 0.005,
           thres_hour_pd = 8, min_n_wndw_dtmax = 3, wndw_size_dtmax = 11,
           alpha = 1.19 * 10^(-4), beta = 1.231,
           do_heartwood_correction = FALSE, ratio_conductive = NULL) {
    ## avoid "No visible binding for global variable" notes
    . <- NULL
    z_target <- NULL
    avg_target <- NULL
    sd_target <- NULL
    target <- NULL
    target_raw <- NULL
    qc_target <- NULL
    target_mr <- NULL
    target_al <- NULL
    target_st <- NULL
    target_nf <- NULL
    target_zs <- NULL
    target_rf <- NULL

    message("*** Fluxfixer started running ***")

    ## Pick up specific columns
    vctr_time <- df %>% dplyr::pull(!!colname_time)
    vctr_target <- df %>% dplyr::pull(!!colname_target)
    vctr_target_raw <- vctr_target

    if(!is.null(colname_radi)) {
      vctr_radi <- df %>% dplyr::pull(!!colname_radi)
    } else {
      vctr_radi <- NULL
    }

    if(!is.null(colname_ta)) {
      vctr_ta <- df %>% dplyr::pull(!!colname_ta)
    } else {
      vctr_ta <- NULL
    }

    if(!is.null(colname_vpd)) {
      vctr_vpd <- df %>% dplyr::pull(!!colname_vpd)
    } else {
      vctr_vpd <- NULL
    }

    ## remove error values manually
    if(!is.null(vctr_time_err)) {
      vctr_target <-
        remove_manually(vctr_time, vctr_target, vctr_time_err, label_err)
      message("Manual error value removal finished")
    } else {
      message("Manual error value removal was not applied")
    }

    vctr_target_mr <- vctr_target

    ## Remove outliers by absolute limits
    vctr_target <-
      check_absolute_limits(vctr_target, thres_al_min, thres_al_max, label_err)
    message("Absolute limits test finished")

    vctr_target_al <- vctr_target

    ## Modify short-term drifts
    if(!is.null(vctr_time_drft_head)) {
      vctr_target <-
        modify_short_drift(vctr_time, vctr_target, vctr_time_drft_head,
                           vctr_time_drft_tail, n_day_ref, label_err)
      message("Short-term drift correction finished")
    } else {
      message("Short-term drift correction was not applied")
    }

    vctr_target_st <- vctr_target

    ## High frequency noise filtering
    if(!is.null(vctr_time_noise)) {
      vctr_target <-
        filter_highfreq_noise(vctr_time, vctr_target, vctr_time_noise,
                              wndw_size_noise, inv_sigma_noise, label_err)
      message("High frequency noise filtering finished")
    } else {
      message("High frequency noise filtering was not applied")
    }

    vctr_target_nf <- vctr_target


    ## Z-score outlier detection
    df_zscore <-
      remove_zscore_outlier(vctr_time, vctr_target, vctr_time_prd_tail,
                            wndw_size_z, min_n_wndw_z, thres_z, n_calc_max,
                            modify_z, vctr_time_zmod, wndw_size_conv,
                            inv_sigma_conv, thres_ratio, label_err)

    vctr_target <- df_zscore$z_cleaned
    vctr_target_zs <- vctr_target

    ## Random forest outlier detection
    df_rf <-
      df %>%
      dplyr::mutate(target_zs = vctr_target_zs) %>%
      dplyr::select(-c(dplyr::all_of(colname_time),
                       dplyr::all_of(colname_target))) %>%
      remove_rf_outlier(., colname_label = "target_zs",
                        vctr_colname_feature, vctr_min_nodesize,
                        vctr_m_try, vctr_subsample_outlier, frac_train,
                        n_tree, ran_seed, coef_iqr, label_err)

    vctr_target <- df_rf$stats$cleaned
    vctr_target_rf <- vctr_target

    ## Reference value definition
    if(detrend == TRUE | correct_damping == TRUE) {
      message("Reference value definition started")

      vctr_stats_ref <-
        data.frame(z_target = vctr_target_rf,
                   avg_target = df_zscore$avg_cleaned,
                   sd_target = df_zscore$sd_cleaned) %>%
        dplyr::mutate(target = ifelse(z_target != label_err &
                                        avg_target != label_err &
                                        sd_target != label_err,
                                      z_target * sd_target + avg_target,
                                      label_err)) %>%
        dplyr::pull(target) %>%
        calc_ref_stats(vctr_time, ., vctr_time_prd_tail, wndw_size_ref,
                       label_err)

      message("--- Reference average of target time series: ",
              vctr_stats_ref[1])
      message("--- Reference standard deviation of target time series: ",
              vctr_stats_ref[2])
      message("Reference value definition finished")
    } else {
      vctr_stats_ref <- NULL
      message("Reference value definition was not applied")
    }


    ## Fill Z-score gaps
    df_gf <-
      df %>%
      dplyr::mutate(target_rf = vctr_target_rf) %>%
      dplyr::select(-c(dplyr::all_of(colname_time),
                       dplyr::all_of(colname_target))) %>%
      fill_gaps(., colname_label = "target_rf", vctr_colname_feature,
               vctr_min_nodesize, vctr_m_try, vctr_subsample_gf,
               frac_train, n_tree, ran_seed, label_err)

    vctr_target <- df_gf$stats$gapfilled
    vctr_target_gf <- vctr_target

    ## Retrieve time series in its original units
    vctr_target <-
      retrieve_ts(vctr_target_gf, vctr_target_avg = df_zscore$avg_cleaned,
                  vctr_target_sd = df_zscore$sd_cleaned,
                  detrend, correct_damping,
                  avg_ref = vctr_stats_ref[1], sd_ref = vctr_stats_ref[2],
                  label_err = -9999)

    vctr_target_rt <- vctr_target

    ## Quality control flag determination
    df_target <-
      data.frame(target_raw = vctr_target_raw,
                 target_mr = vctr_target_mr,
                 target_al = vctr_target_al,
                 target_st = vctr_target_st,
                 target_nf = vctr_target_nf,
                 target_zs = vctr_target_zs,
                 target_rf = vctr_target_rf,
                 target_gf = vctr_target_gf,
                 target_rt = vctr_target_rt)

    vctr_qc_target <-
      df_target %>%
      dplyr::mutate(qc_target = 0) %>%
      dplyr::mutate(qc_target = ifelse(target_raw == label_err, qc_target + 2^1,
                                       qc_target),
                    qc_target = ifelse(target_mr != target_raw, qc_target + 2^2,
                                       qc_target),
                    qc_target = ifelse(target_al != target_mr, qc_target + 2^3,
                                       qc_target),
                    qc_target = ifelse(target_st != target_al, qc_target + 2^4,
                                       qc_target),
                    qc_target = ifelse(target_nf != target_st, qc_target + 2^5,
                                       qc_target),
                    qc_target = ifelse(target_zs == label_err &
                                         target_nf != label_err,
                                       qc_target + 2^6, qc_target),
                    qc_target = ifelse(target_rf == label_err &
                                         target_zs != label_err,
                                       qc_target + 2^7, qc_target),
                    qc_target = ifelse(detrend == TRUE, qc_target + 2^8,
                                       qc_target),
                    qc_target = ifelse(correct_damping == TRUE,
                                       qc_target + 2^9, qc_target)) %>%
      dplyr::pull(qc_target)

    message("Quality-control flag determination finished")

    df_output <-
      data.frame(time = vctr_time,
                 raw = vctr_target_raw,
                 processed = vctr_target_rt,
                 qc = vctr_qc_target)

    if(skip_sapflow_calc) {
      message("dTmax and Fd calculation processes skipped")
      return(df_output)
    }


    ## Zero-flow condition estimation
    df_dtmax <-
      calc_dtmax(vctr_time, vctr_target_rt, vctr_radi, vctr_ta, vctr_vpd,
                 method, thres_hour_sp, thres_radi, thres_ta, thres_vpd,
                 thres_cv, thres_hour_pd, min_n_wndw_dtmax, wndw_size_dtmax,
                 output_daily = FALSE)

    ## Calculate sap flow density
    message("Fd calculation process started")

    if(do_heartwood_correction) {
      message("--- Heartwood correction was applied")
    } else {
      message("--- Heartwood correction was not applied")
    }

    df_fd <- data.frame(time = vctr_time)

    if(length(method[method %in% "sp"]) > 0) {
      df_fd$fd_sp <-
        calc_fd(vctr_target_rt, df_dtmax$dtmax_sp, alpha, beta,
                do_heartwood_correction, ratio_conductive)
      message("--- Fd calculation by the SP method finished")
    }

    if(length(method[method %in% "pd"]) > 0) {
      df_fd$fd_pd <-
        calc_fd(vctr_target_rt, df_dtmax$dtmax_pd, alpha, beta,
                do_heartwood_correction, ratio_conductive)
      message("--- Fd calculation by the PD method finished")
    }

    if(length(method[method %in% "mw"]) > 0) {
      df_fd$fd_mw <-
        calc_fd(vctr_target_rt, df_dtmax$dtmax_mw, alpha, beta,
                do_heartwood_correction, ratio_conductive)
      message("--- Fd calculation by the MW method finished")
    }

    if(length(method[method %in% "dr"]) > 0) {
      df_fd$fd_dr <-
        calc_fd(vctr_target_rt, df_dtmax$dtmax_dr, alpha, beta,
                do_heartwood_correction, ratio_conductive)
      message("--- Fd calculation by the DR method finished")
    }

    if(length(method[method %in% "ed"]) > 0) {
      df_fd$fd_ed <-
        calc_fd(vctr_target_rt, df_dtmax$dtmax_ed, alpha, beta,
                do_heartwood_correction, ratio_conductive)
      message("--- Fd calculation by the ED method finished")
    }

    message("Fd calculation process finished")

    df_output <-
      df_dtmax %>%
      dplyr::select(dplyr::contains("dtmax")) %>%
      dplyr::bind_cols(df_output, .)

    df_output <-
      df_fd %>%
      dplyr::select(dplyr::contains("fd")) %>%
      dplyr::bind_cols(df_output, .)

    message("*** Fluxfixer finished running successfully ***")
    return(df_output)
  }

