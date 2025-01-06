#' Cross varidation for random forest structure
#'
#' @description rf_cv() constructs randow forest model trained from
#'   time series of temperature difference between granier-type sap flow probes
#'   for detecting outliers in the time series
#'
#' @param df Data frame containing dT time series
#' @param name_label Input column names
#' @param list_min_nodesize Minimum number of random forest node size
#' @param list_m_try The number of input column candidate
#' @param list_subsample Subsample ratio
#' @param list_feature list vector of feature variables
#' @param rf_replace replace sampling of non-replace sampling in random forest
#' @param rf_predict_all do prediction by constructed random forest
#' @param frac_train Fraction of training data set sampling
#' @param ran_seed Random seed of random forest
#' @param ntree The number of trees in random forest
#' @param err The label of error or missing values
#'
#' @examples
#' # Construct random forest structure
#' result <-
#'   rf_est(df, name_label, min_nodesize, m_try, subsample, list_feature,
#'          rf_replace = TRUE, rf_predict_all = FALSE,
#'          frac_train = 0.75, ran_seed = 12345, ntree = 500, err = -9999)

## cross validation function
rf_cv <- function(df, name_label,
                  list_min_nodesize, list_mtry, list_subsample,
                  list_feature, rf_replace = TRUE, frac_train = 0.75,
                  nfold = 10, ntree = 500, ran_seed = 12345, err = -9999) {
  ## settings
  set.seed(ran_seed)

  ## filter data
  data <- df %>% dplyr::filter(!!sym(name_label) != err)

  n_data <- nrow(data)
  rownum_train <- sample(n_data, size = n_data * frac_train)
  train <- data[rownum_train, ]

  train_feature <-
    train %>%
    dplyr::select(all_of(list_feature)) %>%
    as.matrix()
  train_target <-
    train %>%
    dplyr::select(all_of(name_label)) %>%
    as.matrix()

  CV_result <- data.frame(NULL)

  ## start CV
  for (i_min_nodesize in 1:length(list_min_nodesize)) {
    for (i_mtry in 1:length(list_mtry)) {
      for (i_subsample in 1:length(list_subsample)) {
        list_MSE_OOB <- NULL
        for (i_nfold in 1:nfold) {
          CV_model <-
            ranger::ranger(x = train_feature,
                           y = train_target,
                           min.node.size = list_min_nodesize[i_min_nodesize],
                           mtry = list_mtry[i_mtry],
                           sample.fraction = list_subsample[i_subsample],
                           replace = rf_replace,
                           num.trees = ntree,
                           seed = ran_seed + nfold - 1)
          list_MSE_OOB <- c(list_MSE_OOB, CV_model$prediction.error)
        }
        CV_result <-
          data.frame(min_nodesize = list_min_nodesize[i_min_nodesize],
                     mtry = list_mtry[i_mtry],
                     subsample = list_subsample[i_subsample],
                     ntree = ntree,
                     MSE_OOB = mean(list_MSE_OOB)) %>%
          dplyr::bind_rows(CV_result, .)

        CV_result %>%
          dplyr::slice_tail() %>%
          print()
      }
    }
  }

  message("Cross varidation was finished.")
  return(CV_result)
}



#' Construct random forest structure for outlier removal
#'
#' @description rf_est() constructs randow forest model trained from
#'   time series of temperature difference between granier-type sap flow probes
#'   for detecting outliers in the time series
#'
#' @param df Data frame containing dT time series
#' @param name_label Input column names
#' @param min_nodesize Minimum number of random forest node size
#' @param m_try The number of input column candidate
#' @param subsample Subsample ratio
#' @param list_feature list vector of feature variables
#' @param rf_replace replace sampling of non-replace sampling in random forest
#' @param rf_predict_all do prediction by constructed random forest
#' @param frac_train Fraction of training data set sampling
#' @param ran_seed Random seed of random forest
#' @param ntree The number of trees in random forest
#' @param err The label of error or missing values
#'
#' @examples
#' # Construct random forest structure
#' result <-
#'   rf_est(df, name_label, min_nodesize, m_try, subsample, list_feature,
#'          rf_replace = TRUE, rf_predict_all = FALSE,
#'          frac_train = 0.75, ran_seed = 12345, ntree = 500, err = -9999)

rf_est <-
  function(df, name_label, min_nodesize, m_try, subsample, list_feature,
           rf_replace = TRUE, rf_predict_all = FALSE,
           frac_train = 0.75, ran_seed = 12345, ntree = 500, err = -9999) {
    ## settings
    set.seed(ran_seed)

    ## filter data
    data <- df %>% dplyr::filter(!!sym(name_label) != err)

    ## create train and test data set
    n_data <- nrow(data)
    rownum_train <- sample(n_data, size = n_data * frac_train)
    train <- data[rownum_train, ]
    test <- data[-rownum_train, ]

    train_feature <-
      train %>%
      dplyr::select(all_of(list_feature)) %>%
      as.matrix()
    train_target <-
      train %>%
      dplyr::select(all_of(name_label)) %>%
      as.matrix()

    test_feature <-
      test %>%
      dplyr::select(all_of(list_feature)) %>%
      as.matrix()
    test_target <-
      test %>%
      dplyr::select(all_of(name_label)) %>%
      as.matrix()

    ## construct random forest
    RF_dT <-
      ranger::ranger(x = train_feature,
                     y = train_target,
                     min.node.size = min_nodesize,
                     mtry = m_try,
                     num.trees = ntree,
                     sample.fraction = subsample,
                     replace = rf_replace,
                     seed = ran_seed)

    message("Random forest construction finished.")

    MSE_test <-
      predict(RF_dT,
              data = test_feature,
              predict.all = FALSE) %>%
      .$predictions %>%
      data.frame(avg = .) %>%
      dplyr::mutate(target = test_target,
                    SE = (target - avg)^2) %>%
      dplyr::summarise(MSE = mean(SE))

    pred_dT <-
      predict(RF_dT,
              data = df,
              predict.all = rf_predict_all)

    if(rf_predict_all == TRUE) {
      df_output <-
        pred_dT$predictions %>%
        as.data.frame() %>%
        dplyr::transmute(med = apply(., MARGIN = 1, FUN = median),
                         Q1 = apply(., MARGIN = 1, FUN = quantile,
                                    probs = 0.25),
                         Q3 = apply(., MARGIN = 1, FUN = quantile,
                                    probs = 0.75),
                         IQR = Q3 - Q1)

      df_output$target_noisy <- dplyr::pull(df, !!sym(name_label))

      df_output <-
        df_output %>%
        dplyr::mutate(flag_out = ifelse(target_noisy != err &
                                          (target_noisy < Q1 - coef_IQR * IQR |
                                             target_noisy > Q3 + coef_IQR * IQR),
                                        1, 0),
                      flag_out = ifelse(target_noisy == err, 2, flag_out),
                      target_cleaned = ifelse(flag_out == 1, err, target_noisy))
    } else {
      df_output <-
        pred_dT$predictions %>%
        data.frame(avg = .)
    }

    message("Random forest prediction finished.")

    list(MSE = MSE_test, stats = df_output) %>% return()
  }



## Not run
## After running outlier removal function
#
# list_feature_dT <- c("rs", "ta", "vpd", "hrmin")
# n_feature_dT <- length(list_feature_dT)
#
# list_min_nodesize_1st <- c(5)
# list_mtry_1st <- seq(2, n_feature_dT - 1)
# list_subsample_1st <- c(0.1)
#
# list_min_nodesize_2nd <- c(5)
# list_mtry_2nd <- seq(2, n_feature_dT - 1)
# list_subsample_2nd <- c(1)
# coef_IQR <- 1.5
#
# CV_dT_noisy <-
#   dt_noisy %>%
#   select(rs, ta, vpd) %>%
#   bind_cols(rslt_test_1) %>%
#   rf_cv(., "dT_z",
#         list_min_nodesize_1st, list_mtry_1st, list_subsample_1st,
#         list_feature_dT, rf_replace = FALSE)
#
# CV_dT_noisy %>%
#   filter(subsample == list_subsample_1st[1]) %>%
#   arrange(MSE_OOB) %>%
#   head(1) %>% {
#     pull(., min_nodesize) ->> min_nodesize_dT
#     pull(., mtry) ->> mtry_dT
#     pull(., subsample) ->> subsample_dT
#   }
#
#
# ## random forest for outlier detection
# RF_1st <-
#   dt_noisy %>%
#   select(rs, ta, vpd) %>%
#   bind_cols(rslt_test_1) %>%
#   rf_est(., "dT_z",
#          min_nodesize_dT, mtry_dT, subsample_dT,
#          list_feature_dT, rf_replace = FALSE, rf_predict_all = TRUE)
#
# dt_process <-
#   rslt_test_1 %>%
#   mutate(med = RF_1st$stats$med,
#          Q1 = RF_1st$stats$Q1,
#          Q3 = RF_1st$stats$Q3,
#          IQR = RF_1st$stats$IQR,
#          flag_out = RF_1st$stats$flag_out,
#          dT_z_mod1 = RF_1st$stats$target_cleaned)
#
#
# time_head <- ymd_hm("2013/08/01 00:30")
# time_tail <- ymd_hm("2013/09/01 00:00")
#
# dt_process %>%
#   na_if(-9999) %>%
#   ggplot()+
#   theme_classic()+
#   labs(x = "",
#        y = bquote( Standardized~Delta*italic("T")~(degree*C) ))+
#   theme(axis.title.y = element_text(size = 10),
#         axis.text.x = element_text(size = 8, colour = "black"),
#         axis.text.y = element_text(size = 8, colour = "black"),
#         legend.position = "none")+
#   scale_x_datetime(limits = c(time_head, time_tail))+
#   scale_color_manual(values = c("grey", "red3", "royalblue"), name = "")+
#   geom_line(aes(time, med), col = "red3", lwd = 1, alpha = 0.6)+
#   geom_ribbon(aes(time, ymin = Q1 - coef_IQR * IQR, ymax = Q3 + coef_IQR * IQR),
#               fill = "orange", alpha = 0.6)+
#   geom_point(aes(time, dT_z, col = as.factor(flag_out)), size = 3, alpha = 0.6)
#
#
## random forest for gap-filling
# CV_dT_cleaned <-
#   dt_noisy %>%
#   select(rs, ta, vpd) %>%
#   bind_cols(dt_process, .) %>%
#   rf_cv(., "dT_z_mod1",
#         list_min_nodesize_2nd, list_mtry_2nd, list_subsample_2nd,
#         list_feature_dT)
#
# CV_dT_cleaned %>%
#   filter(subsample == list_subsample_2nd[1]) %>%
#   arrange(MSE_OOB) %>%
#   head(1) %>% {
#     pull(., min_nodesize) ->> min_nodesize_dT
#     pull(., mtry) ->> mtry_dT
#     pull(., subsample) ->> subsample_dT
#   }
#
# RF_2nd <-
#   dt_noisy %>%
#   select(rs, ta, vpd) %>%
#   bind_cols(rslt_test_1) %>%
#   rf_est(dt_process, "dT_z_mod1",
#          min_nodesize_dT, mtry_dT, subsample_dT,
#          list_feature_dT, rf_replace = TRUE, rf_predict_all = FALSE)
#
# MSE_test_RF_2nd_dT_La1_01 <- RF_2nd$MSE %>% as.numeric()
#
# ## gap-fill and convert standardized dT to real dT
# data_dT_La1_01 %<>%
#   mutate(dT_z_pred = RF_2nd$stats$avg,
#          dT_z_mod2 = ifelse(dT_z_mod1 == -9999, NA, dT_z_mod1),
#          dT_z_mod2 = na.approx(dT_z_mod2, maxgap = 1, na.rm = FALSE),
#          dT_z_mod2 = ifelse(is.na(dT_z_mod2), dT_z_pred, dT_z_mod2)) %>%
#   replace_na(list(dT_mod2 = -9999, dT_mod3 = -9999)) %>%
#   mutate(dT_mod4 = dT_z_mod2 * dT_sd_ref_La1_01 + dT_avg_ref_La1_01,
#          QC_dT = ifelse(dT_mod3 != dT_mod2, 1, 0),
#          QC_dT = ifelse(flag_out >= 1, 2, QC_dT))






