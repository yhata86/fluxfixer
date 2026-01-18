#' Tune hyperparameters used in a random forest model
#'
#' @description `rf_fit()` conducts an out-of-bag evaluation for
#'  hyperparameters used in constructing the random forest model using a grid
#'  search approach.
#'
#' @inheritParams n_valid
#' @param df A data frame including label (explained variable) and feature
#'  (explanatory variables) time series for model input. It is acceptable to
#'  include missing values in each column.
#' @param colname_label A character representing the name of the column for the
#'  label time series.
#' @param vctr_colname_feature A vector of characters indicating the name of
#'  the feature time series columns. If `NULL` (default), all columns excluding
#'  the label column specified as `colname_label` in the input data frame are
#'  used as feature columns.
#' @param vctr_min_nodesize A vector of positive integers indicating candidates
#'  of a hyperparameter defining the minimal node size (the minimum number of
#'  data points included in each leaf node). Default is `c(5)`.
#' @param vctr_m_try A vector of positive integers indicating candidates of a
#'  hyperparameter defining the number of features to be used in splitting
#'  each node. If `NULL` (default), integers between two and the number of all
#'  feature variables are tested.
#' @param vctr_subsample A vector of numerical values between 0 and 1,
#'  indicating candidates of a hyperparameter defining the fraction of input
#'  training data points to be sampled in constructing the random forest.
#'  Default is `c(0.1)`.
#' @param frac_train A numerical value between 0 and 1, defining the fraction
#'  of data points to be categorized as training data. The other data points
#'  are classified as test data. Default is 0.75.
#' @param n_tree An integer representing the number of trees in the random
#'  forest. Default is 500.
#' @param ran_seed An integer representing the random seed. Default is 12345.
#'
#' @returns
#' A data frame with columns below:
#'
#' * The first column, `min_nodesize`, gives one of the `vctr_min_nodesize`
#'  hyperparameter values tested in each model construction during
#'  out-of-bag evaluation.
#'
#' * The second column, `m_try`, gives one of the `vctr_m_try` hyperparameter
#'  values tested in each model construction during out-of-bag evaluation.
#'
#' * The third column, `subsample`, gives one of the `vctr_subsample`
#'  hyperparameter values tested in each model construction during
#'  out-of-bag evaluation.
#'
#' * The fourth column, `MSE_OOB`, provides the mean squared error between the
#'  predicted and original values in out-of-bag data in each model construction
#'  during the evaluation.
#'
#' @author Yoshiaki Hata

rf_fit <-
  function(df, colname_label, vctr_colname_feature = NULL,
           vctr_min_nodesize = c(5), vctr_m_try = NULL,
           vctr_subsample = c(0.1), frac_train = 0.75,
           n_tree = 500, ran_seed = 12345, label_err = -9999) {
    ## avoid "No visible binding for global variable" notes
    . <- NULL

    ## check input values
    if(!is.null(vctr_colname_feature) & length(vctr_colname_feature) <= 1) {
      stop("The number of input features must be two or more")
    }

    ## feature name determination
    if(is.null(vctr_colname_feature)) {
      vctr_colname_feature <- colnames(df) %>% .[. != colname_label]
    }

    if(is.null(vctr_m_try)) {
      vctr_m_try <- seq(2, length(vctr_colname_feature))
    }

    ## settings
    set.seed(ran_seed)

    ## filter data
    data <-
      df %>%
      dplyr::filter(!!rlang::sym(colname_label) != label_err) %>%
      dplyr::na_if(label_err)

    n_data <- nrow(data)
    rownum_train <- sample(n_data, size = n_data * frac_train)
    train <- data[rownum_train, ]

    train_feature <-
      train %>%
      dplyr::select(dplyr::all_of(vctr_colname_feature)) %>%
      as.matrix()
    train_target <-
      train %>%
      dplyr::select(dplyr::all_of(colname_label)) %>%
      as.matrix()

    CV_result <- data.frame(NULL)

    ## start CV
    message("--- MSE: Mean square error for out-of-bag data")
    message("--- Hyperparameter set: [m_try, min_nodesize, subsample]")

    for (i_min_nodesize in 1:length(vctr_min_nodesize)) {
      for (i_m_try in 1:length(vctr_m_try)) {
        for (i_subsample in 1:length(vctr_subsample)) {
          ran_seed <- ran_seed + 1

          CV_model <-
            ranger::ranger(x = train_feature,
                           y = train_target,
                           min.node.size = vctr_min_nodesize[i_min_nodesize],
                           mtry = vctr_m_try[i_m_try],
                           sample.fraction = vctr_subsample[i_subsample],
                           num.trees = n_tree,
                           seed = ran_seed)
        CV_result <-
          data.frame(min_nodesize = vctr_min_nodesize[i_min_nodesize],
                     m_try = vctr_m_try[i_m_try],
                     subsample = vctr_subsample[i_subsample],
                     MSE_OOB = CV_model$prediction.error) %>%
          dplyr::bind_rows(CV_result, .)

        message("--- MSE: ", round(CV_model$prediction.error, 10),
                ", Hyperparameter set: [",
                vctr_m_try[i_m_try], ", ", vctr_min_nodesize[i_min_nodesize],
                ", ", vctr_subsample[i_subsample], "]")
        }
      }
    }
    return(CV_result)
  }


#' Predict targeted time series by a random forest model
#'
#' @description `rf_pred()` constructs a random forest model using optimal
#'  hyperparameters previously determined by out-of-bag evaluation to estimate
#'  the targeted time series.
#'
#' @inheritParams rf_fit
#' @param min_nodesize A positive integer indicating the minimal node size (the
#'  minimum number of data points included in each leaf node). This
#'  hyperparameter should be previously optimized by out-of-bag evaluation.
#' @param m_try A positive integer indicating the number of features to be used
#'  in splitting each node. This hyperparameter should be previously optimized
#'  by out-of-bag evaluation.
#' @param subsample A numerical value between 0 and 1, indicating the fraction
#'  of input training data points to be sampled in constructing the random
#'  forest. This hyperparameter should be previously optimized by
#'  out-of-bag evaluation.
#' @param do_outlier_detection A boolean. If `TRUE` (default), this function
#'  predicts the time series to detect outliers; else, this function estimates
#'  the time series to fill gaps.
#' @param coef_iqr A positive value defining a multiplier of the interquartile
#'  range (IQR). If the value to be checked is less than Q1 (first quartile) -
#'  `coef_iqr` * IQR or
#'  more than Q3 (third quartile) + `coef_iqr` * IQR, the value is detected as
#'  an outlier. Default is 1.5.
#'
#' @returns
#' A list with two elements. The first element `mse` is the mean squared error
#' between predicted and original values in the test data set. The second
#' element `stats` is a data frame, and its contents differ depending on
#' `do_outlier_detection`.
#'
#' If `do_outlier_detection` is `TRUE`, the data frame outputs with columns
#' below:
#'
#' * The first column, `cleaned`, gives the cleaned time series after replacing
#'  the detected outliers with the value specified by `label_err`.
#'
#' * The second column, `flag_out`, gives a flag variable time series
#'  indicating the status of the cleaned time series (0: the input data point
#'  is not originally missing and not detected as an outlier; 1: the input data
#'  point is not originally missing but detected as an outlier; 2: the input
#'  data point is originally missing).
#'
#' * The third column, `med`, gives the ensemble median time series calculated
#'  from estimated values at each time point for each tree in the constructed
#'  random forest.
#'
#' * The fourth column, `q1`, gives the ensemble Q1 (first quartile) time
#'  series calculated from estimated values at each time point for each tree in
#'  the constructed random forest.
#'
#' * The fifth column, `q3`, gives the ensemble Q3 (third quartile) time series
#'  calculated from estimated values at each time point for each tree in the
#'  constructed random forest.
#'
#' If `do_outlier_detection` is `FALSE`, the data frame outputs with columns
#' below:
#'
#' * The first column, `gapfilled`, gives the gap-filled time series, where
#'  missing values are replaced with the predicted values from the random
#'  forest model.
#'
#' * The second column, `avg_predicted`, gives the ensemble mean time series
#'  calculated from estimated values at each time point for each tree in the
#'  constructed random forest.
#'
#' * The third column, `sd_predicted`, gives the ensemble mean time series
#'  calculated from estimated values at each time point for each tree in the
#'  constructed random forest.
#'
#' @author Yoshiaki Hata

rf_pred <-
  function(df, colname_label, vctr_colname_feature = NULL,
           min_nodesize, m_try, subsample, do_outlier_detection = TRUE,
           frac_train = 0.75, n_tree = 500, ran_seed = 12345, coef_iqr = 1.5,
           label_err = -9999) {
    ## avoid "No visible binding for global variable" notes
    . <- NULL
    target <- NULL
    avg <- NULL
    SE <- NULL
    target_noisy <- NULL
    flag_out <- NULL
    q1 <- NULL
    q3 <- NULL
    iqr <- NULL
    cleaned <- NULL
    med <- NULL
    target_input <- NULL
    avg_predicted <- NULL
    gapfilled <- NULL
    sd_predicted <- NULL

    ## settings
    set.seed(ran_seed)

    ## check input values
    if(!is.null(vctr_colname_feature) & length(vctr_colname_feature) <= 1) {
      stop("The number of input features must be two or more")
    }

    ## feature name determination
    if(is.null(vctr_colname_feature)) {
      vctr_colname_feature <- colnames(df) %>% .[. != colname_label]
    }

    ## filter data
    data <-
      df %>%
      dplyr::filter(!!rlang::sym(colname_label) != label_err) %>%
      dplyr::na_if(label_err)

    ## create train and test data set
    n_data <- nrow(data)
    rownum_train <- sample(n_data, size = n_data * frac_train)
    train <- data[rownum_train, ]
    test <- data[-rownum_train, ]

    train_feature <-
      train %>%
      dplyr::select(dplyr::all_of(vctr_colname_feature)) %>%
      as.matrix()
    train_target <-
      train %>%
      dplyr::select(dplyr::all_of(colname_label)) %>%
      as.matrix()

    test_feature <-
      test %>%
      dplyr::select(dplyr::all_of(vctr_colname_feature)) %>%
      as.matrix()
    test_target <-
      test %>%
      dplyr::select(dplyr::all_of(colname_label)) %>%
      as.matrix()

    ## construct random forest
    RF_dT <-
      ranger::ranger(x = train_feature,
                     y = train_target,
                     min.node.size = min_nodesize,
                     mtry = m_try,
                     num.trees = n_tree,
                     sample.fraction = subsample,
                     seed = ran_seed)

    MSE_test <-
      stats::predict(RF_dT,
                     data = test_feature,
                     predict.all = FALSE) %>%
      .$predictions %>%
      data.frame(avg = .) %>%
      dplyr::mutate(target = test_target,
                    SE = (target - avg)^2) %>%
      dplyr::summarise(MSE = mean(SE))

    pred_dT <-
      stats::predict(RF_dT,
                     data = df,
                     predict.all = TRUE)

    if(do_outlier_detection == TRUE) {
      df_output <-
        pred_dT$predictions %>%
        as.data.frame() %>%
        dplyr::transmute(med = apply(., MARGIN = 1, FUN = stats::median),
                         q1 = apply(., MARGIN = 1, FUN = stats::quantile,
                                    probs = 0.25),
                         q3 = apply(., MARGIN = 1, FUN = stats::quantile,
                                    probs = 0.75),
                         iqr = q3 - q1)

      df_output$target_noisy <- dplyr::pull(df, !!rlang::sym(colname_label))

      df_output <-
        df_output %>%
        dplyr::mutate(flag_out = ifelse(target_noisy != label_err &
                                          (target_noisy < q1 - coef_iqr * iqr |
                                             target_noisy > q3 + coef_iqr *
                                             iqr),
                                        1, 0),
                      flag_out = ifelse(target_noisy == label_err, 2, flag_out),
                      cleaned = ifelse(flag_out == 1, label_err,
                                       target_noisy)) %>%
        dplyr::select(cleaned, flag_out, med, q1, q3)
    } else {
      df_output <-
        pred_dT$predictions %>%
        as.data.frame() %>%
        dplyr::transmute(avg_predicted = apply(., MARGIN = 1,
                                               FUN = mean),
                         sd_predicted = apply(., MARGIN = 1, FUN = stats::sd))

      df_output$target_input <- dplyr::pull(df, !!rlang::sym(colname_label))

      df_output <-
        df_output %>%
        dplyr::mutate(gapfilled = ifelse(target_input == label_err,
                                         avg_predicted, target_input)) %>%
        dplyr::select(gapfilled, avg_predicted, sd_predicted)
    }
    list(mse = MSE_test, stats = df_output) %>% return()
  }


#' Remove outliers detected by a random forest model
#'
#' @description `remove_rf_outlier()` detects and removes outliers by a random
#'  forest model whose hyperparameters are calibrated using a grid search
#'  approach and out-of-bag evaluation.
#'
#' @details
#' A random forest model is constructed for the targeted time series to remove
#' outliers. The time series is assumed to be stationary, so detrending is
#' needed before inputting if it has a trend. Users can input any feature from
#' the dataset, and out-of-bag evaluation is used to determine the
#' hyperparameters. This evaluation is applied to a training dataset separated
#' from the entire input data. To reduce the computational cost, the only
#' hyperparameter used by default for grid search is the number of candidate
#' features. To reduce the risk of learning noise, the training data sampling
#' ratio is set to 0.1 by default. After determining the optimal
#' hyperparameters, they are used to construct the optimal random forest model.
#' Output values are obtained from 500 (default) trees, and the first quartile
#' (Q1), third quartile (Q3), and interquartile range (IQR) of the output
#' values at each time point are calculated. If the targeted value is less
#' than Q1 − 1.5IQR or more than Q3 + 1.5IQR (default), the data point is
#' identified as an outlier and removed.
#'
#' @inheritParams rf_fit
#' @inheritParams rf_pred
#'
#' @returns
#' A list with two elements. The first element `mse` is the mean squared error
#' between predicted and original values in the test data set. The second
#' element `stats` is a data frame with columns below:
#'
#' * The first column, `cleaned`, gives the cleaned time series after replacing
#'  the detected outliers with the value specified by `label_err`.
#'
#' * The second column, `flag_out`, gives a flag variable time series
#'  indicating the status of the cleaned time series (0: the input data point
#'  is not originally missing and not detected as an outlier; 1: the input data
#'  point is not originally missing but detected as an outlier; 2: the input
#'  data point is originally missing).
#'
#' * The third column, `med`, gives the ensemble median time series calculated
#'  from estimated values at each time point for each tree in the constructed
#'  random forest.
#'
#' * The fourth column, `q1`, gives the ensemble Q1 (first quartile) time
#'  series calculated from estimated values at each time point for each tree
#'  in the constructed random forest.
#'
#' * The fifth column, `q3`, gives the ensemble Q3 (third quartile) time series
#'  calculated from estimated values at each time point for each tree in the
#'  constructed random forest.
#'
#' @examples
#' ## Load data
#' data(dt_noisy)
#' df_raw <- dt_noisy[c(12097:14400), ]
#'
#' ## Remove outliers
#' result <-
#'   remove_rf_outlier(df = df_raw, colname_label = "dt",
#'                     vctr_colname_feature = c("sw_in", "vpd", "swc", "p"),
#'                     coef_iqr = 3.0)$stats
#'
#' @author Yoshiaki Hata
#'
#' @export

remove_rf_outlier <-
  function(df, colname_label, vctr_colname_feature = NULL,
           vctr_min_nodesize = c(5), vctr_m_try = NULL,
           vctr_subsample = c(0.1), frac_train = 0.75,
           n_tree = 500, ran_seed = 12345, coef_iqr = 1.5, label_err = -9999) {
    ## avoid "No visible binding for global variable" notes
    . <- NULL
    MSE_OOB <- NULL
    min_nodesize <- NULL
    m_try <- NULL
    subsample <- NULL

    message("Random forest outlier detection started")
    message("--- Hyperparameter optimization using grid search started")

    CV_rslt <-
      rf_fit(df, colname_label, vctr_colname_feature,
             vctr_min_nodesize, vctr_m_try, vctr_subsample,
             frac_train = frac_train, n_tree = n_tree,
             ran_seed = ran_seed, label_err = label_err) %>%
      dplyr::arrange(MSE_OOB) %>%
      dplyr::slice_head(.)

    min_nodesize_opt <- dplyr::pull(CV_rslt, min_nodesize)
    m_try_opt <- dplyr::pull(CV_rslt, m_try)
    subsample_opt <- dplyr::pull(CV_rslt, subsample)

    message("--- Optimal hyperparameter set: [", m_try_opt, ", ",
            min_nodesize_opt, ", ", subsample_opt, "]")
    message("--- Hyperparameter optimization using grid search finished")
    message("--- Random forest construction started")

    result <-
      rf_pred(df, colname_label, vctr_colname_feature,
              min_nodesize = min_nodesize_opt,
              m_try = m_try_opt, subsample = subsample_opt,
              do_outlier_detection = TRUE,
              frac_train = frac_train, n_tree = n_tree,
              coef_iqr = coef_iqr, label_err = label_err)

    message("--- Random forest construction finished")
    message("Random forest outlier detection finished")
    return(result)
  }


#' Fill missing values with a random forest model
#'
#' @description `fill_gaps()` replaces all missing values in a target time
#'  series with values estimated by a random forest model whose hyperparameters
#'  are calibrated using a grid search approach and out-of-bag evaluation.
#'
#' @details
#' A random forest model is constructed for the targeted time series to fill
#' missing values. The time series is assumed to be stationary, so detrending
#' is needed before inputting if it has a trend. Users can input any feature
#' from the dataset, and out-of-bag evaluation is used to determine the
#' hyperparameters. This evaluation is applied to a training dataset separated
#' from the entire input data. To reduce the computational cost, the only
#' hyperparameter used by default for grid search is the number of candidate
#' features. After determining the optimal hyperparameters, they are used to
#' construct the optimal random forest model. Predicted time series are equal
#' to the average of 500 (default) tree outputs at each time point. If the
#' input targeted value is missing, the predicted value is used for the
#' imputation.
#'
#' @inheritParams rf_fit
#' @inheritParams rf_pred
#' @param vctr_subsample A vector of numerical values between 0 and 1,
#'  indicating candidates of a hyperparameter defining the fraction of input
#'  training data points to be sampled in constructing the random forest.
#'  Default is `c(1)`.
#'
#' @returns
#' A list with two elements. The first element `mse` is the mean squared error
#' between predicted and original values in the test data set. The second
#' element `stats` is a data frame with columns below:
#'
#' * The first column, `gapfilled`, gives the gap-filled time series, where
#'  missing values are replaced with the predicted values from the random
#'  forest model.
#'
#' * The second column, `avg_predicted`, gives the ensemble mean time series
#'  calculated from estimated values at each time point for each tree in the
#'  constructed random forest.
#'
#' * The third column, `sd_predicted`, gives the ensemble mean time series
#'  calculated from estimated values at each time point for each tree in the
#'  constructed random forest.
#'
#' @examples
#' ## Load data
#' data(dt_noisy)
#' df_raw <- dt_noisy[c(13105:15024), ]
#'
#' ## Remove error values for making data gaps
#' df_raw$dt <- ifelse(df_raw$dt > 9.5, df_raw$dt, -9999)
#'
#' ## Fill data gaps
#' result <-
#'   fill_gaps(df = df_raw, colname_label = "dt",
#'             vctr_colname_feature = c("sw_in", "vpd", "swc", "ta"))$stats
#'
#' @author Yoshiaki Hata
#'
#' @export

fill_gaps <-
  function(df, colname_label, vctr_colname_feature = NULL,
           vctr_min_nodesize = c(5), vctr_m_try = NULL,
           vctr_subsample = c(1), frac_train = 0.75,
           n_tree = 500, ran_seed = 12345, label_err = -9999) {
    ## avoid "No visible binding for global variable" notes
    . <- NULL
    MSE_OOB <- NULL
    min_nodesize <- NULL
    m_try <- NULL
    subsample <- NULL

    message("Random forest-based gap-filling started")
    message("--- Hyperparameter optimization using grid search started")

    CV_rslt <-
      rf_fit(df, colname_label, vctr_colname_feature,
             vctr_min_nodesize, vctr_m_try, vctr_subsample,
             frac_train = frac_train, n_tree = n_tree,
             ran_seed = ran_seed, label_err = label_err) %>%
      dplyr::arrange(MSE_OOB) %>%
      dplyr::slice_head(.)

    min_nodesize_opt <- dplyr::pull(CV_rslt, min_nodesize)
    m_try_opt <- dplyr::pull(CV_rslt, m_try)
    subsample_opt <- dplyr::pull(CV_rslt, subsample)

    message("--- Optimal hyperparameter set: [", m_try_opt, ", ",
            min_nodesize_opt, ", ", subsample_opt, "]")
    message("--- Hyperparameter optimization using grid search finished")
    message("--- Random forest construction started")

    result <-
      rf_pred(df, colname_label, vctr_colname_feature,
              min_nodesize = min_nodesize_opt,
              m_try = m_try_opt,
              subsample = subsample_opt,
              do_outlier_detection = FALSE,
              frac_train = frac_train, n_tree = n_tree,
              ran_seed = ran_seed, label_err = label_err)

    message("--- Random forest construction finished")
    message("Random forest-based gap-filling finished")
    return(result)
  }

