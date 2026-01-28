test_that("rf_fit and rf_pred functions work", {
  data("dt_gf")

  vctr_feature_test <- c("sw_in", "vpd", "swc")

  rslt_test_fit <-
    rf_fit(dt_gf, "dt", vctr_colname_feature = vctr_feature_test)

  rslt_test_pred <-
    rf_pred(dt_gf, "dt", vctr_colname_feature = vctr_feature_test,
            min_nodesize = 5, m_try = 3, subsample = 0.1,
            do_outlier_detection = TRUE)

  expect_lt(min(rslt_test_fit$MSE_OOB), 0.2)
  expect_equal(sum(rslt_test_pred$flag_out), 0)
})


test_that("removes appropriate random forest outlier", {
  data("dt_gf")

  vctr_feature_test <- c("sw_in", "vpd", "swc")

  ## create outliers
  dt_gf$dt[1001] <- 15.0
  dt_gf$dt[3001] <- 6.0

  rslt_test <-
    remove_rf_outlier(dt_gf, "dt", vctr_colname_feature = vctr_feature_test)

  expect_equal(rslt_test$stats$flag_out[1001], 1)
  expect_equal(rslt_test$stats$flag_out[3001], 1)
})


test_that("fills gaps appropriately by random forest model", {
  data("dt_gf")
  vctr_feature_test <- c("sw_in", "vpd", "swc")

  ## store original values
  org_1 <- dt_gf$dt[1001]
  org_2 <- dt_gf$dt[3001]

  ## create gaps
  dt_gf$dt[1001] <- -9999
  dt_gf$dt[3001] <- -9999

  rslt_test <- fill_gaps(dt_gf, "dt", vctr_colname_feature = vctr_feature_test)

  expect_lt(abs(rslt_test$stats$avg[1001] - org_1), 0.61)
  expect_lt(abs(rslt_test$stats$avg[3001] - org_2), 0.61)
})
