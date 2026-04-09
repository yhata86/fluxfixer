test_that("gets right time interval", {
  timezone <- "Etc/GMT-8"

  time_test <- seq(as.POSIXct("2025/01/01 00:30", tz = timezone),
                   as.POSIXct("2025/01/02 00:00", tz = timezone),
                   by = "30 min")

  expect_equal(get_interval(time_test), 30)

  expect_error(get_interval(c(time_test, NA)), "one or more NA values exist")
  expect_error(get_interval(c(time_test,
                              as.POSIXct("2025/01/02 01:00", tz = timezone))),
               "does not have equal intervals")
})


test_that("gets right TOA global radiation", {
  timezone <- "Etc/GMT-8"

  expect_equal(calc_sw_in_toa(as.POSIXct("2025/01/01 07:00", tz = timezone),
                                         35, 140, 135), 0)
})


test_that("gets the right number of data points without missing values", {
  vctr_test <- c(1, -9999, 3, NA, 5)
  expect_equal(n_valid(vctr_test), 3)
})


test_that("gets the right number of short attenuation periods", {
  timezone <- "Etc/GMT-8"

  time_test <- seq(as.POSIXct("2025/01/01 00:30", tz = timezone),
                   as.POSIXct("2025/02/01 00:00", tz = timezone),
                   by = "30 min")

  set.seed(12345)

  z_test <- rep(0, length(time_test))

  avg_test <-
    rep(11, length(time_test)) +
    c(rep(0, 100),
      seq(0, -1, by = -0.1),
      seq(-1, 5, by = 0.1),
      seq(5, -1, by = -0.1),
      seq(-1, 0, by = 0.1),
      rep(0, length(time_test) - 244))

  sd_test <-
    rep(1, length(time_test)) +
    c(rep(0, 100),
      seq(0, 0.1, by = 0.01),
      seq(0.1, -0.5, by = -0.01),
      seq(-0.5, 0.1, by = 0.01),
      seq(0.1, 0, by = -0.01),
      rep(0, length(time_test) - 244))

  rslt_test <-
    check_short_attenuation(time_test, z_test, avg_test, sd_test,
                            wndw_size_conv = 48 * 1)

  expect_equal(nrow(rslt_test), 1)
})

test_that("gets right QC flag interpretation", {
  rslt_test <- interpret_qc(c(0, 1, 3, 7, 15, 31, 63, 127, 255, 511, 1023))

  expect_equal(rslt_test$initial_na,
               c(0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1))
  expect_equal(rslt_test$manual_removal,
               c(0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1))
  expect_equal(rslt_test$absolute_limits,
               c(0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1))
  expect_equal(rslt_test$drift_correction,
               c(0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1))
  expect_equal(rslt_test$noise_filtering,
               c(0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1))
  expect_equal(rslt_test$z_score_outlier,
               c(0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1))
  expect_equal(rslt_test$rf_outlier,
               c(0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1))
  expect_equal(rslt_test$gap_filling,
               c(0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1))
  expect_equal(rslt_test$detrending,
               c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1))
  expect_equal(rslt_test$damping_correction,
               c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1))
})
