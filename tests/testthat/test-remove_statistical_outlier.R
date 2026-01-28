test_that("manual outlier removal works", {
  time_test <-
    seq(as.POSIXct("2025/01/01"), as.POSIXct("2025/01/05"),
        by = "1 day")

  target_test <- seq(1, 5)
  time_err_test <- c(as.POSIXct("2025/01/01"), as.POSIXct("2025/01/03"))

  expect_equal(remove_manually(time_test, target_test, time_err_test),
               c(-9999, 2, -9999, 4, 5))
})

test_that("screening outliers by absolute limits works", {
  target_test <- c(0, -9999, 2, 3, 100, 5)

  expect_equal(check_absolute_limits(target_test, 2, 5),
               c(-9999, -9999, 2, 3, -9999, 5))
})

test_that("modifying short-term drifts works", {
  time_test <-
    seq(as.POSIXct("2025/01/01"), as.POSIXct("2025/10/27"),
        by = "1 day")
  target_test <- c(seq(1, 100), seq(1001, 1100), seq(1, 100))
  time_drft_head_test <- as.POSIXct("2025/04/11")
  time_drft_tail_test <- as.POSIXct("2025/07/19")

  corrected <-
    modify_short_drift(time_test, target_test, time_drft_head_test,
                       time_drft_tail_test, n_day_ref = 100)

  expect_lt(abs(corrected[101] - 1), 0.1)
})


test_that("filtering high frequncy noise works", {
  set.seed(123)
  time_test <- seq(as.POSIXct("2025/01/01"), length.out = 360, by = "1 day")
  x_test <- seq(1, 360)
  target_test <- sin(x_test / 180 * pi) + rnorm(length(x_test), sd = 0.2)
  target_test[c(seq(1, 5), seq(10, 30), seq(240, 250))] <- -9999
  vctr_time_noise_test <- seq(as.POSIXct("2025/01/01"),
                              as.POSIXct("2025/09/01"), by = "1 day")

  target_filtered <-
    filter_highfreq_noise(time_test, target_test, vctr_time_noise_test,
                          wndw_size_noise = 20)

  ## missing values remain
  expect_equal(target_filtered[20], -9999)
  ## noise is filtered
  expect_equal(round(target_filtered[180], digits = 2), 0)
  ## values outside the indicated period remain
  expect_equal(target_filtered[251], target_test[251])
})


test_that("Z-score outlier removal works", {
  time_test <- seq(as.POSIXct("2025/01/01 00:30"), length.out = 6 * 360,
                   by = "30 min")
  x_test <- seq(1, 3 * 360)
  target_test <- sin(x_test / 180 * pi)
  target_test <- c(target_test, 0.2 * sin(x_test / 180 * pi))
  target_test[c(seq(101, 200), seq(1601, 1800))] <- -9999
  target_test[c(300, 500, 1000)] <- 3.9
  target_test[c(1900, 2000, 2100)] <- 0.8
  time_prd_tail_test <- as.POSIXct("2025/01/23 12:00")

  target_filtered <-
    remove_zscore_outlier(time_test, target_test,
                          vctr_time_prd_tail = time_prd_tail_test)

  ## missing values remain
  expect_equal(target_filtered$z_cleaned[101], -9999)
  expect_equal(target_filtered$z_cleaned[1800], -9999)
  ## spikes are filtered
  expect_equal(target_filtered$z_cleaned[300], -9999)
  expect_equal(target_filtered$z_cleaned[500], -9999)
  expect_equal(target_filtered$z_cleaned[1000], -9999)
  expect_equal(target_filtered$z_cleaned[1900], -9999)
  expect_equal(target_filtered$z_cleaned[2000], -9999)
  expect_equal(target_filtered$z_cleaned[2100], -9999)
})


test_that("reference average and sd calculation works", {
  time_test <- seq(as.POSIXct("2025/01/01 00:30"), length.out = 48 * 30,
                   by = "30 min")
  target_test <- c(rep(seq(1, 3), each = 48 * 5), rep(seq(3, 5), each = 48 * 5))
  time_prd_tail_test <- as.POSIXct("2025/01/16 00:00")
  vctr_ref <- calc_ref_stats(time_test, target_test, time_prd_tail_test)

  expect_equal(vctr_ref[1], 3)
  expect_equal(round(vctr_ref[2], 3), 0.817)
})


test_that("time series retrieval works", {
  target_test <- seq(1, 100)
  target_avg <- rep(mean(target_test), 100)
  target_sd <- rep(stats::sd(target_test), 100)
  target_z <- (target_test - target_avg) / target_sd

  vctr_retrieval <- retrieve_ts(target_z, target_avg, target_sd)

  expect_equal(min(vctr_retrieval), 1)
  expect_equal(max(vctr_retrieval), 100)
})
