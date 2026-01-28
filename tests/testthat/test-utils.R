test_that("gets right time interval", {
  time_test <- seq(lubridate::ymd_hm("2025/01/01 00:30"),
                   lubridate::ymd_hm("2025/01/02 00:00"),
                   by = "30 min")

  expect_equal(get_interval(time_test), 30)

  expect_error(get_interval(c(time_test, NA)), "one or more NA values exist")
  expect_error(get_interval(c(time_test,
                              lubridate::ymd_hm("2025/01/02 01:00"))),
               "does not have equal intervals")
})


test_that("gets right TOA global radiation", {
  expect_equal(calc_sw_in_toa(lubridate::ymd_hm("2025/01/01 07:00"),
                                         35, 140, 135), 0)
})


test_that("gets the right number of data points without missing values", {
  vctr_test <- c(1, -9999, 3, NA, 5)
  expect_equal(n_valid(vctr_test), 3)
})


test_that("gets the right number of short attenuation periods", {
  time_test <- seq(lubridate::ymd_hm("2025/01/01 00:30"),
                   lubridate::ymd_hm("2025/02/01 00:00"),
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

