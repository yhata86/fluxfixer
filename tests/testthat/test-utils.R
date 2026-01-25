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

