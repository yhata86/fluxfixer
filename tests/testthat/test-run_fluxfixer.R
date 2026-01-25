test_that("automated process works", {
  data("dt_noisy")

  time_drft_head_test <- as.POSIXct("2013/05/14 13:30", tz = "Etc/GMT-8")
  time_drft_tail_test <- as.POSIXct("2013/05/17 15:00", tz = "Etc/GMT-8")
  time_prd_tail_test <- as.POSIXct("2013/05/14 13:00", tz = "Etc/GMT-8")

  df <-
    run_fluxfixer(dt_noisy, colname_time = "time", colname_target = "dt",
                  vctr_time_drft_head = time_drft_head_test,
                  vctr_time_drft_tail = time_drft_tail_test,
                  vctr_time_prd_tail = time_prd_tail_test,
                  detrend = TRUE, correct_damping = FALSE)

  expect_gt(min(df$processed), 8)
  expect_lt(max(df$processed), 14)
})
