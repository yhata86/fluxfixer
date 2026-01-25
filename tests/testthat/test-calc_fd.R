test_that("calculates right sap flow rate", {
  dt_test <- c(10, 12)
  dtmax_test <- c(11)

  expect_equal(calc_fd(dt_test[1], dtmax_test,
                       alpha = 1.19 * 10^{-4}, beta = 1.231,
                       do_heartwood_correction = FALSE,
                       ratio_conductive = NULL),
               1.19 * 10^{-4} * 0.1^1.231)

  expect_equal(calc_fd(dt_test[2], dtmax_test,
                       alpha = 1.19 * 10^{-4}, beta = 1.231,
                       do_heartwood_correction = FALSE,
                       ratio_conductive = NULL),
               0)

  expect_equal(calc_fd(dt_test[1], dtmax_test,
                       alpha = 1.19 * 10^{-4}, beta = 1.231,
                       do_heartwood_correction = TRUE,
                       ratio_conductive = 0.2),
               1.19 * 10^{-4} * (5 / 6)^1.231)
})
