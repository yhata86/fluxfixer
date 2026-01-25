test_that("calculates right dtmax time series", {
  data("dt_gf")
  data("dtmax")
  dtmax_sp_ref = dtmax$dtmax_sp
  dtmax_pd_ref = dtmax$dtmax_pd
  dtmax_ed_ref = dtmax$dtmax_ed
  dtmax_mw_ref = dtmax$dtmax_mw
  dtmax_dr_ref = dtmax$dtmax_dr

  time_test <- dt_gf$time
  dt_test <- dt_gf$dt
  sw_in_test <- dt_gf$sw_in
  ta_test <- dt_gf$ta
  vpd_test <- dt_gf$vpd

  rslt_test <-
    calc_dtmax(time_test, dt_test, sw_in_test, ta_test, vpd_test,
               method = c("sp", "pd", "mw", "dr", "ed"),
               thres_vpd = 6.0, output_daily = FALSE)

  expect_equal(median(dtmax_sp_ref - rslt_test$dtmax_sp), 0)
  expect_equal(median(dtmax_pd_ref - rslt_test$dtmax_pd), 0)
  expect_equal(median(dtmax_mw_ref - rslt_test$dtmax_mw), 0)
  expect_equal(median(dtmax_dr_ref - rslt_test$dtmax_dr), 0)
  expect_equal(median(dtmax_ed_ref - rslt_test$dtmax_ed), 0)
})
