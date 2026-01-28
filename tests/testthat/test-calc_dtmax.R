test_that("each dtmax calculation functions work", {
  data("dt_gf")
  data("dtmax")

  dtmax_sp_ref = dtmax$dtmax_sp[(1 + 48 * 30) : (1 + 48 * 60)]
  dtmax_pd_ref = dtmax$dtmax_pd[(1 + 48 * 30) : (1 + 48 * 60)]
  dtmax_ed_ref = dtmax$dtmax_ed[(1 + 48 * 30) : (1 + 48 * 60)]
  dtmax_mw_ref = dtmax$dtmax_mw[(1 + 48 * 30) : (1 + 48 * 60)]
  dtmax_dr_ref = dtmax$dtmax_dr[(1 + 48 * 30) : (1 + 48 * 60)]

  time_test <- dt_gf$time[(1 + 48 * 30) : (1 + 48 * 60)]
  dt_test <- dt_gf$dt[(1 + 48 * 30) : (1 + 48 * 60)]
  sw_in_test <- dt_gf$sw_in[(1 + 48 * 30) : (1 + 48 * 60)]
  ta_test <- dt_gf$ta[(1 + 48 * 30) : (1 + 48 * 60)]
  vpd_test <- dt_gf$vpd[(1 + 48 * 30) : (1 + 48 * 60)]


  rslt_test_sp <- calc_dtmax_sp(time_test, dt_test, output_daily = FALSE)

  rslt_test_pd <-
    calc_dtmax_pd(time_test, dt_test, sw_in_test, output_daily = FALSE)

  rslt_test_pd <-
    calc_dtmax_pd(time_test, dt_test, sw_in_test, output_daily = FALSE)

  rslt_test_ed <-
    calc_dtmax_ed(time_test, dt_test, sw_in_test, ta_test, vpd_test,
                  thres_vpd = 6.0, output_daily = FALSE)

  rslt_test_pd_ed <-
    calc_dtmax_pd_ed(time_test, dt_test, sw_in_test, ta_test, vpd_test,
                     thres_vpd = 6.0, output_daily = FALSE)

  rslt_test_mw <-
    calc_dtmax_mw(vctr_time = time_test, vctr_dt = dt_test,
                  vctr_radi = sw_in_test, output_daily = FALSE)

  rslt_test_dr <-
    calc_dtmax_dr(vctr_time = time_test, vctr_dt = dt_test,
                  vctr_radi = sw_in_test, output_daily = FALSE)

  rslt_test_mw_dr <-
    calc_dtmax_mw_dr(vctr_time = time_test, vctr_dt = dt_test,
                     vctr_radi = sw_in_test, output_daily = FALSE)

  expect_lt(abs(median(dtmax_sp_ref - rslt_test_sp$dtmax_sp)), 0.01)
  expect_lt(abs(median(dtmax_pd_ref - rslt_test_pd$dtmax_pd)), 0.01)
  expect_lt(abs(median(dtmax_ed_ref - rslt_test_ed$dtmax_ed)), 0.01)
  expect_lt(abs(median(dtmax_pd_ref - rslt_test_pd_ed$dtmax_pd)), 0.01)
  expect_lt(abs(median(dtmax_ed_ref - rslt_test_pd_ed$dtmax_ed)), 0.01)
  expect_lt(abs(median(dtmax_mw_ref - rslt_test_mw$dtmax_mw)), 0.01)
  expect_lt(abs(median(dtmax_dr_ref - rslt_test_dr$dtmax_dr)), 0.01)
  expect_lt(abs(median(dtmax_mw_ref - rslt_test_mw_dr$dtmax_mw)), 0.01)
  expect_lt(abs(median(dtmax_dr_ref - rslt_test_mw_dr$dtmax_dr)), 0.01)
})


test_that("calculates right dtmax time series", {
  data("dt_gf")
  data("dtmax")
  dtmax_sp_ref = dtmax$dtmax_sp[(1 + 48 * 30) : (1 + 48 * 60)]
  dtmax_pd_ref = dtmax$dtmax_pd[(1 + 48 * 30) : (1 + 48 * 60)]
  dtmax_ed_ref = dtmax$dtmax_ed[(1 + 48 * 30) : (1 + 48 * 60)]
  dtmax_mw_ref = dtmax$dtmax_mw[(1 + 48 * 30) : (1 + 48 * 60)]
  dtmax_dr_ref = dtmax$dtmax_dr[(1 + 48 * 30) : (1 + 48 * 60)]

  time_test <- dt_gf$time[(1 + 48 * 30) : (1 + 48 * 60)]
  dt_test <- dt_gf$dt[(1 + 48 * 30) : (1 + 48 * 60)]
  sw_in_test <- dt_gf$sw_in[(1 + 48 * 30) : (1 + 48 * 60)]
  ta_test <- dt_gf$ta[(1 + 48 * 30) : (1 + 48 * 60)]
  vpd_test <- dt_gf$vpd[(1 + 48 * 30) : (1 + 48 * 60)]

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
