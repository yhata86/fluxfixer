## code to prepare `DATASET` dataset goes here

# settings ----------------------------------------------------------------

library(data.table)
library(dplyr)
library(lubridate)
library(magrittr)
library(stringr)
library(tidyr)
library(zoo)

timezone <- "Etc/GMT-8"


# import dirty dT (La1) at Lambir Hills NP --------------------------------

paths_input <-
  list.files(path = "../../../LHP/LHP_meteoflux/input_data",　full.names = TRUE)
paths_input %<>% str_detect("sapflow_30min_La1") %>% paths_input[.]
data_dT_La1 <- data.frame(NULL)

colname_La1 <-
  fread(paths_input[1], header = FALSE, data.table = FALSE, nrows = 1) %>%
  as.character()

for (i in 1:length(paths_input)) {
  data_dT_La1 <-
    fread(paths_input[i], header = FALSE, data.table = FALSE, skip = 2,
          col.names = colname_La1) %>%
    bind_rows(data_dT_La1, .)
}

data_dT_La1 %<>% mutate(time = ymd_hm(time, tz = timezone))


# import gap-filled dT (La1) at Lambir Hills NP ---------------------------

data_dT_La1_01_gf <-
  fread("../../../LHP/LHP_meteoflux/output_data/tmp_dT_La1_gf.csv",
        header = TRUE, data.table = FALSE) %>%
  mutate(time = ymd_hms(time, tz = timezone))


# import dTmax (La1) calculated by various ways ---------------------------

data_dTmax_La1_01 <-
  fread("../../../LHP/LHP_meteoflux/output_data/tmp_dTmax_La1.csv",
        header = TRUE, data.table = FALSE) %>%
  mutate(time = ymd_hms(time, tz = timezone))


# import meteo data at Lambir Hills NP ------------------------------------

path_input <- "../../../LHP/LHP_meteoflux/output_data/"
csv_tail <- "0112LHP_ver01.csv"

colname_meteoflux <-
  c("year", "month", "day", "hour", "minute",
    "PPT", "QC_PPT", "Rs_in", "QC_Rs_in", "Rl_in", "QC_Rl_in",
    "PPFD_in", "QC_PPFD_in",
    "Ta_76m", "QC_Ta_76m", "Ta_60m", "QC_Ta_60m",
    "ea_76m", "QC_ea_76m", "ea_60m", "QC_ea_60m",
    "VPD_76m", "QC_VPD_76m", "VPD_60m", "QC_VPD_60m",
    "WS", "QC_WS", "WD", "QC_WD",
    "Pa", "QC_Pa", "CO2_mf", "QC_CO2_mf", "USTAR", "QC_USTAR",
    "H_F_MDS", "H_CORR", "QC_H", "LE_F_MDS", "LE_CORR", "ET_CORR", "QC_LE",
    "NEE_VUT_REF", "QC_NEE_VUT_REF",
    "GPP_NT_VUT_REF", "QC_GPP_NT_VUT_REF",
    "GPP_DT_VUT_REF", "QC_GPP_DT_VUT_REF",
    "RECO_NT_VUT_REF", "QC_RECO_NT_VUT_REF",
    "RECO_DT_VUT_REF", "QC_RECO_DT_VUT_REF")


data_meteoflux <- fread(paste0(path_input, "20091112LHP_ver01.csv"),
                        header = TRUE, data.table = FALSE,
                        col.names = colname_meteoflux)

for (i in 2010:2019) {
  data_meteoflux <-
    fread(paste0(path_input, i, csv_tail), header = TRUE, data.table = FALSE,
          col.names = colname_meteoflux) %>%
    bind_rows(data_meteoflux, .)
}

data_meteoflux %<>%
  mutate(time = paste0(year, "/", month, "/", day, " ", hour, ":", minute) %>%
           ymd_hm(., tz = timezone),
         time_lag = time - 1800)


# import SWC half-hourly data at Lambir Hills NP -------------------------

paths_input <-
  list.files(path =
               "../../../LHP/LHP_meteoflux/output_data/for_JapanFlux_Database",
             full.names = TRUE)
paths_input %<>% str_detect("ver01") %>% paths_input[.]
data_SWC_30min <- data.frame(NULL)

for (i in 1:length(paths_input)) {
  data_SWC_30min <-
    fread(paths_input[i], header = TRUE, data.table = FALSE) %>%
    mutate(time = ymd_hm(TIMESTAMP_END, tz = timezone)) %>%
    select(time, starts_with("SWC")) %>%
    bind_rows(data_SWC_30min, .)
}

colname_SWC_30min <-
  c("time",
    "SWC_10_La1", "SWC_30_La1", "SWC_60_La1", "SWC_100_La1",
    "SWC_10_La2", "SWC_30_La2", "SWC_60_La2", "SWC_100_La2",
    "SWC_10_La3", "SWC_30_La3", "SWC_60_La3", "SWC_100_La3")

names(data_SWC_30min) <- colname_SWC_30min

## unit conversion (% to m+3m-3)
data_SWC_30min %<>%
  mutate(SWC_10_La1 = ifelse(SWC_10_La1 > -9999, SWC_10_La1 * 0.01, -9999),
         SWC_30_La1 = ifelse(SWC_30_La1 > -9999, SWC_30_La1 * 0.01, -9999),
         SWC_60_La1 = ifelse(SWC_60_La1 > -9999, SWC_60_La1 * 0.01, -9999),
         SWC_100_La1 = ifelse(SWC_100_La1 > -9999, SWC_100_La1 * 0.01, -9999),
         SWC_10_La2 = ifelse(SWC_10_La2 > -9999, SWC_10_La2 * 0.01, -9999),
         SWC_30_La2 = ifelse(SWC_30_La2 > -9999, SWC_30_La2 * 0.01, -9999),
         SWC_60_La2 = ifelse(SWC_60_La2 > -9999, SWC_60_La2 * 0.01, -9999),
         SWC_100_La2 = ifelse(SWC_100_La2 > -9999, SWC_100_La2 * 0.01, -9999),
         SWC_10_La3 = ifelse(SWC_10_La3 > -9999, SWC_10_La3 * 0.01, -9999),
         SWC_30_La3 = ifelse(SWC_30_La3 > -9999, SWC_30_La3 * 0.01, -9999),
         SWC_60_La3 = ifelse(SWC_60_La3 > -9999, SWC_60_La3 * 0.01, -9999),
         SWC_100_La3 = ifelse(SWC_100_La3 > -9999, SWC_100_La3 * 0.01, -9999))

## calculate weighted average
weight_120 <- c(20, 25, 35, 40)
weight_50 <- c(20, 25, 5)

data_SWC_30min %<>%
  mutate(SWC_120_La1 = (weight_120[1] * SWC_10_La1 +
                          weight_120[2] * SWC_30_La1 +
                          weight_120[3] * SWC_60_La1 +
                          weight_120[4] * SWC_100_La1) / sum(weight_120),
         SWC_120_La1 = ifelse(SWC_120_La1 < 0, -9999, SWC_120_La1),
         SWC_120_La2 = (weight_120[1] * SWC_10_La2 +
                          weight_120[2] * SWC_30_La2 +
                          weight_120[3] * SWC_60_La2 +
                          weight_120[4] * SWC_100_La2) / sum(weight_120),
         SWC_120_La2 = ifelse(SWC_120_La2 < 0, -9999, SWC_120_La2),
         SWC_120_La3 = (weight_120[1] * SWC_10_La3 +
                          weight_120[2] * SWC_30_La3 +
                          weight_120[3] * SWC_60_La3 +
                          weight_120[4] * SWC_100_La3) / sum(weight_120),
         SWC_120_La3 = ifelse(SWC_120_La3 < 0, -9999, SWC_120_La3),
         SWC_50_La1 = (weight_50[1] * SWC_10_La1 +
                         weight_50[2] * SWC_30_La1 +
                         weight_50[3] * SWC_60_La1) / sum(weight_50),
         SWC_50_La1 = ifelse(SWC_50_La1 < 0, -9999, SWC_50_La1),
         SWC_50_La2 = (weight_50[1] * SWC_10_La2 +
                         weight_50[2] * SWC_30_La2 +
                         weight_50[3] * SWC_60_La2) / sum(weight_50),
         SWC_50_La2 = ifelse(SWC_50_La2 < 0, -9999, SWC_50_La2),
         SWC_50_La3 = (weight_50[1] * SWC_10_La3 +
                         weight_50[2] * SWC_30_La3 +
                         weight_50[3] * SWC_60_La3) / sum(weight_50),
         SWC_50_La3 = ifelse(SWC_50_La3 < 0, -9999, SWC_50_La3))


## import SWC daily values
colname_SWC_daily <-
  c("year", "month", "day",
    "SWC_120_La1_calc", "QC_SWC_120_La1_calc",
    "SWC_120_La2_calc", "QC_SWC_120_La2_calc",
    "SWC_120_La3_calc", "QC_SWC_120_La3_calc",
    "SWC_50_La1_calc", "QC_SWC_50_La1_calc",
    "SWC_50_La2_calc", "QC_SWC_50_La2_calc",
    "SWC_50_La3_calc", "QC_SWC_50_La3_calc")

data_SWC_daily <-
  fread("../../../LHP/LHP_meteoflux/output_data/200911_201912LHP_swc_ver01.csv",
        header = TRUE, data.table = FALSE, col.names = colname_SWC_daily) %>%
  mutate(date = paste(year, month, day) %>% ymd(., tz = timezone),
         date_0900 = date + hours(9))

## gap-filling by daily values
data_SWC_30min <-
  data_SWC_daily %>%
  select(date_0900, starts_with("SWC")) %>%
  merge.data.frame(data_SWC_30min, ., by.x = "time", by.y = "date_0900",
                   all = TRUE)

data_SWC_30min[is.na(data_SWC_30min)] <- -9999

data_SWC_30min %<>%
  mutate(SWC_120_La1_mod1 = ifelse(SWC_120_La1 == -9999 &
                                     SWC_120_La1_calc > -9999,
                                   SWC_120_La1_calc, SWC_120_La1),
         SWC_120_La2_mod1 = ifelse(SWC_120_La2 == -9999 &
                                     SWC_120_La2_calc > -9999,
                                   SWC_120_La2_calc, SWC_120_La2),
         SWC_120_La3_mod1 = ifelse(SWC_120_La3 == -9999 &
                                     SWC_120_La3_calc > -9999,
                                   SWC_120_La3_calc, SWC_120_La3),
         SWC_50_La1_mod1 = ifelse(SWC_50_La1 == -9999 & SWC_50_La1_calc > -9999,
                                  SWC_50_La1_calc, SWC_50_La1),
         SWC_50_La2_mod1 = ifelse(SWC_50_La2 == -9999 & SWC_50_La2_calc > -9999,
                                  SWC_50_La2_calc, SWC_50_La2),
         SWC_50_La3_mod1 = ifelse(SWC_50_La3 == -9999 & SWC_50_La3_calc > -9999,
                                  SWC_50_La3_calc, SWC_50_La3))

## gap-filling by linear interpolation
data_SWC_30min %<>%
  na_if(-9999) %>%
  mutate(SWC_120_La1_mod2 = na.approx(SWC_120_La1_mod1, na.rm = FALSE),
         SWC_120_La2_mod2 = na.approx(SWC_120_La2_mod1, na.rm = FALSE),
         SWC_120_La3_mod2 = na.approx(SWC_120_La3_mod1, na.rm = FALSE),
         SWC_50_La1_mod2 = na.approx(SWC_50_La1_mod1, na.rm = FALSE),
         SWC_50_La2_mod2 = na.approx(SWC_50_La2_mod1, na.rm = FALSE),
         SWC_50_La3_mod2 = na.approx(SWC_50_La3_mod1, na.rm = FALSE)) %>%
  fill(c(SWC_120_La1_mod2, SWC_120_La2_mod2, SWC_120_La3_mod2,
         SWC_50_La1_mod2, SWC_50_La2_mod2, SWC_50_La3_mod2),
       .direction = "updown")

data_SWC_30min[is.na(data_SWC_30min)] <- -9999


# arrange data set --------------------------------------------------------

sensor_target <- "La1_01"
dT_target <- paste0("dT_", sensor_target)

# time_head_mea <- ymd_hm("2012/09/08 14:30", tz = timezone)
# time_tail_mea <- ymd_hm("2017/02/24 10:00", tz = timezone)
time_head_mea <- ymd_hm("2012/09/08 00:30", tz = timezone)
time_tail_mea <- ymd_hm("2013/09/08 00:00", tz = timezone)

dt_noisy <-
  data_dT_La1 %>%
  filter(between(time, time_head_mea, time_tail_mea)) %>%
  select(time, all_of(dT_target))

dt_noisy <-
  data_meteoflux %>%
  filter(between(time, time_head_mea, time_tail_mea)) %>%
  select(PPT, Rs_in, Ta_60m, VPD_60m, WS) %>%
  bind_cols(dt_noisy, .) %>%
  rename(dt = !!dT_target,
         p = PPT,
         sw_in = Rs_in,
         ta = Ta_60m,
         vpd = VPD_60m,
         ws = WS)

dt_noisy <-
  data_SWC_30min %>%
  filter(between(time, time_head_mea, time_tail_mea)) %>%
  select(SWC_50_La1_mod2) %>%
  rename(swc = SWC_50_La1_mod2) %>%
  bind_cols(dt_noisy, .)


dt_gf <-
  data_dT_La1_01_gf %>%
  filter(between(time, time_head_mea, time_tail_mea)) %>%
  select(time, all_of(dT_target))

dt_gf <-
  dt_noisy %>%
  select(-c(time, dt)) %>%
  bind_cols(dt_gf, .) %>%
  rename(dt = !!dT_target)


## dTmax data set used for only function test
dtmax <-
  data_dTmax_La1_01 %>%
  filter(between(time, time_head_mea, time_tail_mea)) %>%
  select(time, contains(sensor_target)) %>%
  rename(dtmax_sp = dTmax_FV_La1_01,
         dtmax_pd = dTmax_PD_La1_01,
         dtmax_mw = dTmax_MW_La1_01,
         dtmax_dr = dTmax_DA_La1_01,
         dtmax_ed = dTmax_ED_La1_01) %>%
  select(time, contains("dtmax", ignore.case = FALSE))


# output data set ---------------------------------------------------------

usethis::use_data(dt_noisy, overwrite = TRUE)
usethis::use_data(dt_gf, overwrite = TRUE)
usethis::use_data(dtmax, overwrite = TRUE)

