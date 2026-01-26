# dTmax time series estimated by multiple methods

Dataset consisting of the time series of the dTmax (the maximum
temperature difference between thermal dissipation sap flow probes under
zero-flow conditions) calculated by the successive predawn (SP), daily
predawn (PD), moving window (MW), double regression (DR), and
environmental dependent (ED) methods.

## Usage

``` r
data(dtmax)
```

## Format

A data frame with 17520 rows and 6 variables:

- time:

  Timestamp of the measurement end timing

- dtmax_sp:

  dTmax estimated by the SP method (degrees Celsius)

- dtmax_pd:

  dTmax estimated by the PD method (degrees Celsius)

- dtmax_mw:

  dTmax estimated by the MW method (degrees Celsius)

- dtmax_dr:

  dTmax estimated by the DR method (degrees Celsius)

- dtmax_ed:

  dTmax estimated by the ED method (degrees Celsius)
