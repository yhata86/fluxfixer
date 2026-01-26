# Calculate dTmax by the daily predawn method

\`calc_dtmax_pd()\` calculates the time series of dTmax (the maximum
temperature difference between sap flow probes under zero-flow
conditions) using the daily predawn method.

## Usage

``` r
calc_dtmax_pd(
  vctr_time,
  vctr_dt,
  vctr_radi,
  thres_radi = 100,
  thres_hour_pd = 8,
  min_n_wndw_dtmax = 3,
  output_daily = FALSE
)
```

## Arguments

- vctr_time:

  A timestamp vector of class POSIXct or POSIXt. This vector indicates
  the timings of the end of each measurement in local time. Any interval
  (typically 15 to 60 min) is allowed, but the timestamps must be
  equally spaced and arranged chronologically.

- vctr_dt:

  A vector of dT (the temperature difference between sap flow probes, in
  degrees Celsius) time series. The length of the vector must match that
  of the timestamp vector. Missing values must be gap-filled previously.

- vctr_radi:

  A vector of global solar radiation or a similar radiative variable
  time series. The length of the vector must match that of the timestamp
  vector. Missing values must be gap-filled previously. The unit of the
  time series must match that of \`thres_radi\`.

- thres_radi:

  A threshold value of the radiation to define daytime. Default is 100
  (W m-2). The data points with radiation values above the threshold are
  considered daytime values. The unit of the threshold must match that
  of the input radiation time series.

- thres_hour_pd:

  An integer from 0 to 23. The threshold hour of the day which defines
  the end of predawn in local time (default is 8).

- min_n_wndw_dtmax:

  A positive integer indicating the minimum number of data points for
  calculating statistics using a moving window (default is 3). If the
  number of data points is less than this threshold, the statistics are
  not calculated in the window.

- output_daily:

  A boolean. If \`TRUE\`, returns dTmax time series in daily steps;
  else, returns dTmax in the original time steps. Default is \`FALSE\`.

## Value

A data frame with columns below:

\* The first column, \`time\`, gives the timestamp of the measurements.
If \`output_daily\` is \`FALSE\` (default), this column is the same as
the input timestamp, \`vctr_time\`. If \`output_daily\` is \`TRUE\`, the
timestamp in daily steps is returned.

\* The second column, \`dt\`, gives the input dT (the temperature
difference between sap flow probes, degrees Celsius) time series. If
\`output_daily\` is \`TRUE\`, dT is returned in daily steps. If
\`output_daily\` is \`FALSE\` (default), this column is not output.

\* The third column, \`dtmax_pd\`, gives the estimated dTmax by the
daily predawn method. If \`output_daily\` is \`FALSE\` (default), this
column has the same time step as the input timestamp. If
\`output_daily\` is \`TRUE\`, the dTmax is returned in daily steps.

## Details

The daily predawn method is one of the methods for determining dTmax.
This method defines the dTmax for a day as the maximum dT (the
temperature difference between sap flow probes) between midnight and the
morning (8:00 a.m. in local time) when the global solar radiation is
below the threshold value (100 W m-2). See more details in Peters et al.
(2018; New Phytologist).

## See also

\`calc_dtmax\`, \`calc_dtmax_sp\`, \`calc_dtmax_mw\`, \`calc_dtmax_dr\`,
\`calc_dtmax_ed\`

## Author

Yoshiaki Hata
