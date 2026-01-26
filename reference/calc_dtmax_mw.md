# Calculate dTmax by the moving window method

\`calc_dtmax_mw()\` calculates the time series of dTmax (the maximum
temperature difference between sap flow probes under zero-flow
conditions) using the moving window method.

## Usage

``` r
calc_dtmax_mw(
  vctr_time_daily = NULL,
  vctr_dtmax_pd = NULL,
  wndw_size_dtmax = 11,
  min_n_wndw_dtmax = 3,
  output_daily = FALSE,
  vctr_time = NULL,
  vctr_dt = NULL,
  vctr_radi = NULL,
  thres_radi = 100,
  thres_hour_pd = 8
)
```

## Arguments

- vctr_time_daily:

  A timestamp vector of class POSIXct or POSIXt in daily steps. This
  vector indicates the start and end dates of the measurement, and is
  assumed to be output from the \`calc_dtmax_pd()\` function. The
  timestamps must be equally spaced and arranged chronologically. If
  this argument is \`NULL\` (default), \`vctr_time\`, \`vctr_dt\`, and
  \`vctr_radi\` must be provided to conduct the daily predawn method
  previously.

- vctr_dtmax_pd:

  A vector of dTmax estimated by the daily predawn method in daily
  steps. This vector is assumed to be output from the
  \`calc_dtmax_pd()\` function. The length of the vector must match that
  of the \`vctr_time_daily\`. If this argument is \`NULL\` (default),
  \`vctr_time\`, \`vctr_dt\`, and \`vctr_radi\` must be provided to
  conduct the daily predawn method previously.

- wndw_size_dtmax:

  A positive integer indicating the window size (days) for determining
  moving window maximum values of dTmax. Default is 11 (days).

- min_n_wndw_dtmax:

  A positive integer indicating the minimum number of data points for
  calculating statistics using a moving window (default is 3). If the
  number of data points is less than this threshold, the statistics are
  not calculated in the window.

- output_daily:

  A boolean. If \`TRUE\`, returns dTmax time series in daily steps;
  else, returns dTmax in the original time steps. Default is \`FALSE\`.

- vctr_time:

  Only valid when \`vctr_dtmax_pd\` is \`NULL\`. A timestamp vector of
  class POSIXct or POSIXt. This vector indicates the timings of the end
  of each measurement in local time. Any interval (typically 15 to 60
  min) is allowed, but the timestamps must be equally spaced and
  arranged chronologically. Default is \`NULL\`.

- vctr_dt:

  Only valid when \`vctr_dtmax_pd\` is \`NULL\`. A vector of dT (the
  temperature difference between sap flow probes, in degrees Celsius)
  time series. The length of the vector must match that of the
  \`vctr_time\`. Missing values must be gap-filled previously. Default
  is \`NULL\`.

- vctr_radi:

  Only valid when \`vctr_dtmax_pd\` is \`NULL\`. A vector of global
  solar radiation or a similar radiative variable time series. The
  length of the vector must match that of the \`vctr_time\`. Missing
  values must be gap-filled previously. The unit of the time series must
  match that of \`thres_radi\`. Default is \`NULL\`.

- thres_radi:

  A threshold value of the radiation to define daytime. Default is 100
  (W m-2). The data points with radiation values above the threshold are
  considered daytime values. The unit of the threshold must match that
  of the input radiation time series.

- thres_hour_pd:

  An integer from 0 to 23. The threshold hour of the day which defines
  the end of predawn in local time (default is 8).

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

\* The third column, \`dtmax_mw\`, gives the estimated dTmax by the
moving window method. If \`output_daily\` is \`FALSE\` (default), this
column has the same time step as the input timestamp. If
\`output_daily\` is \`TRUE\`, the dTmax is returned in daily steps.

## Details

The moving window method is one of the methods for determining dTmax.
This method selects the maximum value of dTmax, estimated by the daily
predawn method, using a moving window with an eleven-day length. The
selected dTmax is considered to be the final dTmax. See more details in
Peters et al. (2018; New Phytologist).

## See also

\`calc_dtmax\`, \`calc_dtmax_sp\`, \`calc_dtmax_pd\`, \`calc_dtmax_dr\`,
\`calc_dtmax_ed\`

## Author

Yoshiaki Hata
