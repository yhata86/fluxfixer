# Calculate dTmax by various methods

\`calc_dtmax()\` calculates the time series of dTmax (the maximum
temperature difference between sap flow probes under zero-flow
conditions) using multiple methods.

## Usage

``` r
calc_dtmax(
  vctr_time,
  vctr_dt,
  vctr_radi = NULL,
  vctr_ta = NULL,
  vctr_vpd = NULL,
  method = c("sp"),
  thres_hour_sp = 5,
  thres_radi = 100,
  thres_ta = 1,
  thres_vpd = 1,
  thres_cv = 0.005,
  thres_hour_pd = 8,
  min_n_wndw_dtmax = 3,
  wndw_size_dtmax = 11,
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
  time series. Default is \`NULL\`, but this vector must be provided
  when \`method\` includes \`pd\`, \`mw\`, \`dr\`, or \`ed\`. The length
  of the vector must match that of the timestamp vector. Missing values
  must be gap-filled previously. The unit of the time series must match
  that of \`thres_radi\`.

- vctr_ta:

  A vector of air temperature (degrees Celsius) time series. Default is
  \`NULL\`, but this vector must be provided when \`method\` includes
  \`ed\`. The length of the vector must match that of the timestamp
  vector. Missing values must be gap-filled previously. The unit of the
  time series must match that of \`thres_ta\`.

- vctr_vpd:

  A vector of vapor pressure deficit (VPD, in hPa) time series. Default
  is \`NULL\`, but this vector must be provided when \`method\` includes
  \`ed\`. The length of the vector must match that of the timestamp
  vector. Missing values must be gap-filled previously. The unit of the
  time series must match that of \`thres_vpd\`.

- method:

  A vector of characters indicating the dTmax estimation methods. "sp",
  "pd", "mw", "dr", and "ed" represent the successive predawn, daily
  predawn, moving window, double regression, and environmental dependent
  method, respectively. Default is \`c("sp")\`.

- thres_hour_sp:

  An integer from 0 to 23. The threshold hour of the day which defines
  the start of predawn in local time (default is 5).

- thres_radi:

  A threshold value of the radiation to define daytime. Default is 100
  (W m-2). The data points with radiation values above the threshold are
  considered daytime values. The unit of the threshold must match that
  of the input radiation time series.

- thres_ta:

  A threshold value of the air temperature to define predawn. Default is
  1.0 (degrees Celsius). The dTmax, estimated by the PD method, with air
  temperature values below the threshold, is selected as a candidate for
  the final dTmax. The unit of the threshold must match that of the
  input air temperature time series.

- thres_vpd:

  A threshold value of the VPD to define predawn. Default is 1.0 (hPa).
  The dTmax, estimated by the PD method, with VPD values below the
  threshold, is selected as a candidate for the final dTmax. The unit of
  the threshold must match that of the input VPD time series.

- thres_cv:

  A threshold value of the coefficient of variation (CV) to define
  predawn. Default is 0.005. The dTmax, estimated by the PD method, with
  CV values below the threshold, is selected as a candidate for the
  final dTmax.

- thres_hour_pd:

  An integer from 0 to 23. The threshold hour of the day which defines
  the end of predawn in local time (default is 8).

- min_n_wndw_dtmax:

  A positive integer indicating the minimum number of data points for
  calculating statistics using a moving window (default is 3). If the
  number of data points is less than this threshold, the statistics are
  not calculated in the window.

- wndw_size_dtmax:

  A positive integer indicating the window size (days) for determining
  moving window maximum values of dTmax. Default is 11 (days).

- output_daily:

  A boolean. If \`TRUE\`, returns dTmax time series in daily steps;
  else, returns dTmax in the original time steps. Default is \`FALSE\`.

## Value

A data frame with columns below:

\* The first column, \`time\`, gives the timestamp of the measurements.
If \`output_daily\` is \`FALSE\` (default), this column is the same as
the input timestamp, \`vctr_time\`. If \`output_daily\` is \`TRUE\`, the
timestamp in daily steps is returned.

\* The second column, \`dt\`, gives the input dT time series. If
\`output_daily\` is \`TRUE\`, dT is returned in daily steps. If
\`output_daily\` is \`FALSE\` (default), this column is not output.

\* The other columns, which have the prefix "dtmax\_", provide the dTmax
calculated by the methods specified in \`method\`. The last two letters
of the column name represent the name of the dTmax estimation method.
"sp", "pd", "mw", "dr", and "ed" represent the successive predawn, daily
predawn, moving window, double regression, and environmental dependent
method, respectively. If \`output_daily\` is \`FALSE\` (default), this
column has the same time step as the input timestamp. If
\`output_daily\` is \`TRUE\`, the dTmax is returned in daily steps."

## Details

This function provides multiple dTmax time series estimated by different
methods below:

\* The successive predawn (SP) method defines the dTmax for a day as the
maximum dT (the temperature difference between sap flow probes) within a
24-hour period that begins at 5:00 a.m. (default; just before daybreak
in temperate zones). In other words, the day starts at predawn, not
midnight, and the maximum value for that period is assumed to be dTmax.
This method has the advantage of being able to calculate dTmax quickly
while minimizing the effect of nocturnal transpiration on dTmax
estimation.

\* The daily predawn (PD) method defines the dTmax for a day as the
maximum dT between midnight and the morning (8:00 a.m. in local time)
when the global solar radiation is below the threshold value (100 W
m-2). See more details in Peters et al. (2018; New Phytologist).

\* The moving window (MW) method selects the maximum value of dTmax,
estimated by the daily predawn method, using a moving window with an
eleven-day length. The selected dTmax is considered to be the final
dTmax. See more details in Peters et al. (2018; New Phytologist).

\* The double regression (DR) method first calculates the moving window
mean value of dTmax, estimated by the daily predawn method, with an
eleven-day length. The dTmax that is lower than the mean is omitted, and
then the moving window mean is recalculated as the final dTmax. See more
details in Peters et al. (2018; New Phytologist).

\* The environmental dependent (ED) method filters the dTmax, estimated
by the daily predawn method, using the environmental conditions when
plants let their sap flow nearly zero. A stable dT, with a low
coefficient of variation, and low air temperature or vapor pressure
deficit over a two-hour period, characterizes these zero-flow
conditions. See more details in Oishi et al. (2016; SoftwareX) and
Peters et al. (2018; New Phytologist). After the filtering, the daily
dTmax is interpolated if necessary.

## Examples

``` r
## Load data
data(dt_gf)
time <- dt_gf$time[1:480]
dt <- dt_gf$dt[1:480]
radi <- dt_gf$sw_in[1:480]
ta <- dt_gf$ta[1:480]
vpd <- dt_gf$vpd[1:480]

## Calculate dTmax from gap-filled dT time series
result <-
 calc_dtmax(vctr_time = time, vctr_dt = dt, vctr_radi = radi, vctr_ta = ta,
            vctr_vpd = vpd, method = c("sp", "pd", "mw", "dr", "ed"),
            thres_vpd = 6.0)
#> Zero-flow condition estimation started
#> --- dTmax calculation by the SP method started
#> --- dTmax calculation by the SP method finished
#> --- dTmax calculation by the PD and ED methods started
#>   |                                                                              |                                                                      |   0%  |                                                                              |========                                                              |  11%  |                                                                              |================                                                      |  22%  |                                                                              |=======================                                               |  33%  |                                                                              |===============================                                       |  44%  |                                                                              |=======================================                               |  56%  |                                                                              |===============================================                       |  67%  |                                                                              |======================================================                |  78%  |                                                                              |==============================================================        |  89%  |                                                                              |======================================================================| 100%
#> 
#> --- dTmax calculation by the PD and ED methods finished
#> --- dTmax calculation by the MW and DR methods started
#>   |                                                                              |                                                                      |   0%  |                                                                              |========                                                              |  11%  |                                                                              |================                                                      |  22%  |                                                                              |=======================                                               |  33%  |                                                                              |===============================                                       |  44%  |                                                                              |=======================================                               |  56%  |                                                                              |===============================================                       |  67%  |                                                                              |======================================================                |  78%  |                                                                              |==============================================================        |  89%  |                                                                              |======================================================================| 100%
#> 
#> --- dTmax calculation by the MW and DR methods finished
#> --- dTmax time series aggregation started
#> --- dTmax time series aggregation finished
#> Zero-flow condition estimation finished
```
