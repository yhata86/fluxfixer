# Calculate dTmax by the successive predawn method

\`calc_dtmax_sp()\` calculates the time series of dTmax (the maximum
temperature difference between sap flow probes under zero-flow
conditions) using the successive predawn method.

## Usage

``` r
calc_dtmax_sp(vctr_time, vctr_dt, thres_hour_sp = 5, output_daily = FALSE)
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

- thres_hour_sp:

  An integer from 0 to 23. The threshold hour of the day which defines
  the start of predawn in local time (default is 5).

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

\* The third column, \`dtmax_sp\`, gives the estimated dTmax by the
successive predawn method. If \`output_daily\` is \`FALSE\` (default),
this column has the same time step as the input timestamp. If
\`output_daily\` is \`TRUE\`, the dTmax is returned in daily steps.

## Details

The successive predawn method is one of the methods for determining the
maximum temperature difference between sap flow probes under zero-flow
conditions. This method defines the dTmax for a day as the maximum dT
(the temperature difference between sap flow probes) within a 24-hour
period that begins at 5:00 a.m. (default; just before daybreak in
temperate zones). In other words, the day starts at predawn, not
midnight, and the maximum value for that period is assumed to be dTmax.
This method has the advantage of being able to calculate dTmax quickly
while minimizing the effect of nocturnal transpiration on dTmax
estimation.

## See also

\`calc_dtmax\`, \`calc_dtmax_pd\`, \`calc_dtmax_mw\`, \`calc_dtmax_dr\`,
\`calc_dtmax_ed\`

## Author

Yoshiaki Hata
