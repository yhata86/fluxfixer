# Define reference values of average and standard deviation

\`calc_ref_stats()\` determines reference values of average and standard
deviation for the entire period by calculating the median of these
statistical values for the first several days in each sub-period.

## Usage

``` r
calc_ref_stats(
  vctr_time,
  vctr_target,
  vctr_time_prd_tail = NULL,
  wndw_size_ref = 48 * 15,
  label_err = -9999
)
```

## Arguments

- vctr_time:

  A timestamp vector of class POSIXct or POSIXt.

- vctr_target:

  A vector of a targeted time series to be checked. The length of the
  time series must be the same as that of \`vctr_time\`.

- vctr_time_prd_tail:

  A timestamp vector of class POSIXct or POSIXt, indicating the end
  timings of each sub-period. Note that users must not include the final
  timestamp for the entire time series. For instance, if users want to
  split the entire measurement period into three sub-periods, they only
  need to specify the end time stamps of the first two sub-periods.
  Default is \`NULL\`.

- wndw_size_ref:

  A positive integer indicating the number of data points included in
  calculating the average and standard deviation for their reference
  value determination. The default is 48 \* 15, meaning that the first
  15 days of each sub-period are used in the calculation when the time
  interval of the input timestamp is 30 minutes.

- label_err:

  A numeric value representing a missing value in the input vector(s).
  Default is -9999.

## Value

A vector of two components. The first one is the reference average, and
the second one is the reference standard deviation. The unit of these
values is the same as that of the input time series.

## See also

retrieve_ts

## Author

Yoshiaki Hata

## Examples

``` r
## Load data
data(dt_noisy)
time <- dt_noisy$time[11931:12891]
target <- dt_noisy$dt[11931:12891]
time_prd_tail <- time[148]

## Calculate reference values of average and standard deviation
result <-
  calc_ref_stats(vctr_time = time, vctr_target = target,
                 vctr_time_prd_tail = time_prd_tail)
```
