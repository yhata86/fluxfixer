# Modify short-term drifts

\`modify_short_drift()\` corrects short-term drifts by adjusting the 5th
and 95th percentiles of the drifted time series to those of the
reference time series.

## Usage

``` r
modify_short_drift(
  vctr_time,
  vctr_target,
  vctr_time_drft_head,
  vctr_time_drft_tail,
  n_day_ref = 3,
  label_err = -9999
)
```

## Arguments

- vctr_time:

  A timestamp vector of class POSIXct or POSIXt. The timestamps must be
  equally spaced and arranged chronologically.

- vctr_target:

  A vector of a targeted time series to be checked. The length of the
  time series must be the same as that of \`vctr_time\`.

- vctr_time_drft_head:

  A timestamp vector of class POSIXct or POSIXt, indicating when each
  drift starts.

- vctr_time_drft_tail:

  A timestamp vector of class POSIXct or POSIXt, indicating when each
  drift ends. The length of the time series must be the same as that of
  \`vctr_time_drft_head\`.

- n_day_ref:

  A positive integer representing the number of days to be referenced
  before and after the anomaly period. Default is 3 (days).

- label_err:

  A numeric value representing a missing value in the input vector(s).
  Default is -9999.

## Value

A vector of the drift-corrected time series. The length of the time
series is the same as the input time series.

## Details

The short-term drift correction is to correct sudden changes in the
average in the time series over a short period (hours to days) specified
by \`vctr_time_drft_head\` and \`vctr_time_drft_tail\`. Multiple
short-term drifts can be corrected at once using this function.This
procedure uses a reference period, which is defined to consist of the
three days (default) before and after the occurrence of the anomaly.
Then, the anomalous time series is standardized so that the 5th and 95th
percentile values of the anomalous and reference (non-anomalous) time
series match over this period. These percentile values are used instead
of the maximum and minimum values to ensure robustness against possible
outliers in the original or reference time series.

## Author

Yoshiaki Hata

## Examples

``` r
## Load data
data(dt_noisy)
time <- dt_noisy$time[11931:12891]
target <- dt_noisy$dt[11931:12891]
time_drft_head <- time[1]
time_drft_tail <- time[148]

## Correct a short-term drift
result <-
  modify_short_drift(vctr_time = time, vctr_target = target,
                     vctr_time_drft_head = time_drft_head,
                     vctr_time_drft_tail = time_drft_tail)
```
