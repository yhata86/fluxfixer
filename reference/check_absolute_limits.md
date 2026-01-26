# Remove outliers by absolute limits

\`check_absolute_limits()\` removes out-of-range values by setting lower
and upper limits.

## Usage

``` r
check_absolute_limits(
  vctr_target,
  thres_al_min = 3,
  thres_al_max = 50,
  label_err = -9999
)
```

## Arguments

- vctr_target:

  A vector of a targeted time series to be checked.

- thres_al_min:

  A threshold value for the input time series to define the lower limit.
  Default is 3.0. The data points with values below the threshold are
  considered outliers and removed. The unit of the threshold must match
  that of the input time series.

- thres_al_max:

  A threshold value for the input time series to define the upper limit.
  Default is 50.0. The data points with values above the threshold are
  considered outliers and removed. The unit of the threshold must match
  that of the input time series.

- label_err:

  A numeric value representing a missing value in the input vector(s).
  Default is -9999.

## Value

A vector of cleaned time series. The length of the time series is the
same as the input time series. The data points with values below
\`thres_al_min\` or above \`thres_al_max\` are replaced with the error
label specified in \`label_err\`.

## Author

Yoshiaki Hata

## Examples

``` r
## Load data
data(dt_noisy)
target <- dt_noisy$dt

## Remove out-of-range values
result <- check_absolute_limits(vctr_target = target)
```
