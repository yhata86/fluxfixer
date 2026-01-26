# Retrieve time series in its original units

\`retrieve_ts()\` converts a standardized Z-score time series into a
time series in its original units using specific average and standard
deviation time series.

## Usage

``` r
retrieve_ts(
  vctr_target_z,
  vctr_target_avg = NA,
  vctr_target_sd = NA,
  detrend = FALSE,
  correct_damping = FALSE,
  avg_ref = NULL,
  sd_ref = NULL,
  label_err = -9999
)
```

## Arguments

- vctr_target_z:

  A vector of Z-score time series to be converted. Missing values must
  be gap-filled previously.

- vctr_target_avg:

  A vector of average time series. Missing values are acceptable but
  automatically gap-filled by interpolation during the retrieving
  process. The length of the vector must match that of
  \`vctr_target_z\`. The unit of the time series must match that of time
  series to be output. Default is \`NA\`.

- vctr_target_sd:

  A vector of standard deviation time series. Missing values are
  acceptable but automatically gap-filled by interpolation during the
  retrieving process. The length of the vector must match that of
  \`vctr_target_z\`. The unit of the time series must match that of time
  series to be output. Default is \`NA\`.

- detrend:

  A boolean. If \`TRUE\`, detrending is applied and the reference
  average specified by \`avg_ref\` is used to convert Z-score time
  series into the time series with the reference average in its original
  units; else, the detrending is not applied, and the average time
  series specified by \`vctr_target_avg\` is used in the conversion.
  Default is \`FALSE\`.

- correct_damping:

  A boolean. If \`TRUE\`, the signal damping correction is applied and
  the reference standard deviation specified by \`sd_ref\` is used to
  convert Z-score time series into the time series in its original units
  with the reference standard deviation; else, the correction is not
  applied, and the standard deviation time series specified by
  \`vctr_target_sd\` is used in the conversion. Default is \`FALSE\`.

- avg_ref:

  Only valid if \`detrend\` is \`TRUE\`. A numeric value representing
  the reference average. A vector of reference average time series is
  also acceptable, but the length of the vector must match that of
  \`vctr_target_z\`, and the unit of the time series must match that of
  time series to be output. Default is \`NULL\`.

- sd_ref:

  Only valid if \`correct_damping\` is \`TRUE\`. A positive numeric
  value representing the reference standard deviation. A vector of
  reference standard deviation time series is also acceptable, but the
  length of the vector must match that of \`vctr_target_z\`, and the
  unit of the time series must match that of time series to be output.
  Default is \`NULL\`.

- label_err:

  A numeric value representing a missing value in the input vector(s).
  Default is -9999.

## Value

A vector of the retrieved time series. The length of the vector is the
same as \`vctr_target_z\`.

## Details

Retrieving a time series with its original units is conducted by
multiplying a Z-score by the standard deviation, followed by adding the
average. If the average and standard deviation time series are the same
as those in converting the original time series into the Z-score time
series, the original values with the original average and standard
deviation are retrieved. If reference values of the average and/or
standard deviation are used, the output time series are detrended and/or
applied to signal damping correction.

## See also

calc_ref_stats

## Author

Yoshiaki Hata

## Examples

``` r
## Create data
target <- seq(1, 10)
target_avg <- rep(mean(target), 10)
target_sd <- rep(stats::sd(target), 10)
target_z <- (target - target_avg) / target_sd

## Retrieve time series in its original units
result <-
  retrieve_ts(vctr_target_z = target_z, vctr_target_avg = target_avg,
              vctr_target_sd = target_sd)
#> Time series retrieval started
#> --- Signal damping correction was not applied
#> --- Detrending was not applied
#> Time series retrieval finished
```
