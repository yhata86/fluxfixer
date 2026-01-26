# Remove error values manually

\`remove_manually()\` removes unreasonable values manually by indicating
specific timestamps.

## Usage

``` r
remove_manually(vctr_time, vctr_target, vctr_time_err, label_err = -9999)
```

## Arguments

- vctr_time:

  A timestamp vector of class POSIXct or POSIXt.

- vctr_target:

  A vector of a targeted time series to be checked. The length of the
  time series must be the same as that of \`vctr_time\`.

- vctr_time_err:

  A timestamp vector of class POSIXct or POSIXt, indicating specific
  error timings.

- label_err:

  A numeric value representing a missing value in the input vector(s).
  Default is -9999.

## Value

A vector of cleaned time series. The length of the time series is the
same as the input time series. The data points at the indicated time
points by \`vctr_time_err\` are replaced with the error label specified
in \`label_err\`.

## Author

Yoshiaki Hata

## Examples

``` r
## Load data
data(dt_noisy)
time <- dt_noisy$time[12097:14400]
target <- dt_noisy$dt[12097:14400]
time_err <- seq(as.POSIXct("2013/06/27 18:00", tz = "Etc/GMT-8"),
                as.POSIXct("2013/06/27 22:30", tz = "Etc/GMT-8"),
                by = "30 min")

## Remove error values
result <-
 remove_manually(vctr_time = time, vctr_target = target,
                 vctr_time_err = time_err)
```
