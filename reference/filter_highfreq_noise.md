# Filter high frequency noise by Gaussian filter

\`filter_highfreq_noise()\` filters a time series with a specific period
by convolving it with a Gaussian window, removing high-frequency noise.

## Usage

``` r
filter_highfreq_noise(
  vctr_time,
  vctr_target,
  vctr_time_noise,
  wndw_size_noise = 13,
  inv_sigma_noise = 0.01,
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

- vctr_time_noise:

  A timestamp vector of class POSIXct or POSIXt, indicating when
  high-frequency noise exists in the targeted time series.

- wndw_size_noise:

  A positive integer indicating the number of data points included in a
  moving Gaussian window for the high-frequency noise filtering. The
  default is 13, meaning that the window size is 6.5 hours if the time
  interval of the input timestamp is 30 minutes.

- inv_sigma_noise:

  A positive value defining a Gaussian window width for the
  high-frequency noise filtering. The width of the Gaussian window is
  inversely proportional to this parameter. Default is 0.01.

- label_err:

  A numeric value representing a missing value in the input vector(s).
  Default is -9999.

## Value

A vector of the noise-filtered time series. The length of the time
series is the same as the input time series.

## Author

Yoshiaki Hata

## Examples

``` r
## Create data
time <- seq(as.POSIXct("2026/01/01"), length.out = 360, by = "1 day")
x <- seq(1, 360)
target <- sin(x / 180 * pi) + stats::rnorm(length(x), sd = 0.2)
time_noise <-
  seq(as.POSIXct("2026/01/01"), as.POSIXct("2026/09/01"), by = "1 day")

## Filter noise
result <-
  filter_highfreq_noise(vctr_time = time, vctr_target = target,
                        vctr_time_noise = time_noise)
```
