# Calculate global solar radiation time series at TOA

\`calc_sw_in_toa()\` obtains incident global solar radiation time series
at TOA (top of atmosphere) at a specific location by calculating solar
elevation angle estimated from the equations of Campbell and Norman
(1998).

## Usage

``` r
calc_sw_in_toa(
  vctr_time,
  lat,
  lon,
  std_meridian,
  solar_const = 1365,
  sbeta_min = 0.001
)
```

## Arguments

- vctr_time:

  A timestamp vector of class POSIXct or POSIXlt. The timestamps must be
  equally spaced and arranged chronologically.

- lat:

  A numeric value (degrees) between -90 and 90, indicating the latitude
  of the specific location.

- lon:

  A numeric value (degrees) between -180 and 180, indicating the
  longitude of the specific location.

- std_meridian:

  A numeric value (degrees) between -180 and 180, indicating the
  standard meridian of the specific location.

- solar_const:

  A positive value (W m-2) indicating the solar constant. Default is
  1365 (W m-2).

- sbeta_min:

  A threshold value of the solar elevation angle (degrees). If the
  calculated solar elevation angle is less than this threshold, the
  corresponding global solar radiation becomes zero.

## Value

A vector of the global solar radiation at TOA (W m-2). The length of the
vector matches that of the input timestamp vector.

## References

- Hata, Y. & Kumagai, T. (2026) fluxfixer: An R package for producing
  thermal dissipation sap flow data with high quality control.
  SoftwareX, 34, 102740.
  [doi:10.1016/j.softx.2026.102740](https://doi.org/10.1016/j.softx.2026.102740)

- Campbell, G. S. & Norman J. M. (1998) An Introduction to Environmental
  Biophysics. Springer New York. 2nd Edition.
  [doi:10.1007/978-1-4612-1626-1](https://doi.org/10.1007/978-1-4612-1626-1)

## Author

Yoshiaki Hata

## Examples

``` r
## Make a timestamp vector
timezone <- "Etc/GMT-8"
time <- seq(as.POSIXct("2026/01/01", tz = timezone),
 as.POSIXct("2026/01/02", tz = timezone), by = "30 min")

## Obtain global solar radiation at Lambir Hills National Park in Malaysia
result <-
 calc_sw_in_toa(vctr_time = time, lat = 4.201007, lon = 114.039079,
                std_meridian = 120)
```
