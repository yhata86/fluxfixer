# Obtain time interval of input timestamp vector

\`get_interval()\` retrieves the time interval of the input timestamp
vector and checks whether the format of the time vector is acceptable
for the successive process.

## Usage

``` r
get_interval(vctr_time)
```

## Arguments

- vctr_time:

  A timestamp vector of class POSIXct or POSIXlt. The timestamps must be
  equally spaced and arranged chronologically.

## Value

A numeric (minutes) indicating the time interval of the input timestamp
vector.

## References

Hata, Y. & Kumagai, T. (2026) fluxfixer: An R package for producing
thermal dissipation sap flow data with high quality control. SoftwareX,
34, 102740.
[doi:10.1016/j.softx.2026.102740](https://doi.org/10.1016/j.softx.2026.102740)

## Author

Yoshiaki Hata
