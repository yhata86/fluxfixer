# Obtain the number of data points without missing values

\`n_valid()\` retrieves the number of data points without missing
values.

## Usage

``` r
n_valid(vctr, label_err = -9999)
```

## Arguments

- vctr:

  A vector to be evaluated.

- label_err:

  A numeric value representing a missing value in the input vector(s).
  Default is -9999.

## Value

An integer indicating the number of the input vector elements without
missing values.

## References

Hata, Y. & Kumagai, T. (2026) fluxfixer: An R package for producing
thermal dissipation sap flow data with high quality control. SoftwareX,
34, 102740.
[doi:10.1016/j.softx.2026.102740](https://doi.org/10.1016/j.softx.2026.102740)

## Author

Yoshiaki Hata
