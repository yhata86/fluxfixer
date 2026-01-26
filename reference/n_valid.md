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

## Author

Yoshiaki Hata
