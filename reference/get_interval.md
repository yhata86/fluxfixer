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

  A timestamp vector of class POSIXct or POSIXt. The timestamps must be
  equally spaced and arranged chronologically.

## Value

A numeric (minutes) indicating the time interval of the input timestamp
vector.

## Author

Yoshiaki Hata
