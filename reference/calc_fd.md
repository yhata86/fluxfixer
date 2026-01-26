# Calculate sap flux density time series

\`calc_fd()\` calculates Fd (sap flux density) time series by a
power-type function, including heartwood correction.

## Usage

``` r
calc_fd(
  vctr_dt,
  vctr_dtmax,
  alpha = 1.19 * 10^(-4),
  beta = 1.231,
  do_heartwood_correction = FALSE,
  ratio_conductive = NULL
)
```

## Arguments

- vctr_dt:

  A vector of dT (the temperature difference between sap flow probes, in
  degrees Celsius) time series. The length of the vector must match that
  of the timestamp vector. Missing values must be gap-filled previously.

- vctr_dtmax:

  A vector of dTmax (the maximum temperature difference between sap flow
  probes under zero-flow conditions, in degrees Celsius) time series.
  The length of the vector must match that of \`vctr_dt\`. Missing
  values must be gap-filled previously.

- alpha:

  A positive value representing a multiplier in the equation to
  calculate Fd. Default is 1.19 \* 10^(-4) (m3 m-2 s-1).

- beta:

  A positive value representing a power in the equation to calculate Fd.
  Default is 1.231.

- do_heartwood_correction:

  A boolean. If \`TRUE\`, the heartwood correction is applied to correct
  dT before calculating Fd; else, the correction is not applied. Default
  is \`FALSE\`.

- ratio_conductive:

  A number between 0 and 1, indicating the ratio of the probe length to
  sapwood width. This parameter must be provided if
  \`do_heartwood_correction\` is \`TRUE\`. Default is \`NULL\`.

## Value

A vector of Fd (m3 m-2 s-1). The length of the vector matches that of
the input dT and dTmax vectors.

## Details

Fd is estimated using a power-type function introduced by Granier (1985;
Annales des Sciences Forestieres, 1987; Tree Physiology). First, a
dimensionless index K is obtained from dT and dTmax. Second, K is raised
to the power \`beta\` and then multiplied by \`alpha\`, obtaining Fd.

If the sapwood width is shorter than the probe insertion length, dT can
be overestimated, resulting in an underestimation of Fd. Therefore,
heartwood correction is required to correct dT. Optionally, before
calculating Fd, dT can be replaced with the corrected dT by specifying
the ratio of the probe length to sapwood width. This correction assumes
that the dT measured by the part of the probe that is inserted into the
heartwood is always dTmax. See more details in Clearwater et al. (1999;
Tree Physiology).

## Author

Yoshiaki Hata

## Examples

``` r
## Load data
data(dt_gf)
data(dtmax)
dt <- dt_gf$dt
dtmax <- dtmax$dtmax_sp

## Calculate sap flux density
result <- calc_fd(vctr_dt = dt, vctr_dtmax = dtmax)
```
