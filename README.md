
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fluxfixer

<!-- badges: start -->
<!-- badges: end -->

The goal of fluxfixer is to post-process sap flux data
semi-automatically. Notably, this package can detect outliers and fill
data gaps automatically by machine learning method, which uses
meteorological variables as input. Also, fluxfixer offers various
functions that can detect aberrant structural changes in time series
dynamics, correct such time series, and calculate sap flux density with
different methods for deriving zero-flow conditions.

## Caution

This is alpha-version package and under developing. I recommend you to
install the later version.

## Installation

You can install the development version of fluxfixer like so:

``` r
remotes::install_github("yhata86/fluxfixer")
library(fluxfixer)
```
