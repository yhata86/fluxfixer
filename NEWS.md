# fluxfixer (development version)

## Minor changes

* Better final message appears when setting `skip_sapflow_calc` as `TRUE` in
 `run_fluxfixer()`

* Better message appears when setting `modify_z` as `TRUE` in
 `run_fluxfixer()`

* Better message appears when inputting wrong column names for `colname_time`
 or `colname_target` in `run_fluxfixer()`
 
* Additional columns are now created in the input data frame when only
 mandatory variables are input in `run_fluxfixer()`

## Bug fixes

* QC flag determination in `run_fluxfixer()` now works correctly.

* Z-score modification during short attenuation periods in `run_fluxfixer()`
 now works correctly.

# fluxfixer 1.0.0

* First CRAN release
