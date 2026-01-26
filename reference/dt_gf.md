# Quality-controlled and gap-filled dT time series

Dataset consisting of the time series of the quality-controlled and
gap-filled dT (the temperature difference between thermal dissipation
sap flow probes), meteorological factors, and soil water content at
Lambir Hills National Park, Malaysia.

## Usage

``` r
data(dt_gf)
```

## Format

A data frame with 17520 rows and 8 variables:

- time:

  Timestamp of the measurement end timing

- dt:

  Quality-controlled and gap-filled dT (degrees Celsius)

- p:

  Precipitation (mm)

- sw_in:

  Global solar radiation (W m-2)

- ta:

  Air temperature (degrees Celsius)

- vpd:

  Vapor pressure deficit (hPa)

- ws:

  Horizontal wind speed (m s-1)

- swc:

  Soil water content (m3 m-3)

## Source

Meteorological factors and soil water content data were provided by Dr.
Tomonori Kume \[ORCiD: 0000-0001-6569-139X\]
