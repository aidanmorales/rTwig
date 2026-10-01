# Smooth QSM

Smooth a QSM by ensuring the midpoints of all cylinders are connected
and gaps filled. All length based metrics are also updated. Only TreeQSM
is supported.

## Usage

``` r
smooth_qsm(cylinder)
```

## Arguments

- cylinder:

  QSM cylinder data frame

## Value

Returns a data frame

## Examples

``` r

file <- system.file("extdata/QSM.mat", package = "rTwig")
qsm <- import_qsm(file)
#> Importing TreeQSM
cylinder <- qsm$cylinder
cylinder <- update_cylinders(cylinder)
#> Updating Cylinder Ordering
#> Calculating Total Children
#> Building Cylinder Network
#> Caching Network
#> Verifying Topology
#> Calculating Growth Length
#> Calculating Reverse Branch Order
#> Calculating Branch Segments
#> Calculating Alternate Branch Numbers
#> Calculating Path Metrics

## Before Smoothing
plot_qsm(cylinder)
#> Plotting Cylinders
#> Panning plot on rgl device: 1

## After Smoothing
cylinder <- smooth_qsm(cylinder)
#> Smoothing QSM
#> Calculating Growth Length
#> Calculating Path Metrics
plot_qsm(cylinder)
#> Plotting Cylinders
#> Panning plot on rgl device: 1
```
