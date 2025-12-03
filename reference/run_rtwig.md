# Run Real Twig

Run the Real Twig correction and calculate tree metrics for supported
QSM formats.

## Usage

``` r
run_rtwig(
  filename,
  twig_radius,
  metrics = TRUE,
  version = NULL,
  method = NULL,
  smooth = TRUE,
  standardise = FALSE,
  broken_branch = TRUE,
  ...
)
```

## Arguments

- filename:

  file path to QSM (.mat, .csv, .json, .obj)

- twig_radius:

  Twig radius in millimeters

- metrics:

  Calculate tree metrics. Defaults to TRUE.

- version:

  If using a specific version of TreeQSM, the user can specify the
  version (e.g. 2.4.1, 2.0, etc.).

- method:

  Define the QSM generation method if using AdQSM or AdTree. Define the
  method as `adqsm` or `adtree`. Defaults to `adqsm`.

- smooth:

  Defaults to TRUE if using TreeQSM. Can be set to FALSE.

- standardise:

  Standardise QSM cylinder data. Defaults to FALSE, but can be set to
  TRUE.

- broken_branch:

  Enable or disable the broken branch filter. Defaults to TRUE.

- ...:

  Additional arguments allowing standardise and standardize to be used
  as synonyms.

## Value

Returns cylinder data frame or list if metrics is true.

## Examples

``` r
## TreeQSM
file <- system.file("extdata/QSM.mat", package = "rTwig")
qsm <- run_rtwig(file, twig_radius = 4.23)
#> Importing TreeQSM
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
#> Smoothing QSM
#> Generating Paths
#> Filtering Paths
#> Modeling Paths
#> Updating Radii
#> Calculating Branch Metrics
#> Calculating Segment Metrics
#> Calculating Tree Metrics
#> Generating Point Cloud
#> Calculating Crown Metrics
#> Calculating Tree Distributions
#> Calculating Branch Distributions
#> Calculating Segment Distributions
#> Calculating Stem Taper
#> Calculating Spreads
#> Calculating Alternate Branch Metrics
str(qsm$cylinder)
#> Classes ‘tidytable’, ‘tbl’, ‘data.table’ and 'data.frame':   1136 obs. of  33 variables:
#>  $ length                      : num  0.09392 0.07216 0.06654 0.00938 0.06795 ...
#>  $ start.x                     : num  0.768 0.769 0.768 0.769 0.769 ...
#>  $ start.y                     : num  -16.4 -16.4 -16.4 -16.3 -16.3 ...
#>  $ start.z                     : num  254 254 254 254 254 ...
#>  $ axis.x                      : num  0.009949 -0.022059 0.016224 -0.000391 0.012068 ...
#>  $ axis.y                      : num  0.091165 0.019722 0.034392 -0.000295 0.023483 ...
#>  $ axis.z                      : num  0.996 1 0.999 1 1 ...
#>  $ end.x                       : num  0.769 0.768 0.769 0.769 0.77 ...
#>  $ end.y                       : num  -16.4 -16.4 -16.3 -16.3 -16.3 ...
#>  $ end.z                       : num  254 254 254 254 254 ...
#>  $ added                       : num  0 0 0 0 0 0 0 0 0 0 ...
#>  $ BranchOrder                 : int  0 0 0 0 0 0 0 0 0 0 ...
#>  $ reverseBranchOrder          : num  20 20 20 20 20 20 20 20 20 20 ...
#>  $ PositionInBranch            : int  1 2 3 4 5 6 7 8 9 10 ...
#>  $ segment                     : int  1 1 1 1 1 1 1 1 1 1 ...
#>  $ parentSegment               : int  0 0 0 0 0 0 0 0 0 0 ...
#>  $ mad                         : num  0.00072 0.000538 0.000523 0.000335 0.000438 ...
#>  $ SurfCov                     : num  0.875 1 1 1 1 1 1 1 1 1 ...
#>  $ UnmodRadius                 : num  0.0465 0.0454 0.0442 0.0437 0.0429 ...
#>  $ OldRadius                   : num  0.0465 0.0454 0.0442 0.0437 0.0429 ...
#>  $ growthLength                : num  32.8 32.7 32.6 32.6 32.6 ...
#>  $ branch                      : int  1 1 1 1 1 1 1 1 1 1 ...
#>  $ branch_alt                  : int  0 0 0 0 0 0 0 0 0 0 ...
#>  $ parent                      : num  0 1 2 3 4 5 6 7 8 9 ...
#>  $ extension                   : int  1 2 3 4 5 6 7 8 9 10 ...
#>  $ totalChildren               : int  1 1 1 1 1 1 1 1 1 1 ...
#>  $ distanceFromBase            : num  0 0.0722 0.1387 0.1481 0.216 ...
#>  $ distanceToTwig              : num  3.4 3.31 3.24 3.17 3.16 ...
#>  $ reversePipeAreaBranchorder  : int  82 82 82 82 82 82 82 82 82 82 ...
#>  $ reversePipeRadiusBranchorder: num  9.06 9.06 9.06 9.06 9.06 ...
#>  $ vesselVolume                : num  279 271 266 260 259 ...
#>  $ radius                      : num  0.0465 0.0454 0.0442 0.0437 0.0429 ...
#>  $ modified                    : num  0 0 0 0 0 0 0 0 0 0 ...
#>  - attr(*, ".internal.selfref")=<externalptr> 
```
