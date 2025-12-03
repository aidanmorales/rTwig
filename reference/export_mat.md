# Export MAT

Exports the cylinder data to the format used by TreeQSM

## Usage

``` r
export_mat(
  cylinder,
  filename,
  metrics = TRUE,
  rundata = NULL,
  pmdistance = NULL,
  triangulation = NULL
)
```

## Arguments

- cylinder:

  QSM cylinder data frame

- filename:

  Desired name of file

- metrics:

  Calculate treedata and branch structs? Defaults to TRUE.

- rundata:

  Optional rundata list created by
  [`import_treeqsm()`](https://aidanmorales.github.io/rTwig/reference/import_treeqsm.md)

- pmdistance:

  Optional pmdistance list created by
  [`import_treeqsm()`](https://aidanmorales.github.io/rTwig/reference/import_treeqsm.md)

- triangulation:

  Optional triangulation list created by
  [`import_treeqsm()`](https://aidanmorales.github.io/rTwig/reference/import_treeqsm.md)

## Value

Returns a .mat file

## Examples

``` r
## TreeQSM Processing Chain
file <- system.file("extdata/QSM.mat", package = "rTwig")
qsm <- import_treeqsm(file)
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

filename <- tempfile(pattern = "TreeQSM_QSM", fileext = ".mat")
export_mat(cylinder, filename)
#> Exporting to .mat
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

## SimpleForest Processing Chain
file <- system.file("extdata/QSM.csv", package = "rTwig")
cylinder <- read.csv(file)
cylinder <- update_cylinders(cylinder)
#> Finding Branches
#> Calculating Total Children
#> Building Cylinder Network
#> Caching Network
#> Verifying Topology
#> Calculating Path Metrics

filename <- tempfile(pattern = "SimpleForest_QSM", fileext = ".mat")
export_mat(cylinder, filename)
#> Exporting to .mat
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
```
