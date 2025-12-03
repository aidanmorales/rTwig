# Prune QSM

Efficiently prune a QSM. The user can prune by cylinder, branch, and
segment ids, or by height or diameter classes, individually, or all at
the same time, and return either the pruned data, the remaining data, or
a binary index of which cylinders are pruned.

## Usage

``` r
prune_qsm(
  cylinder,
  cylinder_ids = NULL,
  branch_ids = NULL,
  segment_ids = NULL,
  height_m = NULL,
  diameter_cm = NULL,
  invert = FALSE,
  index = FALSE
)
```

## Arguments

- cylinder:

  QSM cylinder data frame

- cylinder_ids:

  A single or vector of cylinder ids. Everything connected above the
  cylinder is pruned.

- branch_ids:

  A single or vector of branch ids. Everything connected to the branch
  is pruned.

- segment_ids:

  A single or vector of segment ids. Everything connected above the
  segment is pruned.

- height_m:

  Height class in meters below which all cylinders are pruned. Valid
  inputs are numeric to one decimal.

- diameter_cm:

  Branch diameter class in centimeters below which all cylinders are
  pruned. Valid inputs are numeric to one decimal.

- invert:

  Return the remaining or pruned data. Defaults to TRUE (the remaining
  data), but can be set to FALSE.

- index:

  Returns a column index called `pruning` indicating if the cylinder is
  pruned (1) or un-pruned (0). Defaults to FALSE, but can be set to
  TRUE.

## Value

a data frame

## Examples

``` r
## Load QSM
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

## Pruning Index
prune1 <- prune_qsm(cylinder, height_m = 2, index = TRUE)
#> Pruning QSM
plot_qsm(prune1, color = pruning, palette = "blue2red")
#> Plotting Cylinders
#> Panning plot on rgl device: 1

## Remaining
prune2 <- prune_qsm(cylinder, height_m = 2, invert = FALSE)
#> Pruning QSM
plot_qsm(prune2)
#> Plotting Cylinders
#> Panning plot on rgl device: 1

## Pruned
prune3 <- prune_qsm(cylinder, height_m = 2, invert = TRUE)
#> Pruning QSM
plot_qsm(prune3)
#> Plotting Cylinders
#> Panning plot on rgl device: 1
```
