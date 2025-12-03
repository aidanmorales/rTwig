# Cluster Cloud

Cluster a point cloud or simulate a point cloud based on its QSM. If
using the input point cloud, the cylinder ids are transferred to the
cloud using the nearest neighbors. If simulating a cloud, the points are
built directly from the cylinders with user controlled spacing. The
cylinder id can be used to easily join the desired variables from the
QSM to the cloud. The nearest neighbor search uses the C++ nanoflann
library.

## Usage

``` r
cluster_cloud(cylinder, cloud = NULL, spacing = NULL)
```

## Arguments

- cylinder:

  A QSM cylinder data frame.

- cloud:

  The input point cloud for the QSM to cluster. If NULL (default), the
  simulated cloud is returned.

- spacing:

  The point spacing in meters for the simulated cloud. Defaults to 0.02
  meters.

## Value

A point cloud data frame

## References

Blanco JL, Rai PK (2014). “nanoflann: a C++ header-only fork of FLANN, a
library for Nearest Neighbor (NN) with KD-trees.”
<https://github.com/jlblancoc/nanoflann>.

## Examples

``` r
## Load Data
file <- system.file("extdata/QSM.mat", package = "rTwig")
file2 <- system.file("extdata/cloud.txt", package = "rTwig")

qsm <- run_rtwig(file, twig_radius = 4.23, metrics = FALSE)
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
cloud <- read.table(file2)

## Clustered Cloud
clustered_cloud <- cluster_cloud(cylinder = qsm, cloud = cloud)
#> Clustering Point Cloud

# # Join QSM variables and export
# filename <- tempfile(pattern = "clustered_cloud", fileext = ".txt")
# clustered_cloud %>%
#   left_join(qsm) %>%
#   fwrite(file = filename)

## Simulated Cloud
simulated_cloud <- cluster_cloud(cylinder = qsm, spacing = 0.01)
#> Simulating Point Cloud

# Plot Simulated Cloud
plot_qsm(cloud = simulated_cloud)
#> Plotting Cloud
#> Panning plot on rgl device: 1

# # Join QSM variables and export
# filename2 <- tempfile(pattern = "simulated_cloud", fileext = ".txt")
# simulated_cloud %>%
#   left_join(qsm) %>%
#   fwrite(file = filename2)
```
