# Box-Dimension

## Background

Box dimension is a measure of the structural complexity of an object. It
is grounded in theoretical fractal analysis and is applied here to
three-dimensional point cloud data. This method uses box counting (or
more appropriately voxel counting), where the relationship between the
total number of boxes needed to contain the point cloud and the box size
is the structural complexity of the point cloud. Box dimension is the
slope of a simple linear equation between the log of the number of
boxes, and the log of the inverse box size. A larger R-squared value
indicates more self similarity in the point cloud.

Box dimension ranges from 1 (a line) to 3 (a solid cube). A box
dimension of 2.72 is a special object known as the Menger sponge, which
has both zero volume and an infinite surface area. Most individual trees
have a box dimension between 1 and 2.2.

Box dimension is sensitive to the resolution and level of occlusion in
the point cloud. Too low of a resolution can lead to oversimplification,
and too much occlusion can lead to too few boxes being used to contain
the cloud as box size decreases. A good quality point cloud with low
occlusion and a resolution between 0-1 cm is optimal for calculating box
dimension.

## Installation

You can install the package directly from
[CRAN](https://CRAN.R-project.org):

``` r
install.packages("rTwig")
```

Or the latest development version from [GitHub](https://github.com/):

``` r
devtools::install_github("https://github.com/aidanmorales/rTwig")
```

## Load Packages

The first step is to load the rTwig package.

``` r
library(rTwig)
```

## Load Point Cloud

Now we need to load a point cloud. Box dimension can run on any point
cloud, from single trees, to entire forest stands. Let’s use our example
Kentucky coffee tree.

``` r
file <- system.file("extdata/cloud.txt", package = "rTwig")
cloud <- read.table(file, header = FALSE)
```

## Box Dimension

Now we can calculate the box dimension of our point cloud with the
[`box_dimension()`](https://aidanmorales.github.io/rTwig/reference/box_dimension.md)
function. We need to specify the location of the x, y, and z
coordinates, but it defaults to the first three columns in the point
cloud data frame. We can also set the lower cutoff, or the smallest box
size. Since our point cloud resolution is 1 cm, we set the `lowercutoff`
parameter to 0.01 m, which is also the default. Let’s take a look at the
summary data and extract the box dimension (slope).

``` r
# Box Dimension Summary
output <- box_dimension(cloud)
str(output)
#> List of 2
#>  $ :Classes 'tidytable', 'tbl', 'data.table' and 'data.frame':   9 obs. of  2 variables:
#>   ..$ log.box.size: num [1:9] 0 0.693 1.386 2.079 2.773 ...
#>   ..$ log.voxels  : num [1:9] 1.1 1.95 3.09 4.11 5.18 ...
#>   ..- attr(*, ".internal.selfref")=<externalptr> 
#>  $ :Classes 'tidytable', 'tbl', 'data.table' and 'data.frame':   1 obs. of  4 variables:
#>   ..$ r.squared    : num 1
#>   ..$ adj.r.squared: num 1
#>   ..$ intercept    : num 1.02
#>   ..$ slope        : num 1.48

# Box Dimension (slope)
output[[2]]$slope
#> [1] 1.483312
```

## Visualization

We can also plot the results as both 2D and 3D plots, by setting the
`plot` parameter to `2D`, `3D` or `ALL`. The `plot` parameter is set to
`FALSE` by default to speed up the calculations.

``` r
# 2D Plot
output <- box_dimension(cloud, plot = "2D")
```

![](Box-Dimension_files/figure-html/unnamed-chunk-8-1.png)

``` r
# 3D Plot
output <- box_dimension(cloud, plot = "3D")
```
