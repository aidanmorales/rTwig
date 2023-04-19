# rTwig

## Description

Official repository for the rTwig package. The Real Twig method corrects overestimated twig cylinder radii in QSMs using real twig measurements from corresponding tree species and network analysis of paths in the QSM. Also included are tools for QSM visualization and point cloud fractal analysis.

## Installation

You can install the released version of rTwig from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("rTwig") # placeholder for potential CRAN release. 
```

You can install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("aidanmorales/rTwig")
```

## QSM Radii Correction

Real twig is run as a chain of function that build on one another. The general processing chain is as follows:\
\
1. Import a QSM: TreeQSM and SimpleForest are currently supported.\
2. Update the cylinder data to allow for network analysis and growth length calculations.\
3. Correct the twig radii.\
4. Summarize, visualize (optional QSM smoothing), and export the results.

#### TreeQSM

``` r
 file <- system.file("extdata/QSM.mat", package = "rTwig")
 df <- import_qsm(file)
 df <- update_cylinders(df)
 df <- growth_length(df)
 df <- correct_radii(df, twigRad = 0.003)
 df <- smooth_qsm(df)
 summary(df)
 plot_qsm(df)
```

#### SimpleForest

``` r
 file <- system.file("extdata/QSM.csv", package = "rTwig")
 df <- read.csv(file)
 df <- update_cylinders(df)
 df <- correct_radii(df, twigRad = 0.003)
 df <- smooth_qsm(df)
 summary(df)
 plot_qsm(df)
```

## Box Dimension

The box dimension is the measure of the structural complexity of an object. The box dimension of point clouds can be easily calculated as follows:

``` r
# Calculate Box Dimension
file <- system.file("extdata/cloud.txt", package = "rTwig")
cloud <- read.table(file)
output <- box_dimension(cloud)
output

# Plot Results
data <- output[[1]]
plot(data$log.box.size,
  data$log.voxels,
  pch = 19,
  xlab = "Log(Inverse Box Size)",
  ylab = "log(Box Count)"
)
abline(lm(data$log.voxels ~ data$log.box.size))
```
