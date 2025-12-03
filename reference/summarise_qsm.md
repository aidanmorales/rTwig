# Summarise QSM

Generates a simple QSM summary. The summary includes basic tree metrics,
such as DBH, total height, and also volume and surface area arranged by
totals and branching order.

## Usage

``` r
summarise_qsm(cylinder, radius, triangulation = NULL)

summarize_qsm(cylinder, radius, triangulation = NULL)
```

## Arguments

- cylinder:

  QSM cylinder data frame

- radius:

  Radius column name either quoted or unquoted.

- triangulation:

  Calculate optional QSM triangulation metrics created with
  [`import_treeqsm()`](https://aidanmorales.github.io/rTwig/reference/import_treeqsm.md).
  Only supports TreeQSM. Defaults to `NULL`.

## Value

Returns a list

## Details

`summarise_qsm()` and `summarize_qsm()` are synonyms.

## Examples

``` r
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
summarise_qsm(cylinder, radius)
#> Creating QSM Summary
#> [[1]]
#> # A tidytable: 5 × 3
#>   branch_order tree_volume_L tree_area_m2
#>          <int>         <dbl>        <dbl>
#> 1            0       12.9          0.768 
#> 2            1        8.70         1.07  
#> 3            2        6.46         0.871 
#> 4            3        1.82         0.298 
#> 5            4        0.0723       0.0148
#> 
#> [[2]]
#> # A tidytable: 1 × 8
#>   dbh_qsm_cm tree_height_m stem_volume_L branch_volume_L tree_volume_L
#>        <dbl>         <dbl>         <dbl>           <dbl>         <dbl>
#> 1       7.39          3.67          12.9            17.1          30.0
#> # ℹ 3 more variables: stem_area_m2 <dbl>, branch_area_m2 <dbl>,
#> #   tree_area_m2 <dbl>
#> 

# TreeQSM Triangulation
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
summarise_qsm(cylinder, radius, triangulation = qsm$triangulation)
#> Creating QSM Summary
#> [[1]]
#> # A tidytable: 6 × 3
#>   branch_order tree_volume_L tree_area_m2
#>          <int>         <dbl>        <dbl>
#> 1            0       10.8         0.644  
#> 2            1        5.80        0.827  
#> 3            2        3.86        0.732  
#> 4            3        1.06        0.237  
#> 5            4        0.622       0.0785 
#> 6            5        0.0346      0.00693
#> 
#> [[2]]
#> # A tidytable: 1 × 8
#>   dbh_qsm_cm tree_height_m stem_volume_L branch_volume_L tree_volume_L
#>        <dbl>         <dbl>         <dbl>           <dbl>         <dbl>
#> 1       7.18          3.69          10.8            11.4          22.2
#> # ℹ 3 more variables: stem_area_m2 <dbl>, branch_area_m2 <dbl>,
#> #   tree_area_m2 <dbl>
#> 
#> [[3]]
#> # A tidytable: 1 × 8
#>   dbh_tri_cm tri_volume_L stem_mix_volume_L tree_mix_volume_L tri_area_m2
#>        <dbl>        <dbl>             <dbl>             <dbl>       <dbl>
#> 1       6.13         24.7              28.7              40.1       0.320
#> # ℹ 3 more variables: stem_mix_area_m2 <dbl>, tree_mix_area_m2 <dbl>,
#> #   tri_length_m <dbl>
#> 
```
