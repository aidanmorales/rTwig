# Download Twigs

Download twig measurements from an external database, including
additional, verified measurements from contributors.

## Usage

``` r
download_twigs(database = "twigs")
```

## Arguments

- database:

  Twig database to return. Can be "twigs", "twigs_index", "raw", for the
  raw twig measurements, or "all" for all databases.

## Value

Returns a data frame

## Examples

``` r

download_twigs()
#> Downloading Twig Measurements
#> # A tidytable: 113 × 7
#>    scientific_name      radius_mm   min   max   std     n    cv
#>    <chr>                    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1 Abies concolor            1.43  0.89  1.9   0.28    21  0.19
#>  2 Abies spp.                1.43  0.89  1.9   0.28    21  0.19
#>  3 Acer campestre            1     0.51  1.65  0.28    42  0.28
#>  4 Acer platanoides          1.5   0.64  2.92  0.4     60  0.27
#>  5 Acer pseudoplantanus      1.5   1.27  2.03  0.28     6  0.19
#>  6 Acer rubrum               1.18  0.89  1.52  0.16    30  0.14
#>  7 Acer saccharinum          1.41  0.89  1.9   0.27    14  0.2 
#>  8 Acer saccharum            1.2   0.89  1.65  0.23    30  0.19
#>  9 Acer spp.                 1.3   0.51  2.92  0.29   182  0.22
#> 10 Aesculus flava            2.96  2.29  4.44  0.58    14  0.19
#> # ℹ 103 more rows
```
