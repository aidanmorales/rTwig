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
#> # A tidytable: 104 × 7
#>    scientific_name  radius_mm   min   max   std     n    cv
#>    <chr>                <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1 Abies concolor        1.43  0.89  1.9   0.28    21  0.19
#>  2 Abies spp.            1.43  0.89  1.9   0.28    21  0.19
#>  3 Acer platanoides      1.39  0.89  2.03  0.3     30  0.21
#>  4 Acer rubrum           1.18  0.89  1.52  0.16    30  0.14
#>  5 Acer saccharinum      1.41  0.89  1.9   0.27    14  0.2 
#>  6 Acer saccharum        1.2   0.89  1.65  0.23    30  0.19
#>  7 Acer spp.             1.29  0.89  2.03  0.23   104  0.18
#>  8 Aesculus flava        2.96  2.29  4.44  0.58    14  0.19
#>  9 Aesculus spp.         2.96  2.29  4.44  0.58    14  0.19
#> 10 Betula nigra          0.85  0.51  1.52  0.23    30  0.27
#> # ℹ 94 more rows
```
