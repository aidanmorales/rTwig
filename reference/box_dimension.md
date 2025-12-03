# Box Dimension

R port of Dominik Seidel's fractal analysis "box-dimension" metric.

## Usage

``` r
box_dimension(cloud, lowercutoff = 0.01, rm_int_box = FALSE, plot = FALSE)
```

## Arguments

- cloud:

  A point cloud matrix size n x 3. Non-matrices are automatically
  converted to a matrix.

- lowercutoff:

  The smallest box size determined by the point spacing of the cloud in
  meters. Defaults to 1 cm.

- rm_int_box:

  Remove the initial box as TRUE or FALSE. Defaults to FALSE.

- plot:

  Plot the results. The user can specify "2D", "3D", or "ALL" plots.
  FALSE disables plotting. Defaults to FALSE.

## Value

Returns a list

## References

Arseniou G, MacFarlane DW, Seidel D (2021). “Measuring the Contribution
of Leaves to the Structural Complexity of Urban Tree Crowns with
Terrestrial Laser Scanning.” *Remote Sensing*, **13**(14).
[doi:10.3390/rs13142773](https://doi.org/10.3390/rs13142773) .

Mandelbrot BB (1983). *The fractal geometry of nature*. Freeman.

Saarinen N, Calders K, Kankare V, Yrttimaa T, Junttila S, Luoma V,
Huuskonen S, Hynynen J, Verbeeck H (2021). “Understanding 3D structural
complexity of individual Scots pine trees with different management
history.” *Ecology and Evolution*, **11**(6), 2561-2572.
[doi:10.1002/ece3.7216](https://doi.org/10.1002/ece3.7216) .

Seidel D (2018). “A holistic approach to determine tree structural
complexity based on laser scanning data and fractal analysis.” *Ecology
and Evolution*, **8**(1), 128-134.
[doi:10.1002/ece3.3661](https://doi.org/10.1002/ece3.3661) .

Seidel D, Annighöfer P, Stiers M, Zemp CD, Burkardt K, Ehbrecht M,
Willim K, Kreft H, Hölscher D, Ammer C (2019). “How a measure of tree
structural complexity relates to architectural benefit-to-cost ratio,
light availability, and growth of trees.” *Ecology and Evolution*,
**9**(12), 7134-7142.
[doi:10.1002/ece3.5281](https://doi.org/10.1002/ece3.5281) .

## Examples

``` r
## Calculate Box Dimension
file <- system.file("extdata/cloud.txt", package = "rTwig")
cloud <- read.table(file, header = FALSE)
output <- box_dimension(cloud, plot = "ALL")

#> Panning plot on rgl device: 1
output
#> [[1]]
#> # A tidytable: 9 × 2
#>   log.box.size log.voxels
#>          <dbl>      <dbl>
#> 1        0           1.10
#> 2        0.693       1.95
#> 3        1.39        3.09
#> 4        2.08        4.11
#> 5        2.77        5.18
#> 6        3.47        6.14
#> 7        4.16        7.16
#> 8        4.85        8.23
#> 9        5.55        9.27
#> 
#> [[2]]
#> # A tidytable: 1 × 4
#>   r.squared adj.r.squared intercept slope
#>       <dbl>         <dbl>     <dbl> <dbl>
#> 1     1.000         1.000      1.02  1.48
#> 
```
