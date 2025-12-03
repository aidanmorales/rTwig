# Import TreeQSM

Imports a QSM created by TreeQSM

## Usage

``` r
import_treeqsm(filename, version = "2.x.x")
```

## Arguments

- filename:

  a TreeQSM .mat MATLAB file

- version:

  TreeQSM version. Valid versions include `2.3.0` - `2.4.1` and `2.0`.
  Defaults to the most recent format of `2.x.x`.

## Value

Returns a list

## References

Raumonen P, Kaasalainen M, Åkerblom M, Kaasalainen S, Kaartinen H,
Vastaranta M, Holopainen M, Disney M, Lewis P (2013). “Fast Automatic
Precision Tree Models from Terrestrial Laser Scanner Data.” *Remote
Sensing*, **5**(2), 491–520.
[doi:10.3390/rs5020491](https://doi.org/10.3390/rs5020491) .

## Examples

``` r
## Read a TreeQSM MATLAB file in the 2.3.x - 2.4.x format
file <- system.file("extdata/QSM.mat", package = "rTwig")
qsm <- import_treeqsm(file, version = "2.x.x")
#> Importing TreeQSM
summary(qsm)
#>               Length Class      Mode
#> cylinder      17     data.frame list
#> branch        10     data.frame list
#> treedata      91     -none-     list
#> rundata       45     data.frame list
#> pmdistance    21     -none-     list
#> triangulation 12     -none-     list

## Read a TreeQSM MATLAB file in the 2.0 format
file <- system.file("extdata/QSM_2.mat", package = "rTwig")
qsm <- import_treeqsm(file, version = "2.0")
#> Importing TreeQSM
names(qsm)
#> [1] "cylinder" "treedata"
```
