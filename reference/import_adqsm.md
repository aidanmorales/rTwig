# Import AdQSM

Imports a QSM created by AdQSM or AdTree

## Usage

``` r
import_adqsm(filename, method = "adqsm")
```

## Arguments

- filename:

  an AdQSM or AdTree .obj file

- method:

  specify the method as `adqsm` or `adtree`. Defaults to `adqsm`.

## Value

Returns a data frame

## References

Fan G, Nan L, Dong Y, Su X, Chen F (2020). “AdQSM: A New Method for
Estimating Above-Ground Biomass from TLS Point Clouds.” *Remote
Sensing*, **12**(18), 3089. Du S, Lindenbergh R, Ledoux H, Stoter J, Nan
L (2019). “AdTree: Accurate, detailed, and automatic modelling of
laser-scanned trees.” *Remote Sensing*, **11**(18), 2074.

## Examples

``` r
if (FALSE) { # \dontrun{

# Import an AdQSM or AdTree
qsm <- import_adqsm("path/to/obj/file")
} # }
```
