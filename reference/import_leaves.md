# Import Leaves

Imports leaves created by the QSM foliage and needles naive insertion
(QSM-FaNNI) algorithm. The leaves are stored as a triangular mesh,
compatible with both rTwig and RGL functions.

## Usage

``` r
import_leaves(filename, format = "obj", simplify = FALSE)
```

## Arguments

- filename:

  a QSM-FaNNI .obj file

- format:

  .obj file format. Valid formats include `obj` and `obj_ext`. Defaults
  to `obj`.

- simplify:

  simplify the mesh by removing duplicate vertices. This can reduce
  memory usage at the cost of import speed. Defaults to FALSE.

## Value

rgl::mesh3d object

## References

Åkerblom M, Raumonen P, Casella E, Disney MI, Danson FM, Gaulton R,
Schofield LA, Kaasalainen M (2018). “An algorithm to insert leaves into
quantitative structure models of trees.” *Interface Focus*, **8**(2),
20170045. ISSN 2042-8898,
[doi:10.1098/rsfs.2017.0045](https://doi.org/10.1098/rsfs.2017.0045) ,
20180216.

## Examples

``` r
if (FALSE) { # \dontrun{

## Import FaNNI leaves from the standard obj format
file <- "path_to_leaves.obj"
qsm <- import_leaves(file, format = "obj")

## Import FaNNI leaves from the extended obj format
file <- "path_to_leaves_extended.obj"
qsm <- import_leaves(file, format = "obj_ext")
} # }
```
