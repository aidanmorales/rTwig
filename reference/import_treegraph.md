# Import Treegraph

Imports a QSM created by treegraph

## Usage

``` r
import_treegraph(filename)
```

## Arguments

- filename:

  a treegraph .json file

## Value

Returns a list

## References

Yang W, Wilkes P, Vicari MB, Hand K, Calders K, Disney M (2024).
“Treegraph: tree architecture from terrestrial laser scanning point
clouds.” *Remote Sensing in Ecology and Conservation*. ISSN 2056-3485,
[doi:10.1002/rse2.399](https://doi.org/10.1002/rse2.399) .

Wilkes P, Shenkin A, Disney M, Malhi Y, Bentley LP, Vicari MB (2021).
“Terrestrial laser scanning to reconstruct branch architecture from
harvested branches.” *Methods in Ecology and Evolution*, **12**,
2487-2500.
[doi:10.1111/2041-210X.13709](https://doi.org/10.1111/2041-210X.13709) .

## Examples

``` r
if (FALSE) { # \dontrun{

# Import a treegraph QSM
qsm <- import_treegraph("path/to/json/file")
} # }
```
