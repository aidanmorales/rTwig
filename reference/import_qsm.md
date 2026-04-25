# Import QSM

Imports a QSM created by TreeQSM, Treegraph, AdQSM, AdTree, or SmartQSM

## Usage

``` r
import_qsm(filename)
```

## Arguments

- filename:

  a QSM file. Accepts `.mat` (TreeQSM, SmartQSM), `.json` (Treegraph),
  or `.obj` (AdQSM, AdTree)

## Value

Returns a list

## References

Raumonen P, Kaasalainen M, Åkerblom M, Kaasalainen S, Kaartinen H,
Vastaranta M, Holopainen M, Disney M, Lewis P (2013). “Fast Automatic
Precision Tree Models from Terrestrial Laser Scanner Data.” *Remote
Sensing*, **5**(2), 491–520.
[doi:10.3390/rs5020491](https://doi.org/10.3390/rs5020491) . Yang W,
Wilkes P, Vicari MB, Hand K, Calders K, Disney M (2024). “Treegraph:
tree architecture from terrestrial laser scanning point clouds.” *Remote
Sensing in Ecology and Conservation*. ISSN 2056-3485.
[doi:10.1002/rse2.399](https://doi.org/10.1002/rse2.399) . Wilkes P,
Shenkin A, Disney M, Malhi Y, Bentley LP, Vicari MB (2021). “Terrestrial
laser scanning to reconstruct branch architecture from harvested
branches.” *Methods in Ecology and Evolution*, **12**, 2487-2500.
[doi:10.1111/2041-210X.13709](https://doi.org/10.1111/2041-210X.13709) .
Fan G, Nan L, Dong Y, Su X, Chen F (2020). “AdQSM: A New Method for
Estimating Above-Ground Biomass from TLS Point Clouds.” *Remote
Sensing*, **12**(18), 3089. Du S, Lindenbergh R, Ledoux H, Stoter J, Nan
L (2019). “AdTree: Accurate, detailed, and automatic modelling of
laser-scanned trees.” *Remote Sensing*, **11**(18), 2074. Yang J, Zhang
H, Li J, Yang H, Gao T, Yang T, Wang J, Zhang X, Yun T, Duanmu Y, Chen
S, Shi Y (2026). “SmartQSM: a novel quantitative structure model using
sparse-convolution-based point cloud contraction for reconstruction and
analysis of individual tree architecture.” *ISPRS Journal of
Photogrammetry and Remote Sensing*, **232**, 712-739. ISSN 0924-2716.
[doi:10.1016/j.isprsjprs.2026.01.011](https://doi.org/10.1016/j.isprsjprs.2026.01.011)
. <https://www.sciencedirect.com/science/article/pii/S0924271626000110>.

## Examples

``` r
# Read a TreeQSM MATLAB file
file <- system.file("extdata/QSM.mat", package = "rTwig")
qsm <- import_qsm(file)
#> Importing TreeQSM
summary(qsm)
#>               Length Class      Mode
#> cylinder      17     data.frame list
#> branch        10     data.frame list
#> treedata      91     -none-     list
#> rundata       46     data.frame list
#> pmdistance    21     -none-     list
#> triangulation 12     -none-     list
```
