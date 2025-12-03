# Tree Metrics

Calculates tree metrics from a QSM

## Usage

``` r
tree_metrics(cylinder, verify = TRUE, triangulation = NULL)
```

## Arguments

- cylinder:

  QSM cylinder data frame

- verify:

  Verify QSM topology. Defaults to TRUE. If `verify = TRUE`, only
  topologically connected structures (e.g. a whole a tree or an
  individual branch) are eligible to run. This ensures all metrics are
  correct and verified. If `verify = FALSE`, the metrics are run, but
  not verified. This is strongly discouraged, but can enable the
  calculation of tree metrics on topologically disconnected structures.

- triangulation:

  Calculate optional QSM triangulation metrics created with
  [`import_treeqsm()`](https://aidanmorales.github.io/rTwig/reference/import_treeqsm.md).
  Only supports TreeQSM. Defaults to `NULL`.

## Value

Returns a list of tree metric data frames and a synthetic point cloud

## Details

Calculates detailed tree, branch, and segment metrics from a QSM. Valid
inputs require a connected QSM, which can be a whole tree or an
individual branch. The outputs include all of the standard outputs from
TreeQSM, and also additional variables, including, but not limited to,
growth length, reverse branch order, branch segment or node
relationships, and distances from twigs and the base of the tree, across
various distribution metrics. Also included is a simulated point cloud
of the tree, based on the QSM cylinder radii. When corrected with Real
Twig, this allows for the testing and validation of point cloud diameter
overestimation throughout the tree.

## References

Raumonen P, Kaasalainen M, Åkerblom M, Kaasalainen S, Kaartinen H,
Vastaranta M, Holopainen M, Disney M, Lewis P (2013). “Fast Automatic
Precision Tree Models from Terrestrial Laser Scanner Data.” *Remote
Sensing*, **5**(2), 491–520.
[doi:10.3390/rs5020491](https://doi.org/10.3390/rs5020491) .

Hackenberg J, Spiecker H, Calders K, Disney M, Raumonen P (2015).
“SimpleTree —An Efficient Open Source Tool to Build Tree Models from TLS
Clouds.” *Forests*, **6**(11), 4245–4294.
[doi:10.3390/f6114245](https://doi.org/10.3390/f6114245) .

Hackenberg J, Bontemps J (2023). “Improving quantitative structure
models with filters based on allometric scaling theory.” *Applied
Geomatics*, **15**.
[doi:10.1007/s12518-023-00537-4](https://doi.org/10.1007/s12518-023-00537-4)
.

Yang W, Wilkes P, Vicari MB, Hand K, Calders K, Disney M (2024).
“Treegraph: tree architecture from terrestrial laser scanning point
clouds.” *Remote Sensing in Ecology and Conservation*. ISSN 2056-3485,
[doi:10.1002/rse2.399](https://doi.org/10.1002/rse2.399) .

Smith D, Sperry J, Enquist B, Savage V, McCulloh K, Bentley L (2013).
“Deviation from symmetrically self-similar branching in trees predicts
altered hydraulics, mechanics, light interception and metabolic
scaling.” *The New phytologist*, **201**.
[doi:10.1111/nph.12487](https://doi.org/10.1111/nph.12487) .

## Examples

``` r
## TreeQSM Processing Chain
file <- system.file("extdata/QSM.mat", package = "rTwig")
cylinder <- import_treeqsm(file)$cylinder
#> Importing TreeQSM
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
metrics <- tree_metrics(cylinder)
#> Calculating Branch Metrics
#> Calculating Segment Metrics
#> Calculating Tree Metrics
#> Generating Point Cloud
#> Calculating Crown Metrics
#> Calculating Tree Distributions
#> Calculating Branch Distributions
#> Calculating Segment Distributions
#> Calculating Stem Taper
#> Calculating Spreads
#> Calculating Alternate Branch Metrics
names(metrics)
#>  [1] "branch"                "segment"               "cloud"                
#>  [4] "tree"                  "crown_convex_hull"     "crown_alpha_shape"    
#>  [7] "tree_height_dist"      "tree_diameter_dist"    "tree_zenith_dist"     
#> [10] "tree_azimuth_dist"     "branch_diameter_dist"  "branch_height_dist"   
#> [13] "branch_angle_dist"     "branch_zenith_dist"    "branch_azimuth_dist"  
#> [16] "branch_order_dist"     "segment_diameter_dist" "segment_height_dist"  
#> [19] "segment_angle_dist"    "segment_zenith_dist"   "segment_azimuth_dist" 
#> [22] "segment_order_dist"    "stem_taper"            "spreads"              
#> [25] "vertical_profile"      "branch_alt"            "version"              
#> [28] "run_date"             

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
metrics <- tree_metrics(cylinder)
#> Calculating Branch Metrics
#> Calculating Segment Metrics
#> Calculating Tree Metrics
#> Generating Point Cloud
#> Calculating Crown Metrics
#> Calculating Tree Distributions
#> Calculating Branch Distributions
#> Calculating Segment Distributions
#> Calculating Stem Taper
#> Calculating Spreads
#> Calculating Alternate Branch Metrics
names(metrics)
#>  [1] "branch"                "segment"               "cloud"                
#>  [4] "tree"                  "crown_convex_hull"     "crown_alpha_shape"    
#>  [7] "tree_height_dist"      "tree_diameter_dist"    "tree_zenith_dist"     
#> [10] "tree_azimuth_dist"     "branch_diameter_dist"  "branch_height_dist"   
#> [13] "branch_angle_dist"     "branch_zenith_dist"    "branch_azimuth_dist"  
#> [16] "branch_order_dist"     "segment_diameter_dist" "segment_height_dist"  
#> [19] "segment_angle_dist"    "segment_zenith_dist"   "segment_azimuth_dist" 
#> [22] "segment_order_dist"    "stem_taper"            "spreads"              
#> [25] "vertical_profile"      "branch_alt"            "version"              
#> [28] "run_date"             
```
