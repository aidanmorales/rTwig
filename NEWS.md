# rTwig (development version)

## New Features

-   `standardize_qsm()`: New function to standardize variable names across all supported QSM software. See the dictionary vignette for more details on the variable names. 

-   `tree_metrics()`: New function to calculate all the standard outputs of TreeQSM, plus new variables, metrics, and simulated point clouds reconstructed entirely from the QSM. Computationally expensive tasks are implemented with Rcpp for maximum performance.

-   `import_treegraph()`: New function to import Treegraph QSM. Treegraph support is implemented throughout the package!

-   `update_cylinders()`

    -   Two new variables: *distanceFromBase* and *distanceToTwig*

-   `plot_qsm()`

    -   Complete refactor with Rcpp and tidyeval for massive performance improvements
    -   New QSM skeleton plotting option
    -   Supports plotting multiple QSMs in the same plot
    -   Cylinder and cloud colors can now be set to *random*
    -   *bg_color* can change the background color of the plot

-   `export_mesh()`

    -   Complete refactor with Rcpp and tidyeval for massive performance improvements
    -   Cylinder *color* parameter can now be set to *random*, or *FALSE* to disable exporting colors

## Breaking Changes

-   All parent child cylinder IDs, nodes, and branches begin numbering at 1 to ensure compatibility with R indexing and igraph. This affects SimpleForest and Treegraph.

-   Redefined SimpleForest branch structure to begin at a new branch order and end in a twig. This ensures consistency in the code and calculations between TreeQSM, Treegraph, and SimpleForest. The old branch definition is saved as a new variable called branchOld.

-   Renamed *twigRad* function parameter to *twig_radius* to be consistent with tidy style guidelines

-   Renamed *GrowthLength* to *growthLength* to be consistent with SimpleForest

-   Variable names in `qsm_summary()` updated to be explicit and consistent with the outputs of `tree_metrics()`

-   `plot_qsm()` now takes input column parameters as quoted variables. *cyl_color* and *cyl_palette* have been changed to *color* and *palette* respectively. *cyl_sides* has been renamed to *facets*.

-   `export_mesh()` now takes input column parameters as quoted variables. *cyl_color* and *cyl_palette* have been changed to *color* and *palette* respectively. *cyl_sides* has been renamed to *facets*.

-   `plot_stand()` has been deprecated as all of its functionality is now incorporated into `plot_qsm()`

## Improvements

-   Implement Rcpp across package

    -   `plot_qsm()` \> 10x performance improvement!
    
    -   `export_mesh()` \> 10x performance improvement!

    -   `box_dimension()` \~ 3x performance improvement

-   `update_cylinder()`

    -   \~2x performance improvement with better vectorization

    -   Cleaned variable names and removed repeat calculations

-   Reduced package dependencies

-   Cleanup function imports

-   Update twig data base

-   Update vignettes

-   Update citations

# rTwig 1.0.2

-   Changes for CRAN re-submission
    -   Reduced package size
    -   Allow user to run all examples
    -   Small text formatting changes
-   Vignette and webpage typo fixes
-   Improved treedata import in `import_qsm()`

# rTwig 1.0.1

-   Initial CRAN Submission

# rTwig 1.0.0

-   Initial release of Real Twig!
