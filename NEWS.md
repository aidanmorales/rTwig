# rTwig (development version)

## New features

-   `tree_metrics()`: New function to calculate all the standard outputs of TreeQSM in R, plus new variables, metrics, and simulated point clouds built entirely from the QSM. TreeQSM is currently supported, with SimpleForest and Treegraph coming very soon!

-   `import_treegraph()`: New function to import Treegraph QSMs

-   `update_cylinders()`

    -   Add Treegraph support

    -   Two new variables: *distanceFromBase* and *distanceToTwig*

-   `plot_qsm()`

    -   New QSM skeleton option for faster plotting
    -   Added Treegraph support

## Breaking Changes

-   Renamed *twigRad* to *twig_radius* to be consistent with tidy style guidelines

-   Renamed *GrowthLength* to *growthLength* to be consistent with SimpleForest

## Improvements

-   Implement Rcpp across package

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
