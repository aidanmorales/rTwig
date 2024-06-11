# rTwig (development version)

## New Features

-   `tree_metrics()`: New function to calculate all the standard outputs of TreeQSM in R, plus new variables, metrics, and simulated point clouds reconstructed entirely from the QSM.

-   `import_treegraph()`: New function to import Treegraph QSM. Treegraph support is implemeted throughout package! However, the Real Twig correction is not implemented yet, as Treegraph QSMs needs to be validated against reference data.

-   `update_cylinders()`

    -   Two new variables: *distanceFromBase* and *distanceToTwig*

-   `plot_qsm()`

    -   New QSM skeleton option for faster plotting

## Breaking Changes

-   All parent child cylinder IDs, nodes, and branches begin numbering at 1 to ensure compatibility with R indexing and igraph. This affects SimpleForest and Treegraph.

-   Redefined SimpleForest branch structure to begin at a new branch order and end in a twig. This ensures consistency in the code and calculations between TreeQSM, Treegraph, and SimpleForest. The old branch definition is saved as a new variable called branchOld.

-   Renamed *twigRad* function parameter to *twig_radius* to be consistent with tidy style guidelines

-   Renamed *GrowthLength* to *growthLength* to be consistent with SimpleForest

-   Variable names in `qsm_summary()` updated to be explicit and consistent with the outputs of `tree_metrics()`

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
