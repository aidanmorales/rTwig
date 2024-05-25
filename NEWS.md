# rTwig (development version)

-   *TreeQSM* & *TreeGraph*
    -   Renamed *GrowthLength* to *growthLength* to be consistent with *SimpleForest*
-   `import_treegraph()`
    -   New function to import *treegraph* QSMs
-   `update_cylinders()`
    -   \~2x performance improvement with better vectorization
    -   Refactor with tidyeval for better maintainability
    -   Cleaned variable names and removed repeat calculations
    -   New variables: *distanceFromBase* and *distanceToTwig*
    -   Added *treegraph* support
-   `plot_qsm()`
    -   New QSM skeleton option for faster plotting
    -   Added *treegraph* support
-   Reduced package dependencies
-   Cleanup function imports
-   Update twig data base
-   Update vignettes

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
