# rTwig 1.2.0

## New Features

-   `cluster_cloud()`: A New function to transfer QSM variables to the input point cloud or simulate a point cloud from the QSM. Efficient nearest neighbor searching is done with the C++ *nanoflann* library: <https://github.com/jlblancoc/nanoflann>

-   `prune_qsm()`: A new function to efficiently and easily prune QSMs using multiple pruning criteria and return options.

-   aRchi QSMs are now supported in the package.

-   Users can now pass radius or color column variables either unquoted or quoted to user facing functions like `plot_qsm()`, `qsm_summary()`, or `export_mesh()`. Piping the QSM cylinders into these functions allows the user to easily select unquoted columns.

-   `update_cylinders()`: Three new allometric scaling variables as standard outputs - *vessel_volume*, *pipe_area*, *pipe_radius*.

-   `plot_qsm()`: Point clouds and stem triangulation meshes can now be plotted independently of a QSM.

    -   *triangulation*: The user can now plot TreeQSM triangulation meshes by passing in the triangulation list from `import_qsm()`. The user can also control the color and palette with *tri_color* and *tri_palette*.
    -   *pan*: The right mouse button now pans the plot instead of zooming by default. The functionality mimics `pan3d()` from RGL, but rewritten with Rcpp for better responsiveness.
    -   *lit*: Enable or disable plot lighting
    -   *alpha*: Set cylinder transparency. Note: can degrade performance with large numbers of cylinders or facets.
    -   *axes_color*: The axes color can now be set by the user.
    -   *skeleton_lwd*: Set the line width of the skeleton plots.
    -   *normalize*: Center the cylinders around 0,0,0. Defaults to FALSE.

-   `correct_radii()`:

    -   The broken branch filter can now be disabled, with *broken_branch = FALSE*

    -   Now returns *modified*, an index of what cylinders were modified by Real Twig.

-   `run_rtwig()`: The broken branch filter can now be disabled, with *broken_branch = FALSE*

## Improvements

-   `correct_radii()`: Complete refactor with Rcpp and tidyeval for better maintainability.
    -   \~ 2 - 3x performance improvement!
    -   The outlier and broken branch filters are rewritten in Rcpp, for massive performance gains.
    -   Parallel processing has been removed, eliminating overhead and excessive memory usage.
-   `update_cylinders()`: Fix a bug in path metrics for SimpleForest QSMs (issue #4).
-   `qsm_summary()`:
    -   Refactored with tidyeval.
    -   Fix bug where triangulation was not using the correct ending cylinder.
-   `box_dimension()`: The 3D plot now pans on right mouse button.
-   `run_rtwig()`: Fix missing Treegraph parameter (issue #5).
-   `tree_metrics()`:
    -   Fix a bug where the start x,y,z values were incorrect.
    -   Fix a bug where NA cylinder orientation values would cause the calculations to fail.
    -   NA values are removed and the user sees a warning promoting them to check the QSM.
-   The QSM network is now cached in the temp folder to avoid repeat calculations.
-   Data checking and helpful error messages are now included for every function.
-   Add the Real Twig Method paper citation in the package and readme.
-   New website look.
-   Update vignettes.
-   Update twig data base.
-   Reduced package dependencies.

## Breaking Changes

-   `qsm_summary()`: User can pass *radius* as either a quoted or unquoted column name, so there is no longer any default value. Failing to provide a radius will throw an error prompting the user for a radius column name.
-   `correct_radii()`: parallel processing has been removed, so *backend* is no longer a parameter.
-   `run_rtwig()`:parallel processing has been removed, so *backend* is no longer a parameter.
-   *file* has been changed to *filename* in `run_rtwig()`, `import_qsm()`, and `import_treegraph()` to be consistent with other functions.

# rTwig 1.1.0

## New Features

-   `run_rtwig()`: New function to run the Real Twig method and calculate tree metrics in one simple function.

-   `standardize_qsm()`: New function to standardize variable names across all supported QSM software. See the dictionary vignette for more details on the variable names.

-   `tree_metrics()`: New function to calculate all the standard outputs of TreeQSM, plus new variables, metrics, and simulated point clouds reconstructed entirely from the QSM. Computationally expensive tasks are implemented with Rcpp for maximum performance.

-   `import_treegraph()`: New function to import Treegraph QSM. Treegraph support is implemented throughout the package!

-   `update_cylinders()`: New variables: *distanceFromBase*, *distanceToTwig*, *branch_alt.* QSM topology is now verified and corrected if there are any topological errors.

-   `plot_qsm()`

    -   Complete refactor with Rcpp and tidyeval for massive performance improvements
    -   New QSM skeleton plotting option
    -   Supports plotting multiple QSMs in the same plot
    -   Cylinder and cloud colors can now be set to *random*
    -   *bg_color* can change the background color of the plot

-   `export_mesh()`

    -   Complete refactor with Rcpp and tidyeval for massive performance improvements
    -   Cylinder *color* parameter can now be set to *random*, or *FALSE* to disable exporting colors
    -   *Note: color exporting will be fixed in a future rgl package update*

## Improvements

-   Implement Rcpp across package

    -   `plot_qsm()` \> 10x performance improvement!

    -   `smooth_qsm()` \> 10x performance improvement!

    -   `export_mesh()` \> 10x performance improvement!

    -   `box_dimension()` \~ 3x performance improvement

-   `update_cylinder()`

    -   \~2x performance improvement with better vectorization

    -   Cleaned variable names and removed repeat calculations

    -   Verify and correct QSM topology

-   Reduced package dependencies

-   Cleanup function imports

-   Update twig data base

-   Update vignettes

-   Update citations

## Breaking Changes

-   All parent child cylinder IDs, nodes, and branches begin numbering at 1 to ensure compatibility with R indexing and igraph. This affects SimpleForest and Treegraph.

-   Redefined SimpleForest branch structure to begin at a new branch order and end in a twig. This ensures consistency in the code and calculations between TreeQSM, Treegraph, and SimpleForest. The old branch definition is saved as a new variable called *branch_alt*, and is changed to begin at zero to be consistent with the new *branch_alt* variable from Real Twig.

-   Renamed *twigRad* function parameter to *twig_radius* to be consistent with tidy style guidelines

-   Renamed *GrowthLength* to *growthLength* to be consistent with SimpleForest

-   Variable names in `qsm_summary()` updated to be explicit and consistent with the outputs of `tree_metrics()`

-   `plot_qsm()` now takes input column parameters as quoted variables. *cyl_color* and *cyl_palette* have been changed to *color* and *palette* respectively. *cyl_sides* has been renamed to *facets*.

-   `export_mesh()` now takes input column parameters as quoted variables. *cyl_color* and *cyl_palette* have been changed to *color* and *palette* respectively. *cyl_sides* has been renamed to *facets*.

-   `plot_stand()` has been deprecated as all of its functionality is now incorporated into `plot_qsm()`

-   *twigs*: renamed *scientific.name* to *scientific_name* and *radius.mm* to *radius_mm* for consistent naming conventions across the package.

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
