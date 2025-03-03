# rTwig 1.4.0

## New Features

-   `import_leaves()`: a new function to import leaves from [QSM-FaNNI](https://github.com/InverseTampere/qsm-fanni-matlab). The leaves are stored as an `rgl::mesh3d()` object.
-   `export_mesh()`: The function now supports multiple 3d mesh formats including, `.ply`, `.obj` and `.stl`. It also supports exporting a QSM to [Blender](https://www.blender.org/) using the [QSM Blender Addons](https://github.com/InverseTampere/qsm-blender-addons) format `.txt`.
-   New database called `twigs_index`. This is based on qualitative size classes supported by the literature, making it easier to select a twig radius for tree species without twig measurements. More info can be found in the `Twigs` vignette.
-   `plot_qsm()`:
    -   Leaf meshes from `import_leaves()` or `aRchi::add_leaves()` can now be plotted with the `leaves` parameter. Leaf color and transparency can be controlled with `lf_color` and `lf_alpha` respectively.
    -   Now accepts random color palettes for cylinders, points, or triangulation meshes by setting the `*_palette = "random"`. Additionally, the user can set a random color for each cylinder by setting both `color` and `palette` to `"random"`.
    -   Transparency options added for point clouds and main stem triangulation mesh with `pt_alpha` and `tri_alpha`.
    -   Point clouds can now be colored by any variable or palette using `pt_color` and `pt_palette`. The user can set a random color for each point by setting both `pt_color` and `pt_palette` to `random`.
    -   Any `rgl::mesh3d()` objects can now be passed to `plot_qsm()` with the `mesh` parameter.
-   `tree_metrics()`:
    -   `modified` is a standard output in the `tree` data frame. This is an average of the binary index of the cylinders modified by Real Twig. The `rTwig` version and run date are now also saved as `version` and `run_date`.

    -   `volume_change_pct` and `area_change_pct` are standard outputs of the `tree` data frame. These represent the percent change in total tree volume and surface area using `radius` and `raw_radius`.

    -   `vessel_volume`, `pipe_area`, `pipe_radius`, and `twig_distance_m` are standard outputs taken from the base of the QSM.

    -   Added first order branches to the branch distribution metrics to better match the output of TreeQSM. These columns include `_1` in their column names.

    -   `path_fraction` is now a standard output.

## Improvements

-   `export_mesh()`: The function was entirely reworked using `Rcpp` for fast and efficient mesh export. All functionality that was dependent on `rgl` has been implemented in custom C++ functions.

-   Function names now use British English to be consistent with R standards. American spelling will still work interchangeably (e.g. `standardize_qsm()` vs `standardise_qsm()`.

-   Some functions have been renamed to follow tidy guidelines and have consistency within rTwig and other R packages (e.g. `qsm_summary()` -\> `summarise_qsm()`).

-   Fixed a bug in `plot_qsm()`, where certain variable names conflicted with internal functions, resulting in an error (issue #12).

-   Fixed multiple bugs in `tree_metrics()`:

    -   Empty azimuth and zenith factors are now filled with 0, instead of being missing.

    -   Issue #15 occurred because the cylinder verification was at too high a level and could not properly create the cylinder network when an error condition was met.

    -   Fixed an issue when only a single branch or segment was provided as filtered data, causing the calculations to fail. The user is now show a warning when these edge cases occur.

    -   Fixed an issue where crown base height calculations would fail if there was only one first order branch present.

    -   Fixed an issue where disconnected data using `verify = FALSE` would fail crown base calculations due to missing branches (issue #18).

-   `run_rtwig()`: The user can now use `standardize` and `standardise` interchangeably.

-   Updated `Twigs` vignette with new info and plots.

-   Updated `Dictionary` vignette describing all standard outputs from `tree_metrics()`.

## Breaking Changes

-   `import_qsm()` is deprecated and has been replaced with `import_treeqsm()`. The functionality is unchanged, but the name changed to be explicit about its use. `import_qsm()` still functions, but will be removed in a future rTwig release.

-   `qsm_summary()` is deprecated and has been replaced with `summarise_qsm()`. The functionality is unchanged, but the name changed to be consistent with tidy guidelines. `qsm_summary()` still functions, but will be removed in a future rTwig release.

# rTwig 1.3.0

## New Features

-   `tree_metrics()` now outputs the base diameters as a standard output in the `tree` data frame. The new variables are `d_base_qsm_cm` and `d_base_raw_cm`, which are the modified and unmodified diameter of the base of the tree or branch.

## Improvements

-   `tree_metrics()` is now dynamic and can accept filtered data. The QSM must be a connected structure (e.g. a whole tree or single branch). If the main stem is shorter than DBH (1.37 m), DBH will be NA, and the user will be notified. If DBH is NA, the base diameter will be used in calculations instead. If a branch is provided, the lowest order will be considered as the main stem, and the standard summary will be calculated. Previously, trying to summarize any filtered data would throw an error (issue #8).

-   `qsm_summary()` is now dynamic and can accept filtered data. If the filtered QSM data is a connected structure (e.g. a branch), the lowest order will be considered as the main stem, and the standard summary will be calculated. If the QSM data is disconnected (e.g. several pruned branches), only total volume and surface area will be calculated, and the user will be notified. Additionally, DBH calculations are also dynamic. If the main stem of a tree or branch is shorter than DBH (1.37 m), DBH will be NA, and the user will be notified. Previously, trying to summarize any filtered data would throw an error (issue #7).

-   Fixed a bug in `plot_qsm()` where color vectors or a vector element would throw an unintended error (issue #9).

-   Fixed a bug in `update_cylinders()` where `distanceFromBase` was being joined with the wrong index values (issue #10).

-   Fix an issue for CRAN, where Fedora with the clang compiler threw an error.

-   Random colors are now consistently brighter.

-   Update Validation vignette with more data.

-   Add start-up message on package load.

-   Polish up documentation.

# rTwig 1.2.0

## New Features

-   `cluster_cloud()`: A new function to transfer QSM variables to the input point cloud or simulate a point cloud from the QSM. Efficient nearest neighbor searching is done with the C++ *nanoflann* library: <https://github.com/jlblancoc/nanoflann>

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
