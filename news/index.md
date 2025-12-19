# Changelog

## rTwig (development version)

### New Features

- The latest versions of AdQSM (v1.7.5) and AdTree (v1.1.2) are now
  supported in the package.
  [`import_adqsm()`](https://aidanmorales.github.io/rTwig/reference/import_adqsm.md)
  can import an AdQSM or AdTree from its `.obj` file, making them
  immediately usable with the other package functions, and does not
  require
  [`update_cylinders()`](https://aidanmorales.github.io/rTwig/reference/update_cylinders.md)
  to be run.

- [`reconstruct_qsm()`](https://aidanmorales.github.io/rTwig/reference/reconstruct_qsm.md):
  New function to reconstruct a QSM and all of its variables from the
  minimum amount of required data. This enables the reconstruction and
  analysis of generic QSMs from external software or even manual
  measurements. The only required variables are the cylinder id, parent
  id, 3d information (e.g. a combination of start and end points, or
  start, axis, and length), and the radius.

- [`export_mesh()`](https://aidanmorales.github.io/rTwig/reference/export_mesh.md):
  Add [GroIMP](https://grogra.de/) as a QSM export format with
  `format = "groimp"` (issue
  [\#21](https://github.com/aidanmorales/rTwig/issues/21)).

- [`download_twigs()`](https://aidanmorales.github.io/rTwig/reference/download_twigs.md):
  New function to download an expanded twig database containing both the
  package and user contributed data. Both the raw and summarised data
  can be downloaded, including data on the country, region, and
  contributor of the measurements.

### Improvements

- [`export_mat()`](https://aidanmorales.github.io/rTwig/reference/export_mat.md)
  now exports the full TreeQSM structure, improving compatibility with
  both TreeQSM and other R packages depending on the `.mat` format. All
  tree metrics are automatically calculated by setting `metrics = TRUE`.
  Additionally, `pmdistance`, `rundata`, and `triangulation` can all be
  re-exported.

- [`tree_metrics()`](https://aidanmorales.github.io/rTwig/reference/tree_metrics.md)

  - An alpha shape is now calculated for the crown and the 3d mesh is
    saved as `crown_alpha_shape`. New exported tree variables include:

    - `crown_projected_alpha_area_m2`: the projected surface area of the
      crown

    - `crown_alpha_area_m2`: surface area of the crown

    - `crown_alpha_volume_m3` the volume of the crown

  - The 3d mesh for the crown convex hull is now returned as
    `crown_convex_hull`.

  - Better support for TreeQSM triangulation data. If triangulation data
    is supplied, the corresponding metrics are now stored in an
    additional data frame called `triangulation`.

  - Fixed a bug where `crown_volume_m3` had the wrong units.

  - New metrics for paths and branches:

    - `*_length_mean_m`
    - `*_length_min_m`
    - `*_length_max_m`
    - `*_length_sd_m`
    - `*_length_gini`

- [`summarise_qsm()`](https://aidanmorales.github.io/rTwig/reference/summarise_qsm.md)
  better supports TreeQSM triangulation data. If triangulation data is
  supplied, the corresponding metrics are now stored in an additional
  data frame, and all triangulation data has been removed from the
  initial summary and is only based on the cylinders.

- [`plot_qsm()`](https://aidanmorales.github.io/rTwig/reference/plot_qsm.md)
  now uses a dark theme by default for better cylinder and point cloud
  contrast.

- Users can now pass column names by `.$` and `.[[]]` notation to all
  `rTwig` functions when piping data.

- Fixed a bug in
  [`cluster_cloud()`](https://aidanmorales.github.io/rTwig/reference/cluster_cloud.md)
  where the simulated point cloud cylinders were not rotated correctly
  when internally calling `generate_cloud()`.

- Fixed a bug where the mesh for axis aligned cylinders would not
  calculate correctly. This affected both
  [`plot_qsm()`](https://aidanmorales.github.io/rTwig/reference/plot_qsm.md)
  and
  [`export_mesh()`](https://aidanmorales.github.io/rTwig/reference/export_mesh.md).

- Fixed a bug in
  [`tree_metrics()`](https://aidanmorales.github.io/rTwig/reference/tree_metrics.md)
  where the `diameter_mid_cm` column was returning the radius instead of
  the diameter.

- Fixed a bug in the internal `build_network()` function, where the
  named child network would sometimes fail to build with \>100k
  individual cylinders.

- Fixed a bug in
  [`update_cylinders()`](https://aidanmorales.github.io/rTwig/reference/update_cylinders.md)
  where an unintended index column was being returned with the path
  metrics.

- Updated the defaults for
  [`plot_qsm()`](https://aidanmorales.github.io/rTwig/reference/plot_qsm.md)
  to be explicit instead of NULL.

- Replaced `group_by %>% summarise` with `summarise(.by)` for slightly
  better memory efficiency and speed (issue
  [\#25](https://github.com/aidanmorales/rTwig/issues/25)).

## rTwig 1.4.0

CRAN release: 2025-03-03

### New Features

- [`import_leaves()`](https://aidanmorales.github.io/rTwig/reference/import_leaves.md):
  a new function to import leaves from
  [QSM-FaNNI](https://github.com/InverseTampere/qsm-fanni-matlab). The
  leaves are stored as an
  [`rgl::mesh3d()`](https://dmurdoch.github.io/rgl/dev/reference/mesh3d.html)
  object.
- [`export_mesh()`](https://aidanmorales.github.io/rTwig/reference/export_mesh.md):
  The function now supports multiple 3d mesh formats including, `.ply`,
  `.obj` and `.stl`. It also supports exporting a QSM to
  [Blender](https://www.blender.org/) using the [QSM Blender
  Addons](https://github.com/InverseTampere/qsm-blender-addons) format
  `.txt`.
- New database called `twigs_index`. This is based on qualitative size
  classes supported by the literature, making it easier to select a twig
  radius for tree species without twig measurements. More info can be
  found in the `Twigs` vignette.
- [`plot_qsm()`](https://aidanmorales.github.io/rTwig/reference/plot_qsm.md):
  - Leaf meshes from
    [`import_leaves()`](https://aidanmorales.github.io/rTwig/reference/import_leaves.md)
    or `aRchi::add_leaves()` can now be plotted with the `leaves`
    parameter. Leaf color and transparency can be controlled with
    `lf_color` and `lf_alpha` respectively.
  - Now accepts random color palettes for cylinders, points, or
    triangulation meshes by setting the `*_palette = "random"`.
    Additionally, the user can set a random color for each cylinder by
    setting both `color` and `palette` to `"random"`.
  - Transparency options added for point clouds and main stem
    triangulation mesh with `pt_alpha` and `tri_alpha`.
  - Point clouds can now be colored by any variable or palette using
    `pt_color` and `pt_palette`. The user can set a random color for
    each point by setting both `pt_color` and `pt_palette` to `random`.
  - Any
    [`rgl::mesh3d()`](https://dmurdoch.github.io/rgl/dev/reference/mesh3d.html)
    objects can now be passed to
    [`plot_qsm()`](https://aidanmorales.github.io/rTwig/reference/plot_qsm.md)
    with the `mesh` parameter.
- [`tree_metrics()`](https://aidanmorales.github.io/rTwig/reference/tree_metrics.md):
  - `modified` is a standard output in the `tree` data frame. This is an
    average of the binary index of the cylinders modified by Real Twig.
    The `rTwig` version and run date are now also saved as `version` and
    `run_date`.

  - `volume_change_pct` and `area_change_pct` are standard outputs of
    the `tree` data frame. These represent the percent change in total
    tree volume and surface area using `radius` and `raw_radius`.

  - `vessel_volume`, `pipe_area`, `pipe_radius`, and `twig_distance_m`
    are standard outputs taken from the base of the QSM.

  - Added first order branches to the branch distribution metrics to
    better match the output of TreeQSM. These columns include `_1` in
    their column names.

  - `path_fraction` is now a standard output.

### Improvements

- [`export_mesh()`](https://aidanmorales.github.io/rTwig/reference/export_mesh.md):
  The function was entirely reworked using `Rcpp` for fast and efficient
  mesh export. All functionality that was dependent on `rgl` has been
  implemented in custom C++ functions.

- Function names now use British English to be consistent with R
  standards. American spelling will still work interchangeably
  (e.g. [`standardize_qsm()`](https://aidanmorales.github.io/rTwig/reference/standardise_qsm.md)
  vs
  [`standardise_qsm()`](https://aidanmorales.github.io/rTwig/reference/standardise_qsm.md).

- Some functions have been renamed to follow tidy guidelines and have
  consistency within rTwig and other R packages
  (e.g. [`qsm_summary()`](https://aidanmorales.github.io/rTwig/reference/qsm_summary.md)
  -\>
  [`summarise_qsm()`](https://aidanmorales.github.io/rTwig/reference/summarise_qsm.md)).

- Fixed a bug in
  [`plot_qsm()`](https://aidanmorales.github.io/rTwig/reference/plot_qsm.md),
  where certain variable names conflicted with internal functions,
  resulting in an error (issue
  [\#12](https://github.com/aidanmorales/rTwig/issues/12)).

- Fixed multiple bugs in
  [`tree_metrics()`](https://aidanmorales.github.io/rTwig/reference/tree_metrics.md):

  - Empty azimuth and zenith factors are now filled with 0, instead of
    being missing.

  - Issue [\#15](https://github.com/aidanmorales/rTwig/issues/15)
    occurred because the cylinder verification was at too high a level
    and could not properly create the cylinder network when an error
    condition was met.

  - Fixed an issue when only a single branch or segment was provided as
    filtered data, causing the calculations to fail. The user is now
    show a warning when these edge cases occur.

  - Fixed an issue where crown base height calculations would fail if
    there was only one first order branch present.

  - Fixed an issue where disconnected data using `verify = FALSE` would
    fail crown base calculations due to missing branches (issue
    [\#18](https://github.com/aidanmorales/rTwig/issues/18)).

- [`run_rtwig()`](https://aidanmorales.github.io/rTwig/reference/run_rtwig.md):
  The user can now use `standardize` and `standardise` interchangeably.

- Updated `Twigs` vignette with new info and plots.

- Updated `Dictionary` vignette describing all standard outputs from
  [`tree_metrics()`](https://aidanmorales.github.io/rTwig/reference/tree_metrics.md).

### Breaking Changes

- [`import_qsm()`](https://aidanmorales.github.io/rTwig/reference/import_qsm.md)
  is deprecated and has been replaced with
  [`import_treeqsm()`](https://aidanmorales.github.io/rTwig/reference/import_treeqsm.md).
  The functionality is unchanged, but the name changed to be explicit
  about its use.
  [`import_qsm()`](https://aidanmorales.github.io/rTwig/reference/import_qsm.md)
  still functions, but will be removed in a future rTwig release.

- [`qsm_summary()`](https://aidanmorales.github.io/rTwig/reference/qsm_summary.md)
  is deprecated and has been replaced with
  [`summarise_qsm()`](https://aidanmorales.github.io/rTwig/reference/summarise_qsm.md).
  The functionality is unchanged, but the name changed to be consistent
  with tidy guidelines.
  [`qsm_summary()`](https://aidanmorales.github.io/rTwig/reference/qsm_summary.md)
  still functions, but will be removed in a future rTwig release.

## rTwig 1.3.0

CRAN release: 2024-11-21

### New Features

- [`tree_metrics()`](https://aidanmorales.github.io/rTwig/reference/tree_metrics.md)
  now outputs the base diameters as a standard output in the `tree` data
  frame. The new variables are `d_base_qsm_cm` and `d_base_raw_cm`,
  which are the modified and unmodified diameter of the base of the tree
  or branch.

### Improvements

- [`tree_metrics()`](https://aidanmorales.github.io/rTwig/reference/tree_metrics.md)
  is now dynamic and can accept filtered data. The QSM must be a
  connected structure (e.g. a whole tree or single branch). If the main
  stem is shorter than DBH (1.37 m), DBH will be NA, and the user will
  be notified. If DBH is NA, the base diameter will be used in
  calculations instead. If a branch is provided, the lowest order will
  be considered as the main stem, and the standard summary will be
  calculated. Previously, trying to summarize any filtered data would
  throw an error (issue
  [\#8](https://github.com/aidanmorales/rTwig/issues/8)).

- [`qsm_summary()`](https://aidanmorales.github.io/rTwig/reference/qsm_summary.md)
  is now dynamic and can accept filtered data. If the filtered QSM data
  is a connected structure (e.g. a branch), the lowest order will be
  considered as the main stem, and the standard summary will be
  calculated. If the QSM data is disconnected (e.g. several pruned
  branches), only total volume and surface area will be calculated, and
  the user will be notified. Additionally, DBH calculations are also
  dynamic. If the main stem of a tree or branch is shorter than DBH
  (1.37 m), DBH will be NA, and the user will be notified. Previously,
  trying to summarize any filtered data would throw an error (issue
  [\#7](https://github.com/aidanmorales/rTwig/issues/7)).

- Fixed a bug in
  [`plot_qsm()`](https://aidanmorales.github.io/rTwig/reference/plot_qsm.md)
  where color vectors or a vector element would throw an unintended
  error (issue [\#9](https://github.com/aidanmorales/rTwig/issues/9)).

- Fixed a bug in
  [`update_cylinders()`](https://aidanmorales.github.io/rTwig/reference/update_cylinders.md)
  where `distanceFromBase` was being joined with the wrong index values
  (issue [\#10](https://github.com/aidanmorales/rTwig/issues/10)).

- Fix an issue for CRAN, where Fedora with the clang compiler threw an
  error.

- Random colors are now consistently brighter.

- Update Validation vignette with more data.

- Add start-up message on package load.

- Polish up documentation.

## rTwig 1.2.0

CRAN release: 2024-11-08

### New Features

- [`cluster_cloud()`](https://aidanmorales.github.io/rTwig/reference/cluster_cloud.md):
  A new function to transfer QSM variables to the input point cloud or
  simulate a point cloud from the QSM. Efficient nearest neighbor
  searching is done with the C++ *nanoflann* library:
  <https://github.com/jlblancoc/nanoflann>

- [`prune_qsm()`](https://aidanmorales.github.io/rTwig/reference/prune_qsm.md):
  A new function to efficiently and easily prune QSMs using multiple
  pruning criteria and return options.

- aRchi QSMs are now supported in the package.

- Users can now pass radius or color column variables either unquoted or
  quoted to user facing functions like
  [`plot_qsm()`](https://aidanmorales.github.io/rTwig/reference/plot_qsm.md),
  [`qsm_summary()`](https://aidanmorales.github.io/rTwig/reference/qsm_summary.md),
  or
  [`export_mesh()`](https://aidanmorales.github.io/rTwig/reference/export_mesh.md).
  Piping the QSM cylinders into these functions allows the user to
  easily select unquoted columns.

- [`update_cylinders()`](https://aidanmorales.github.io/rTwig/reference/update_cylinders.md):
  Three new allometric scaling variables as standard outputs -
  *vessel_volume*, *pipe_area*, *pipe_radius*.

- [`plot_qsm()`](https://aidanmorales.github.io/rTwig/reference/plot_qsm.md):
  Point clouds and stem triangulation meshes can now be plotted
  independently of a QSM.

  - *triangulation*: The user can now plot TreeQSM triangulation meshes
    by passing in the triangulation list from
    [`import_qsm()`](https://aidanmorales.github.io/rTwig/reference/import_qsm.md).
    The user can also control the color and palette with *tri_color* and
    *tri_palette*.
  - *pan*: The right mouse button now pans the plot instead of zooming
    by default. The functionality mimics `pan3d()` from RGL, but
    rewritten with Rcpp for better responsiveness.
  - *lit*: Enable or disable plot lighting
  - *alpha*: Set cylinder transparency. Note: can degrade performance
    with large numbers of cylinders or facets.
  - *axes_color*: The axes color can now be set by the user.
  - *skeleton_lwd*: Set the line width of the skeleton plots.
  - *normalize*: Center the cylinders around 0,0,0. Defaults to FALSE.

- [`correct_radii()`](https://aidanmorales.github.io/rTwig/reference/correct_radii.md):

  - The broken branch filter can now be disabled, with *broken_branch =
    FALSE*

  - Now returns *modified*, an index of what cylinders were modified by
    Real Twig.

- [`run_rtwig()`](https://aidanmorales.github.io/rTwig/reference/run_rtwig.md):
  The broken branch filter can now be disabled, with *broken_branch =
  FALSE*

### Improvements

- [`correct_radii()`](https://aidanmorales.github.io/rTwig/reference/correct_radii.md):
  Complete refactor with Rcpp and tidyeval for better maintainability.
  - ~ 2 - 3x performance improvement!
  - The outlier and broken branch filters are rewritten in Rcpp, for
    massive performance gains.
  - Parallel processing has been removed, eliminating overhead and
    excessive memory usage.
- [`update_cylinders()`](https://aidanmorales.github.io/rTwig/reference/update_cylinders.md):
  Fix a bug in path metrics for SimpleForest QSMs (issue
  [\#4](https://github.com/aidanmorales/rTwig/issues/4)).
- [`qsm_summary()`](https://aidanmorales.github.io/rTwig/reference/qsm_summary.md):
  - Refactored with tidyeval.
  - Fix bug where triangulation was not using the correct ending
    cylinder.
- [`box_dimension()`](https://aidanmorales.github.io/rTwig/reference/box_dimension.md):
  The 3D plot now pans on right mouse button.
- [`run_rtwig()`](https://aidanmorales.github.io/rTwig/reference/run_rtwig.md):
  Fix missing Treegraph parameter (issue
  [\#5](https://github.com/aidanmorales/rTwig/issues/5)).
- [`tree_metrics()`](https://aidanmorales.github.io/rTwig/reference/tree_metrics.md):
  - Fix a bug where the start x,y,z values were incorrect.
  - Fix a bug where NA cylinder orientation values would cause the
    calculations to fail.
  - NA values are removed and the user sees a warning promoting them to
    check the QSM.
- The QSM network is now cached in the temp folder to avoid repeat
  calculations.
- Data checking and helpful error messages are now included for every
  function.
- Add the Real Twig Method paper citation in the package and readme.
- New website look.
- Update vignettes.
- Update twig data base.
- Reduced package dependencies.

### Breaking Changes

- [`qsm_summary()`](https://aidanmorales.github.io/rTwig/reference/qsm_summary.md):
  User can pass *radius* as either a quoted or unquoted column name, so
  there is no longer any default value. Failing to provide a radius will
  throw an error prompting the user for a radius column name.
- [`correct_radii()`](https://aidanmorales.github.io/rTwig/reference/correct_radii.md):
  parallel processing has been removed, so *backend* is no longer a
  parameter.
- [`run_rtwig()`](https://aidanmorales.github.io/rTwig/reference/run_rtwig.md):parallel
  processing has been removed, so *backend* is no longer a parameter.
- *file* has been changed to *filename* in
  [`run_rtwig()`](https://aidanmorales.github.io/rTwig/reference/run_rtwig.md),
  [`import_qsm()`](https://aidanmorales.github.io/rTwig/reference/import_qsm.md),
  and
  [`import_treegraph()`](https://aidanmorales.github.io/rTwig/reference/import_treegraph.md)
  to be consistent with other functions.

## rTwig 1.1.0

CRAN release: 2024-08-20

### New Features

- [`run_rtwig()`](https://aidanmorales.github.io/rTwig/reference/run_rtwig.md):
  New function to run the Real Twig method and calculate tree metrics in
  one simple function.

- [`standardize_qsm()`](https://aidanmorales.github.io/rTwig/reference/standardise_qsm.md):
  New function to standardize variable names across all supported QSM
  software. See the dictionary vignette for more details on the variable
  names.

- [`tree_metrics()`](https://aidanmorales.github.io/rTwig/reference/tree_metrics.md):
  New function to calculate all the standard outputs of TreeQSM, plus
  new variables, metrics, and simulated point clouds reconstructed
  entirely from the QSM. Computationally expensive tasks are implemented
  with Rcpp for maximum performance.

- [`import_treegraph()`](https://aidanmorales.github.io/rTwig/reference/import_treegraph.md):
  New function to import Treegraph QSM. Treegraph support is implemented
  throughout the package!

- [`update_cylinders()`](https://aidanmorales.github.io/rTwig/reference/update_cylinders.md):
  New variables: *distanceFromBase*, *distanceToTwig*, *branch_alt.* QSM
  topology is now verified and corrected if there are any topological
  errors.

- [`plot_qsm()`](https://aidanmorales.github.io/rTwig/reference/plot_qsm.md)

  - Complete refactor with Rcpp and tidyeval for massive performance
    improvements
  - New QSM skeleton plotting option
  - Supports plotting multiple QSMs in the same plot
  - Cylinder and cloud colors can now be set to *random*
  - *bg_color* can change the background color of the plot

- [`export_mesh()`](https://aidanmorales.github.io/rTwig/reference/export_mesh.md)

  - Complete refactor with Rcpp and tidyeval for massive performance
    improvements
  - Cylinder *color* parameter can now be set to *random*, or *FALSE* to
    disable exporting colors
  - *Note: color exporting will be fixed in a future rgl package update*

### Improvements

- Implement Rcpp across package

  - [`plot_qsm()`](https://aidanmorales.github.io/rTwig/reference/plot_qsm.md)
    \> 10x performance improvement!

  - [`smooth_qsm()`](https://aidanmorales.github.io/rTwig/reference/smooth_qsm.md)
    \> 10x performance improvement!

  - [`export_mesh()`](https://aidanmorales.github.io/rTwig/reference/export_mesh.md)
    \> 10x performance improvement!

  - [`box_dimension()`](https://aidanmorales.github.io/rTwig/reference/box_dimension.md)
    ~ 3x performance improvement

- `update_cylinder()`

  - ~2x performance improvement with better vectorization

  - Cleaned variable names and removed repeat calculations

  - Verify and correct QSM topology

- Reduced package dependencies

- Cleanup function imports

- Update twig data base

- Update vignettes

- Update citations

### Breaking Changes

- All parent child cylinder IDs, nodes, and branches begin numbering at
  1 to ensure compatibility with R indexing and igraph. This affects
  SimpleForest and Treegraph.

- Redefined SimpleForest branch structure to begin at a new branch order
  and end in a twig. This ensures consistency in the code and
  calculations between TreeQSM, Treegraph, and SimpleForest. The old
  branch definition is saved as a new variable called *branch_alt*, and
  is changed to begin at zero to be consistent with the new *branch_alt*
  variable from Real Twig.

- Renamed *twigRad* function parameter to *twig_radius* to be consistent
  with tidy style guidelines

- Renamed *GrowthLength* to *growthLength* to be consistent with
  SimpleForest

- Variable names in
  [`qsm_summary()`](https://aidanmorales.github.io/rTwig/reference/qsm_summary.md)
  updated to be explicit and consistent with the outputs of
  [`tree_metrics()`](https://aidanmorales.github.io/rTwig/reference/tree_metrics.md)

- [`plot_qsm()`](https://aidanmorales.github.io/rTwig/reference/plot_qsm.md)
  now takes input column parameters as quoted variables. *cyl_color* and
  *cyl_palette* have been changed to *color* and *palette* respectively.
  *cyl_sides* has been renamed to *facets*.

- [`export_mesh()`](https://aidanmorales.github.io/rTwig/reference/export_mesh.md)
  now takes input column parameters as quoted variables. *cyl_color* and
  *cyl_palette* have been changed to *color* and *palette* respectively.
  *cyl_sides* has been renamed to *facets*.

- `plot_stand()` has been deprecated as all of its functionality is now
  incorporated into
  [`plot_qsm()`](https://aidanmorales.github.io/rTwig/reference/plot_qsm.md)

- *twigs*: renamed *scientific.name* to *scientific_name* and
  *radius.mm* to *radius_mm* for consistent naming conventions across
  the package.

## rTwig 1.0.2

CRAN release: 2024-04-08

- Changes for CRAN re-submission
  - Reduced package size
  - Allow user to run all examples
  - Small text formatting changes
- Vignette and webpage typo fixes
- Improved treedata import in
  [`import_qsm()`](https://aidanmorales.github.io/rTwig/reference/import_qsm.md)

## rTwig 1.0.1

- Initial CRAN Submission

## rTwig 1.0.0

- Initial release of Real Twig!
