# Plot QSM

Efficiently plot QSMs, point clouds, leaves, and stem triangulation
meshes. Plots can be combined and extensively customized.

## Usage

``` r
plot_qsm(
  cylinder = NULL,
  radius = NULL,
  color = NULL,
  palette = NULL,
  alpha = 1,
  facets = 6,
  skeleton = FALSE,
  skeleton_lwd = 1,
  cloud = NULL,
  pt_color = "#FFFFFF",
  pt_palette = NULL,
  pt_size = 0.1,
  pt_alpha = 1,
  triangulation = NULL,
  tri_color = NULL,
  tri_palette = NULL,
  tri_alpha = 1,
  leaves = NULL,
  lf_color = "#5BA803",
  lf_alpha = 1,
  axes = TRUE,
  axes_color = "#FFFFFF",
  grid = FALSE,
  grid_color = "#D3D3D3",
  hover = FALSE,
  bg_color = "#000000",
  lit = FALSE,
  pan = TRUE,
  normalize = FALSE,
  mesh = NULL
)
```

## Arguments

- cylinder:

  QSM cylinder data frame.

- radius:

  Radius column name either quoted or unquoted. Defaults to the modified
  radii.

- color:

  Optional cylinder color parameter. `color` must be a single hex color
  string, a
  [`grDevices::colors()`](https://rdrr.io/r/grDevices/colors.html), a
  vector of hex colors, or a quoted/unquoted column name. Vectors must
  have the same length as the cylinder data frame. `color = "random"`
  will generate a random color applied to all cylinders. Defaults to
  branching order.

- palette:

  Optional cylinder color palette for numerical data. Palettes include
  [`colourvalues::color_palettes()`](https://symbolixau.github.io/colourvalues/reference/colour_palettes.html)
  or a user supplied RGB palette matrix with the length of cylinder. It
  can also be set to "random" to generate a random palette. If combined
  with `color = "random"`, each cylinder will have a random, distinct
  color.

- alpha:

  Set the transparency of the cylinders. Defaults to 1. 1 is opaque and
  0 is fully transparent.

- facets:

  The number of facets in the polygon cross section. Defaults to 6. A
  higher number of facets improves visual smoothness at the cost of
  plotting speed, performance and memory.

- skeleton:

  Plot the QSM skeleton instead of cylinders. Defaults to FALSE.

- skeleton_lwd:

  Skeleton line width. Defaults to 1.

- cloud:

  Point cloud data frame or matrix where the first three columns are the
  x, y, and z coordinates.

- pt_color:

  Optional point cloud color parameter. `pt_color` must be a single hex
  color string, a
  [`grDevices::colors()`](https://rdrr.io/r/grDevices/colors.html), a
  vector of hex colors, or a quoted/unquoted column name.
  `pt_color = "random"` will generate a random color for all points.
  Vectors must have the same length as the point cloud data frame.
  Defaults to white.

- pt_palette:

  Optional point cloud color palette for numerical data. `pt_palette`
  includes
  [`colourvalues::color_palettes()`](https://symbolixau.github.io/colourvalues/reference/colour_palettes.html)
  or a user supplied RGB palette matrix with the length of the points.
  It can also be set to "random" to generate a random palette. If
  combined with `color = "random"`, each point will have a random,
  distinct color.

- pt_size:

  Size of the points. Defaults to 0.1.

- pt_alpha:

  Set the transparency of the point cloud. Defaults to 1. 1 is opaque
  and 0 is fully transparent.

- triangulation:

  Stem triangulation mesh from TreeQSM. Defaults to NULL.

- tri_color:

  Color of the triangulation mesh. Accepts hex colors,
  [`grDevices::colors()`](https://rdrr.io/r/grDevices/colors.html), or
  "random". Defaults to rainbow z-axis.

- tri_palette:

  Optional triangulation color palette for z values. Supports the same
  inputs as palette.

- tri_alpha:

  Set the transparency of the triangulation mesh. Defaults to 1. 1 is
  opaque and 0 is fully transparent.

- leaves:

  Leaf mesh in the
  [`rgl::mesh3d()`](https://dmurdoch.github.io/rgl/dev/reference/mesh3d.html)
  format. Defaults to NULL.

- lf_color:

  Color of the leaves. Defaults to leaf green. Accepts hex colors,
  [`grDevices::colors()`](https://rdrr.io/r/grDevices/colors.html),"random",
  or "random_vertex". `lf_color = "random"` assigns a random color to
  all leaves, while `lf_color = "random_vertex"` which assigns a random
  color to each vertex.

- lf_alpha:

  Set the transparency of the leaves. Defaults to 1. 1 is opaque and 0
  is fully transparent.

- axes:

  Show plot axes. Defaults to TRUE.

- axes_color:

  Set the axes color. Defaults to white.

- grid:

  Show plot grid lines. Defaults to FALSE.

- grid_color:

  Set grid lines color. Defaults to grey.

- hover:

  Show cylinder and branch id on mouse hover. Defaults to FALSE.

- bg_color:

  Set the background color of the plot. Accepts hex colors or
  [`grDevices::colors()`](https://rdrr.io/r/grDevices/colors.html).
  Defaults to black.

- lit:

  Enable light source in plot. Defaults to FALSE.

- pan:

  Use right mouse button to pan the plot. Defaults to TRUE. Panning is
  disabled when `hover = TRUE`.

- normalize:

  Normalize the cylinders to 0,0,0. Defaults to FALSE.

- mesh:

  Plot any
  [`rgl::mesh3d()`](https://dmurdoch.github.io/rgl/dev/reference/mesh3d.html)
  object.

## Value

An
[`rgl::open3d()`](https://dmurdoch.github.io/rgl/dev/reference/open3d.html)
plot

## Examples

``` r
## TreeQSM
file <- system.file("extdata/QSM.mat", package = "rTwig")
qsm <- import_treeqsm(file)
#> Importing TreeQSM
cylinder <- qsm$cylinder
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
plot_qsm(cylinder)
#> Plotting Cylinders
#> Panning plot on rgl device: 1

triangulation <- qsm$triangulation
plot_qsm(triangulation = triangulation)
#> Plotting Triangulation
#> Panning plot on rgl device: 1
```
