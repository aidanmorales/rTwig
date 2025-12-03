# Export Mesh

Exports a QSM cylinder mesh in various formats

## Usage

``` r
export_mesh(
  cylinder,
  filename,
  format = "ply",
  radius = NULL,
  color = NULL,
  palette = NULL,
  facets = 6,
  normals = FALSE,
  alpha = 1
)
```

## Arguments

- cylinder:

  QSM cylinder data frame.

- filename:

  File name and path for exporting. The file extension is automatically
  added if not present.

- format:

  Mesh file format. Defaults to `ply`. Supported formats include `ply`,
  `obj`, `stl`, `blender`, and `groimp`. `format = "blender"` exports
  the mesh in the qsm-blender-addons format. `format = "groimp"` exports
  the mesh in the GroIMP format.

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

- facets:

  The number of facets in the polygon cross section. Defaults to 6. A
  higher number of facets improves visual smoothness at the cost of
  plotting speed, performance and memory.

- normals:

  Export surface normals per vertex. Defaults to FALSE.

- alpha:

  Set the transparency of the cylinders used in the "ply" format.
  Defaults to 1. 1 is opaque and 0 is fully transparent.

## Value

A mesh file

## Examples

``` r
## Load QSM
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

# PLY
filename <- tempfile(pattern = "QSM_ply")
export_mesh(
  cylinder = cylinder,
  filename = filename,
  format = "ply",
  color = "distanceToTwig",
  palette = "viridis",
  normals = TRUE
)
#> Exporting Mesh

# OBJ
filename <- tempfile(pattern = "QSM_obj")
export_mesh(
  cylinder = cylinder,
  filename = filename,
  format = "obj",
  normals = TRUE
)
#> Exporting Mesh

# STL
filename <- tempfile(pattern = "QSM_stl")
export_mesh(
  cylinder = cylinder,
  filename = filename,
  format = "stl",
  normals = TRUE
)
#> Exporting Mesh

# QSM Blender Addons
filename <- tempfile(pattern = "QSM_blender")
export_mesh(
  cylinder = cylinder,
  filename = filename,
  format = "blender",
  normals = TRUE
)
#> Exporting Mesh

# GroIMP
filename <- tempfile(pattern = "QSM_groimp")
export_mesh(
  cylinder = cylinder,
  filename = filename,
  format = "groimp"
)
#> Exporting Mesh
```
