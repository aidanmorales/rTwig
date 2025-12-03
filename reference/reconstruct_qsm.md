# Reconstruct QSM

Reconstruct a QSM and all of its variables from the minimum amount of
required data. The required variables are the cylinder id, parent id, 3d
information and the radius. The cylinder can be defined in 3d space with
either a combination of the start and end points, or the start, axis,
and length. Branch and branch order information are optional. If not
provided, the branches and orders are defined recursively using the
growth length.

## Usage

``` r
reconstruct_qsm(
  cylinder,
  id,
  parent,
  radius,
  start_x,
  start_y,
  start_z,
  end_x = NULL,
  end_y = NULL,
  end_z = NULL,
  axis_x = NULL,
  axis_y = NULL,
  axis_z = NULL,
  length = NULL,
  branch = NULL,
  branch_order = NULL,
  metrics = FALSE
)
```

## Arguments

- cylinder:

  QSM cylinder data frame

- id:

  cylinder ids

- parent:

  cylinder parent ids

- radius:

  cylinder radius

- start_x:

  cylinder start x position

- start_y:

  cylinder start y position

- start_z:

  cylinder start z position

- end_x:

  cylinder end x position

- end_y:

  cylinder end y position

- end_z:

  cylinder end z position

- axis_x:

  cylinder end x position

- axis_y:

  cylinder end y position

- axis_z:

  cylinder end z position

- length:

  cylinder length

- branch:

  optional cylinder branch id

- branch_order:

  optional cylinder branch order

- metrics:

  Calculate tree metrics. Defaults to FALSE.

## Value

data frame

## Examples

``` r
file <- system.file("extdata/QSM.csv", package = "rTwig")
qsm <- reconstruct_qsm(
  cylinder = read.csv(file),
  id = "ID", parent = "parentID", radius = "radius",
  start_x = "startX", start_y = "startY", start_z = "startZ",
  end_x = "endX", end_y = "endY", end_z = "endZ"
)
#> Updating Cylinder Ordering
#> Building Cylinder Network
#> Calculating Growth Length
#> Calculating Reverse Branch Order
#> Calculating Total Children
#> Calculating Alternate Branch Numbers
#> Calculating Branch Segments
#> Calculating Path Metrics
plot_qsm(qsm)
#> Plotting Cylinders
#> Panning plot on rgl device: 1
```
