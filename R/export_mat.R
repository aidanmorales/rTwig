#' Export MAT
#'
#' @description Exports the cylinder data to the format used by TreeQSM
#'
#' @param cylinder QSM cylinder data frame
#' @param filename Desired name of file
#' @param metrics Calculate treedata and branch structs? Defaults to TRUE.
#' @param rundata Optional rundata list created by `import_treeqsm()`
#' @param pmdistance Optional pmdistance list created by `import_treeqsm()`
#' @param triangulation Optional triangulation list created by `import_treeqsm()`
#'
#' @return Returns a .mat file
#' @export
#'
#' @examples
#'
#' ## TreeQSM Processing Chain
#' file <- system.file("extdata/QSM.mat", package = "rTwig")
#' qsm <- import_treeqsm(file)
#' cylinder <- qsm$cylinder
#' cylinder <- update_cylinders(cylinder)
#'
#' filename <- tempfile(pattern = "TreeQSM_QSM", fileext = ".mat")
#' export_mat(cylinder, filename)
#'
#' ## SimpleForest Processing Chain
#' file <- system.file("extdata/QSM.csv", package = "rTwig")
#' cylinder <- read.csv(file)
#' cylinder <- update_cylinders(cylinder)
#'
#' filename <- tempfile(pattern = "SimpleForest_QSM", fileext = ".mat")
#' export_mat(cylinder, filename)
#'
export_mat <- function(
    cylinder,
    filename,
    metrics = TRUE,
    rundata = NULL,
    pmdistance = NULL,
    triangulation = NULL) {
  # Check inputs ---------------------------------------------------------------
  if (is_missing(cylinder)) {
    message <- "argument `cylinder` is missing, with no default."
    abort(message, class = "missing_argument")
  }

  if (!is.data.frame(cylinder)) {
    message <- paste(
      paste0("`cylinder` must be a data frame, not ", class(cylinder), "."),
      "i Did you accidentally pass the QSM list instead of the cylinder data frame?",
      sep = "\n"
    )
    abort(message, class = "data_format_error")
  }

  if (is_missing(filename)) {
    message <- "argument `filename` is missing, with no default."
    abort(message, class = "missing_argument")
  }

  if (!is_string(filename)) {
    message <- paste0(
      "`filename` must be a string, not ", class(filename), "."
    )
    abort(message, class = "invalid_argument")
  }

  # Ensure filename ends with correct extension
  if (substr(filename, nchar(filename) - 3, nchar(filename)) != ".mat") {
    filename <- paste0(filename, ".mat")
  }

  if (!is_logical(metrics)) {
    message <- paste0(
      "`metrics` must be logical, not ", class(metrics), "."
    )
    abort(message, class = "invalid_argument")
  }

  if (!is.null(rundata)) {
    if (!is_list(rundata)) {
      message <- paste(
        paste0("`rundata` must be a list, not ", class(rundata), "."),
        "i `rundata` must be created by `import_treeqsm()`.",
        sep = "\n"
      )
      abort(message, class = "data_format_error")
    }
    if (is_list(rundata) & length(rundata) == 0) {
      abort("`rundata` is an empty list!")
    }
  }

  if (!is.null(pmdistance)) {
    if (!is_list(pmdistance)) {
      message <- paste(
        paste0("`pmdistance` must be a list, not ", class(pmdistance), "."),
        "i `pmdistance` must be created by `import_treeqsm()`.",
        sep = "\n"
      )
      abort(message, class = "data_format_error")
    }
    if (is_list(pmdistance) & length(pmdistance) == 0) {
      abort("`pmdistance` is an empty list!")
    }
  }

  if (!is.null(triangulation)) {
    if (!is_list(triangulation)) {
      message <- paste(
        paste0("`triangulation` must be a list, not ", class(triangulation), "."),
        "i `triangulation` must be created by `import_treeqsm()`.",
        sep = "\n"
      )
      abort(message, class = "data_format_error")
    }
    if (is_list(triangulation) & length(triangulation) == 0) {
      abort("`triangulation` is an empty list!")
    }
  }

  # Verify cylinders
  cylinder <- verify_cylinders(cylinder)

  inform("Exporting to .mat")

  # rTwig ----------------------------------------------------------------------
  if (all(c("id", "parent", "start_x", "branch_order") %in% colnames(cylinder))) {
    radius <- as.matrix(cylinder$radius)
    length <- as.matrix(cylinder$length)

    start <- cylinder %>%
      select(start.x = "start_x", start.y = "start_y", start.z = "start_z") %>%
      as.matrix()

    axis <- cylinder %>%
      select(axis.x = "axis_x", axis.y = "axis_y", axis.z = "axis_z") %>%
      as.matrix()

    parent <- as.matrix(cylinder$parent)
    extension <- as.matrix(cylinder$id)
    added <- numeric(0)
    UnmodRadius <- as.matrix(cylinder$raw_radius)
    branch <- as.matrix(cylinder$branch)
    SurfCov <- numeric(0)
    mad <- numeric(0)
    BranchOrder <- as.matrix(cylinder$branch_order)
    PositionInBranch <- as.matrix(cylinder$branch_position)

    cylinder_struct <- list(
      radius = radius,
      length = length,
      start = start,
      axis = axis,
      parent = parent,
      extension = extension,
      added = added,
      UnmodRadius = UnmodRadius,
      branch = branch,
      SurfCov = SurfCov,
      mad = mad,
      BranchOrder = BranchOrder,
      PositionInBranch = PositionInBranch
    )

    structs <- build_treeqsm_struct(
      cylinder = cylinder,
      metrics = metrics,
      rundata = rundata,
      pmdistance = pmdistance,
      triangulation = triangulation
    )
  }
  # TreeQSM --------------------------------------------------------------------
  else if (all(c("parent", "extension", "branch", "BranchOrder") %in% colnames(cylinder))) {
    radius <- as.matrix(cylinder$radius)
    length <- as.matrix(cylinder$length)

    start <- cylinder %>%
      select("start.x", "start.y", "start.z") %>%
      as.matrix()

    axis <- cylinder %>%
      select("axis.x", "axis.y", "axis.z") %>%
      as.matrix()

    parent <- as.matrix(cylinder$parent)
    extension <- as.matrix(cylinder$extension)
    added <- as.matrix(cylinder$added)
    UnmodRadius <- as.matrix(cylinder$UnmodRadius)
    branch <- as.matrix(cylinder$branch)
    BranchOrder <- as.matrix(cylinder$BranchOrder)
    PositionInBranch <- as.matrix(cylinder$PositionInBranch)

    # Checks for columns only in TreeQSM v2.4.0 and up
    if (all(c("SurfCov", "mad") %in% colnames(cylinder))) {
      SurfCov <- as.matrix(cylinder$SurfCov)
      mad <- as.matrix(cylinder$mad)

      cylinder_struct <- list(
        radius = radius,
        length = length,
        start = start,
        axis = axis,
        parent = parent,
        extension = extension,
        added = added,
        UnmodRadius = UnmodRadius,
        branch = branch,
        SurfCov = SurfCov,
        mad = mad,
        BranchOrder = BranchOrder,
        PositionInBranch = PositionInBranch
      )
    } else {
      SurfCov <- numeric(0)
      mad <- numeric(0)

      cylinder_struct <- list(
        radius = radius,
        length = length,
        start = start,
        axis = axis,
        parent = parent,
        extension = extension,
        added = added,
        UnmodRadius = UnmodRadius,
        branch = branch,
        SurfCov = SurfCov,
        mad = mad,
        BranchOrder = BranchOrder,
        PositionInBranch = PositionInBranch
      )
    }

    structs <- build_treeqsm_struct(
      cylinder = cylinder,
      metrics = metrics,
      rundata = rundata,
      pmdistance = pmdistance,
      triangulation = triangulation
    )
  }
  # SimpleForest ---------------------------------------------------------------
  else if (all(c("ID", "parentID", "branchID", "branchOrder") %in% colnames(cylinder))) {
    radius <- as.matrix(cylinder$radius)
    length <- as.matrix(cylinder$length)

    start <- cylinder %>%
      select(start.x = "startX", start.y = "startY", start.z = "startZ") %>%
      as.matrix()

    axis <- cylinder %>%
      select(axis.x = "axisX", axis.y = "axisY", axis.z = "axisZ") %>%
      as.matrix()

    parent <- as.matrix(cylinder$parentID)
    extension <- as.matrix(cylinder$ID)
    added <- numeric(0)
    UnmodRadius <- as.matrix(cylinder$UnmodRadius)
    branch <- as.matrix(cylinder$branchID)
    SurfCov <- numeric(0)
    mad <- as.matrix(cylinder$averagePointDistance)
    BranchOrder <- as.matrix(cylinder$branchOrder)
    PositionInBranch <- as.matrix(cylinder$positionInBranch)

    cylinder_struct <- list(
      radius = radius,
      length = length,
      start = start,
      axis = axis,
      parent = parent,
      extension = extension,
      added = added,
      UnmodRadius = UnmodRadius,
      branch = branch,
      SurfCov = SurfCov,
      mad = mad,
      BranchOrder = BranchOrder,
      PositionInBranch = PositionInBranch
    )

    structs <- build_treeqsm_struct(
      cylinder = cylinder,
      metrics = metrics,
      rundata = rundata,
      pmdistance = pmdistance,
      triangulation = triangulation
    )
  }
  # Treegraph ------------------------------------------------------------------
  else if (all(c("p1", "p2", "ninternode") %in% colnames(cylinder))) {
    radius <- as.matrix(cylinder$radius)
    length <- as.matrix(cylinder$length)

    start <- cylinder %>%
      select(start.x = "sx", start.y = "sy", start.z = "sz") %>%
      as.matrix()

    axis <- cylinder %>%
      select(axis.x = "ax", axis.y = "ay", axis.z = "az") %>%
      as.matrix()

    parent <- as.matrix(cylinder$p2)
    extension <- as.matrix(cylinder$p1)
    added <- numeric(0)
    UnmodRadius <- as.matrix(cylinder$UnmodRadius)
    branch <- as.matrix(cylinder$nbranch)
    SurfCov <- numeric(0)
    mad <- numeric(0)
    BranchOrder <- as.matrix(cylinder$branch_order)
    PositionInBranch <- as.matrix(cylinder$positionInBranch)

    cylinder_struct <- list(
      radius = radius,
      length = length,
      start = start,
      axis = axis,
      parent = parent,
      extension = extension,
      added = added,
      UnmodRadius = UnmodRadius,
      branch = branch,
      SurfCov = SurfCov,
      mad = mad,
      BranchOrder = BranchOrder,
      PositionInBranch = PositionInBranch
    )

    structs <- build_treeqsm_struct(
      cylinder = cylinder,
      metrics = metrics,
      rundata = rundata,
      pmdistance = pmdistance,
      triangulation = triangulation
    )
  }
  # aRchi ----------------------------------------------------------------------
  else if (all(c("cyl_ID", "parent_ID", "branching_order") %in% colnames(cylinder))) {
    radius <- as.matrix(cylinder$radius_cyl)
    length <- as.matrix(cylinder$length)

    start <- cylinder %>%
      select(start.x = "startX", start.y = "startY", start.z = "startZ") %>%
      as.matrix()

    axis <- cylinder %>%
      select(axis.x = "axisX", axis.y = "axisY", axis.z = "axisZ") %>%
      as.matrix()

    parent <- as.matrix(cylinder$parent_ID)
    extension <- as.matrix(cylinder$cyl_ID)
    added <- numeric(0)
    UnmodRadius <- as.matrix(cylinder$UnmodRadius)
    branch <- as.matrix(cylinder$branch_ID)
    SurfCov <- numeric(0)
    mad <- numeric(0)
    BranchOrder <- as.matrix(cylinder$branching_order)
    PositionInBranch <- as.matrix(cylinder$positionInBranch)

    cylinder_struct <- list(
      radius = radius,
      length = length,
      start = start,
      axis = axis,
      parent = parent,
      extension = extension,
      added = added,
      UnmodRadius = UnmodRadius,
      branch = branch,
      SurfCov = SurfCov,
      mad = mad,
      BranchOrder = BranchOrder,
      PositionInBranch = PositionInBranch
    )

    structs <- build_treeqsm_struct(
      cylinder = cylinder,
      metrics = metrics,
      rundata = rundata,
      pmdistance = pmdistance,
      triangulation = triangulation
    )
  } else {
    message <- paste(
      "Unsupported QSM format provided.",
      "i Only TreeQSM, SimpleForest, Treegraph, aRchi, AdQSM, or AdTree QSMs are supported.",
      sep = "\n"
    )
    abort(message, class = "data_format_error")
  }

  # Export mat
  write_mat(
    filename = filename,
    cylinder = cylinder_struct,
    branch = structs$branch,
    treedata = structs$treedata,
    rundata = structs$rundata,
    pmdistance = structs$pmdistance,
    triangulation = structs$triangulation
  )
}

#' Build TreeQSM Struct
#'
#' @param cylinder cylinder data frame
#' @param metrics boolean, defaults to TRUE
#' @param rundata rundata list from `import_treeqsm()`
#' @param pmdistance pmdistance list from `import_treeqsm()`
#' @param triangulation triangulation list from `import_treeqsm()`
#'
#' @returns list
#' @noRd
#'
build_treeqsm_struct <- function(
    cylinder,
    metrics = TRUE,
    rundata = NULL,
    pmdistance = NULL,
    triangulation = NULL) {
  # branch and treedata struct -------------------------------------------------
  if (metrics == TRUE) {
    # Calculate tree metrics
    metrics <- tree_metrics(cylinder = cylinder, triangulation = triangulation)

    # Branch struct
    branch_struct <- list(
      order = metrics$branch$branch_order,
      parent = metrics$branch$parent_branch,
      diameter = metrics$branch$diameter_base_cm / 100,
      volume = metrics$branch$volume_m3 * 1000,
      area = metrics$branch$area_m2,
      length = metrics$branch$length_m,
      angle = metrics$branch$angle_deg,
      height = metrics$branch$height_m,
      azimuth = metrics$branch$azimuth_deg,
      zenith = metrics$branch$zenith
    )

    # Treedata struct
    treedata_struct <- list(
      # Tree metrics
      TotalVolume = metrics$tree$tree_volume_m3 * 1000,
      TrunkVolume = metrics$tree$stem_volume_m3 * 1000,
      BranchVolume = metrics$tree$branch_volume_m3 * 1000,
      TreeHeight = metrics$tree$tree_height_m,
      TrunkLength = metrics$tree$stem_length_m,
      BranchLength = metrics$tree$branch_length_m,
      TotalLength = metrics$tree$tree_length_m,
      NumberBranches = metrics$tree$branches,
      MaxBranchOrder = metrics$tree$max_branch_order,
      TrunkArea = metrics$tree$stem_area_m2,
      BranchArea = metrics$tree$branch_area_m2,
      TotalArea = metrics$tree$tree_area_m2,
      DBHqsm = metrics$tree$dbh_qsm_cm / 100,
      DBHcyl = metrics$tree$dbh_raw_cm / 100,
      CrownDiamAve = metrics$tree$crown_diameter_mean_m,
      CrownDiamMax = metrics$tree$crown_diameter_max_m,
      CrownAreaConv = metrics$tree$crown_projected_area_m2,
      CrownAreaAlpha = metrics$tree$crown_projected_alpha_area_m2,
      CrownBaseHeight = metrics$tree$crown_base_height_m,
      CrownLength = metrics$tree$crown_length_m,
      CrownRatio = metrics$tree$crown_ratio,
      CrownVolumeConv = metrics$tree$crown_volume_m3,
      CrownVolumeAlpha = metrics$tree$crown_alpha_volume_m3,

      # Triangulation metrics placeholder
      DBHtri = NULL,
      TriaTrunkVolume = NULL,
      MixTrunkVolume = NULL,
      MixTotalVolume = NULL,
      TriaTrunkArea = NULL,
      MixTrunkArea = NULL,
      MixTotalArea = NULL,
      TriaTrunkLength = NULL,

      # Stem location
      location = cbind(
        metrics$tree$start_x,
        metrics$tree$start_y,
        metrics$tree$start_z
      ),

      # Stem taper
      StemTaper = rbind(
        metrics$stem_taper$height_m,
        metrics$stem_taper$diameter_cm / 100
      ),

      # Vertical profile
      VerticalProfile = metrics$vertical_profile$avg_spread_m,

      # Spreads
      Spreads = metrics$spreads %>%
        pivot_wider(
          names_from = .data$azimuth_deg,
          values_from = .data$spread_m,
          values_fill = 0
        ) %>%
        select(-"height_class") %>%
        as.matrix(),

      # Tree distributions by diameter), height), zenith), azimuth), and order
      VolCylDia = t(as.matrix(metrics$tree_diameter_dist$volume_m3 * 1000)),
      AreCylDia = t(as.matrix(metrics$tree_diameter_dist$area_m2)),
      LenCylDia = t(as.matrix(metrics$tree_diameter_dist$length_m)),
      VolCylHei = t(as.matrix(metrics$tree_height_dist$volume_m3 * 1000)),
      AreCylHei = t(as.matrix(metrics$tree_height_dist$area_m2)),
      LenCylHei = t(as.matrix(metrics$tree_height_dist$length_m)),
      VolCylZen = t(as.matrix(metrics$tree_zenith_dist$volume_m3 * 1000)),
      AreCylZen = t(as.matrix(metrics$tree_zenith_dist$area_m2)),
      LenCylZen = t(as.matrix(metrics$tree_zenith_dist$length_m)),
      VolCylAzi = t(as.matrix(metrics$tree_azimuth_dist$volume_m3 * 1000)),
      AreCylAzi = t(as.matrix(metrics$tree_azimuth_dist$area_m2)),
      LenCylAzi = t(as.matrix(metrics$tree_azimuth_dist$length_m)),
      VolBranchOrd = t(as.matrix(metrics$branch_order_dist$volume_m3 * 1000)),
      AreBranchOrd = t(as.matrix(metrics$branch_order_dist$area_m2)),
      LenBranchOrd = t(as.matrix(metrics$branch_order_dist$length_m)),
      NumBranchOrd = t(as.matrix(metrics$branch_order_dist$branches)),

      # Branch distributions by diameter
      VolBranchDia = t(as.matrix(metrics$branch_diameter_dist$volume_m3 * 1000)),
      VolBranchDia1 = t(as.matrix(metrics$branch_diameter_dist$volume_1_m3 * 1000)),
      AreBranchDia = t(as.matrix(metrics$branch_diameter_dist$area_m2)),
      AreBranchDia1 = t(as.matrix(metrics$branch_diameter_dist$area_1_m2)),
      LenBranchDia = t(as.matrix(metrics$branch_diameter_dist$length_m)),
      LenBranchDia1 = t(as.matrix(metrics$branch_diameter_dist$length_1_m)),
      NumBranchDia = t(as.matrix(metrics$branch_diameter_dist$branches)),
      NumBranchDia1 = t(as.matrix(metrics$branch_diameter_dist$branches_1)),

      # Branch distributions by height
      VolBranchHei = t(as.matrix(metrics$branch_height_dist$volume_m3 * 1000)),
      VolBranchHei1 = t(as.matrix(metrics$branch_height_dist$volume_1_m3 * 1000)),
      AreBranchHei = t(as.matrix(metrics$branch_height_dist$area_m2)),
      AreBranchHei1 = t(as.matrix(metrics$branch_height_dist$area_1_m2)),
      LenBranchHei = t(as.matrix(metrics$branch_height_dist$length_m)),
      LenBranchHei1 = t(as.matrix(metrics$branch_height_dist$length_1_m)),
      NumBranchHei = t(as.matrix(metrics$branch_height_dist$branches)),
      NumBranchHei1 = t(as.matrix(metrics$branch_height_dist$branches_1)),

      # Branch distributions by angle
      VolBranchAng = t(as.matrix(metrics$branch_angle_dist$volume_m3 * 1000)),
      VolBranchAng1 = t(as.matrix(metrics$branch_angle_dist$volume_1_m3 * 1000)),
      AreBranchAng = t(as.matrix(metrics$branch_angle_dist$area_m2)),
      AreBranchAng1 = t(as.matrix(metrics$branch_angle_dist$area_1_m2)),
      LenBranchAng = t(as.matrix(metrics$branch_angle_dist$length_m)),
      LenBranchAng1 = t(as.matrix(metrics$branch_angle_dist$length_1_m)),
      NumBranchAng = t(as.matrix(metrics$branch_angle_dist$branches)),
      NumBranchAng1 = t(as.matrix(metrics$branch_angle_dist$branches_1)),

      # Branch distributions by azimuth
      VolBranchAzi = t(as.matrix(metrics$branch_azimuth_dist$volume_m3 * 1000)),
      VolBranchAzi1 = t(as.matrix(metrics$branch_azimuth_dist$volume_1_m3 * 1000)),
      AreBranchAzi = t(as.matrix(metrics$branch_azimuth_dist$area_m2)),
      AreBranchAzi1 = t(as.matrix(metrics$branch_azimuth_dist$area_1_m2)),
      LenBranchAzi = t(as.matrix(metrics$branch_azimuth_dist$length_m)),
      LenBranchAzi1 = t(as.matrix(metrics$branch_azimuth_dist$length_1_m)),
      NumBranchAzi = t(as.matrix(metrics$branch_azimuth_dist$branches)),
      NumBranchAzi1 = t(as.matrix(metrics$branch_azimuth_dist$branches_1)),

      # Branch distributions by zenith
      VolBranchZen = t(as.matrix(metrics$branch_zenith_dist$volume_m3 * 1000)),
      VolBranchZen1 = t(as.matrix(metrics$branch_zenith_dist$volume_1_m3 * 1000)),
      AreBranchZen = t(as.matrix(metrics$branch_zenith_dist$area_m2)),
      AreBranchZen1 = t(as.matrix(metrics$branch_zenith_dist$area_1_m2)),
      LenBranchZen = t(as.matrix(metrics$branch_zenith_dist$length_m)),
      LenBranchZen1 = t(as.matrix(metrics$branch_zenith_dist$length_1_m)),
      NumBranchZen = t(as.matrix(metrics$branch_zenith_dist$branches)),
      NumBranchZen1 = t(as.matrix(metrics$branch_zenith_dist$branches_1))
    )

    # Update triangulation metrics
    if (!is.null(triangulation)) {
      tri <- summarise_triangulation(cylinder, triangulation)
      treedata_struct$DBHtri <- tri$dbh_tri_cm / 100
      treedata_struct$TriaTrunkVolume <- tri$tri_volume_m3 * 1000
      treedata_struct$MixTrunkVolume <- tri$stem_mix_volume_m3 * 1000
      treedata_struct$MixTotalVolume <- tri$tree_mix_volume_m3 * 1000
      treedata_struct$TriaTrunkArea <- tri$tri_area_m2
      treedata_struct$MixTrunkArea <- tri$stem_mix_area_m2
      treedata_struct$MixTotalArea <- tri$tree_mix_area_m2
      treedata_struct$TriaTrunkLength <- tri$tri_length_m
    } else {
      treedata_struct[c(
        "DBHtri",
        "TriaTrunkVolume",
        "MixTrunkVolume",
        "MixTotalVolume",
        "TriaTrunkArea",
        "MixTrunkArea",
        "MixTotalArea",
        "TriaTrunkLength"
      )] <- NULL
    }
  } else {
    branch_struct <- list()
    treedata_struct <- list()
  }

  # rundata struct -------------------------------------------------------------
  if (!is.null(rundata)) {
    inputs <- list(
      PatchDiam1 = rundata$PatchDiam1,
      PatchDiam2Min = rundata$PatchDiam2Min,
      PatchDiam2Max = rundata$PatchDiam2Max,
      BallRad1 = rundata$BallRad1,
      BallRad2 = rundata$BallRad2,
      nmin1 = rundata$nmin1,
      nmin2 = rundata$nmin2,
      OnlyTree = rundata$OnlyTree,
      Tria = rundata$Tria,
      Dist = rundata$Dist,
      MinCylRad = rundata$MinCylRad,
      ParentCor = rundata$ParentCor,
      TaperCor = rundata$TaperCor,
      GrowthVolCor = rundata$GrowthVolCor,
      GrowthVolFac = rundata$GrowthVolFac,
      filter = list(
        k = rundata$filter.k,
        radius = rundata$filter.radius,
        nsigma = rundata$filter.nsigma,
        PatchDiam1 = rundata$filter.PatchDiam1,
        BallRad1 = rundata$filter.BallRad1,
        ncomp = rundata$filter.ncomp,
        EdgeLength = rundata$filter.EdgeLength,
        plot = rundata$filter.plot
      ),
      name = rundata$name,
      tree = rundata$tree,
      model = rundata$model,
      savemat = rundata$savemat,
      savetxt = rundata$savetxt,
      plot = rundata$plot,
      disp = rundata$disp
    )

    time <- as.vector(as.matrix(rundata[1, 31:42]))

    date <- rbind(
      date_to_matrix(rundata$start.date),
      date_to_matrix(rundata$end.date)
    )

    version <- rundata$version

    rundata_struct <- list(
      inputs = inputs,
      time = time,
      date = date,
      version = version
    )
  } else {
    rundata_struct <- list()
  }

  # pmdistance struct ----------------------------------------------------------
  if (!is.null(pmdistance)) {
    pmdistance_struct <- list(
      CylDist = pmdistance$CylDist[[1]],
      median = pmdistance$median[[1]],
      mean = pmdistance$mean[[1]],
      max = pmdistance$max[[1]],
      std = pmdistance$std[[1]],
      TrunkMedian = pmdistance$TrunkMedian[[1]],
      TrunkMean = pmdistance$TrunkMean[[1]],
      TrunkMax = pmdistance$TrunkMax[[1]],
      TrunkStd = pmdistance$TrunkStd[[1]],
      BranchMedian = pmdistance$BranchMedian[[1]],
      BranchMean = pmdistance$BranchMean[[1]],
      BranchMax = pmdistance$BranchMax[[1]],
      BranchStd = pmdistance$BranchStd[[1]],
      Branch1Median = pmdistance$Branch1Median[[1]],
      Branch1Mean = pmdistance$Branch1Mean[[1]],
      Branch1Max = pmdistance$Branch1Max[[1]],
      Branch1Std = pmdistance$Branch1Std[[1]],
      Branch2Median = pmdistance$Branch2Median[[1]],
      Branch2Mean = pmdistance$Branch2Mean[[1]],
      Branch2Max = pmdistance$Branch2Max[[1]],
      Branch2Std = pmdistance$Branch2Std[[1]]
    )
  } else {
    pmdistance_struct <- list()
  }

  # triangulation struct -------------------------------------------------------
  if (!is.null(triangulation)) {
    triangulation_struct <- list(
      vert = as.matrix(triangulation$vert),
      facet = as.matrix(triangulation$facet),
      fvd = as.matrix(triangulation$fvd),
      volume = triangulation$volume$volume,
      SideArea = triangulation$SideArea$SideArea,
      BottomArea = triangulation$BottomArea$BottomArea,
      bottom = triangulation$bottom$bottom,
      top = triangulation$top$top,
      triah = triangulation$triah$triah,
      triaw = triangulation$triaw$triaw,
      cylind = triangulation$cylind$cylind
    )
  } else {
    triangulation_struct <- list()
  }

  return(
    list(
      branch = branch_struct,
      treedata = treedata_struct,
      rundata = rundata_struct,
      pmdistance = pmdistance_struct,
      triangulation = triangulation_struct
    )
  )
}

#' Date to Matrix
#'
#' @param date date string
#'
#' @returns matrix
#' @noRd
#'
date_to_matrix <- function(date) {
  date <- as.POSIXct(date)
  parts <- c(
    as.numeric(format(date, "%Y")), # year
    as.numeric(format(date, "%m")), # month
    as.numeric(format(date, "%d")), # day
    as.numeric(format(date, "%H")), # hour
    as.numeric(format(date, "%M")), # minute
    as.numeric(format(date, "%OS6")) # seconds
  )

  matrix(parts, nrow = 1)
}
