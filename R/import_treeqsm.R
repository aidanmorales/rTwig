#' Import TreeQSM
#'
#' @description Imports a QSM created by TreeQSM
#'
#' @param filename a TreeQSM .mat MATLAB file
#'
#' @return Returns a list
#' @export
#'
#' @references
#' \insertRef{TreeQSM}{rTwig}
#'
#' @examples
#'
#' ## Read a TreeQSM MATLAB file in the 2.3.x - 2.4.x format
#' file <- system.file("extdata/QSM.mat", package = "rTwig")
#' qsm <- import_treeqsm(file)
#' summary(qsm)
#'
#' ## Read a TreeQSM MATLAB file in the 2.0 format
#' file <- system.file("extdata/QSM_2.mat", package = "rTwig")
#' qsm <- import_treeqsm(file)
#' names(qsm)
#'
import_treeqsm <- function(filename) {
  # Check inputs ---------------------------------------------------------------
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

  if (!file.exists(filename)) {
    message <- paste(
      "The file in `filename` does not exist.",
      "i Did you enter the correct path to your QSM?",
      sep = "\n"
    )
    abort(message, class = "file_error")
  }

  # Get file extension
  extension <- sub(".*\\.", "", basename(filename))

  # Ensure filename ends with correct extension
  if (extension != "mat") {
    abort("`filename` must end in `.mat`.", class = "data_format_error")
  }

  inform("Importing TreeQSM")

  # Dynamically check TreeQSM version
  required <- c(
    "Added", "Axe", "BAng", "BLen", "BOrd", "BPar", "BSeg", "BVol",
    "Base", "BoC", "CExt", "CPar", "FCB", "Len", "Rad", "SoC", "Sta", "TreeData"
  )

  tmp <- try(read_mat_20(filename), silent = TRUE)

  if (!inherits(tmp, "try-error") && setequal(names(tmp), required)) {
    qsm <- tmp
    version <- 2.0
  } else {
    qsm <- tryCatch(
      read_mat_23(filename),
      error = function(e) {
        abort(
          paste(
            "Failed to read TreeQSM!",
            "i File is not a valid TreeQSM 2.0 or 2.x.x structure.",
            sep = "\n"
          ),
          class = "data_format_error"
        )
      }
    )

    version <- 2.3
  }

  ##############################################################################
  ###### TreeQSM 2.3.x - 2.4.x Structure #######################################
  ##############################################################################

  if (version == 2.3) {
    if (!is.null(qsm[[1]]) && is.list(qsm[[1]]) && !is.null(qsm[[1]]$cylinder)) {
      inform("Multiple QSMs detected!")
      qsms <- qsm
    } else {
      qsms <- list(qsm)
    }

    out <- vector("list", length(qsms))

    for (i in seq_along(qsms)) {
      qsm <- qsms[[i]]

      # Cylinder Data ----------------------------------------------------------
      cylinder <- list()

      # Names of each element of the cylinder data
      cylinder_names <- names(qsm$cylinder)

      # Assigns names to each element in the cylinder qsm
      for (j in 1:length(qsm$cylinder)) {
        temp <- as.data.frame(lapply(qsm$cylinder[j], function(x) {
          if (is.null(x) || length(x) == 0 || prod(dim(x)) == 0) NA else x
        }))
        if (min(dim(temp)) == 1) {
          colnames(temp) <- cylinder_names[j]
        } else if (min(dim(temp)) == 3) {
          colnames(temp) <- c(
            paste0(cylinder_names[j], ".x"),
            paste0(cylinder_names[j], ".y"),
            paste0(cylinder_names[j], ".z")
          )
        }
        cylinder <- c(cylinder, list(temp))
      }

      # Combines all of the cylinder data into a single data frame
      cylinder <- do.call(cbind, cylinder)
      cylinder$added <- as.numeric(cylinder$added)

      # Branch Data ------------------------------------------------------------
      branch <- list()

      # Names of each element of the branch data
      branch_names <- names(qsm$branch)

      if (!is.null(branch_names)) {
        # Assigns names to each element in the branch qsm
        for (j in 1:length(qsm$branch)) {
          temp <- as.data.frame(qsm$branch[j])
          colnames(temp) <- branch_names[j]
          branch <- c(branch, list(temp))
        }

        # Combines all of the branch data into a single data frame
        branch <- do.call(cbind, branch)
      }
      # Tree Data --------------------------------------------------------------
      treedata <- list()

      # Names of each element of the treedata data
      treedata_names <- names(qsm$treedata)

      if (!is.null(treedata_names)) {
        # Loops through and converts the MATLAB arrays to data frames
        # The results are stored in the treedata qsm
        for (j in 1:length(qsm$treedata)) {
          # Gets the name of the qsm element
          name <- treedata_names[j]

          # Extracts the tree variables
          if (!name %in% c("location", "StemTaper", "spreads")) {
            temp <- as.data.frame(t(qsm$treedata[[j]]))
            colnames(temp) <- name
          } else if (name == "location") {
            temp <- as.vector(qsm$treedata[[j]])
          } else if (name == "StemTaper") {
            temp <- as.data.frame(t(qsm$treedata[[j]]))
            rownames(temp) <- 1:nrow(temp)
            colnames(temp) <- c("Dist.m", "Stem.dia.m")
          } else if (name == "spreads") {
            temp <- qsm$treedata[[j]]
          }

          # Binds each iteration to the treedata qsm and names it
          treedata[[j]] <- temp
          names(treedata)[j] <- name
        }
      }

      # Run Data ---------------------------------------------------------------
      rundata <- list()

      # Names of each element of the rundata data
      rundata_names <- names(qsm$rundata)

      if (!is.null(rundata_names)) {
        for (j in 1:length(qsm$rundata)) {
          # Gets the main inputs data
          if (rundata_names[j] == "inputs") {
            input_names <- names(qsm$rundata$inputs)

            # Loops over the rundata and extracts the input variables
            for (k in 1:length(qsm$rundata$inputs)) {
              if (length(qsm$rundata$inputs[[k]]) == 1) {
                inputs <- as.data.frame(qsm$rundata$inputs[k])
                if (length(inputs) == 1) {
                  colnames(inputs) <- input_names[k]
                  rundata <- c(rundata, list(inputs))
                }
              }

              # Extracts the filter information
              if (input_names[k] == "filter") {
                filter <- list()
                filter_names <- names(qsm$rundata$inputs[[k]])

                for (l in 1:length(filter_names)) {
                  temp <- as.data.frame(qsm$rundata$inputs$filter[l])
                  colnames(temp) <- paste0("filter.", filter_names[l])
                  filter <- c(filter, list(temp))
                }

                # Combines all of the filter data into a single data frame
                filter <- do.call(cbind, filter)
                rundata <- c(rundata, list(filter))
              }
            }
          }

          # Extracts reconstruction times
          if (rundata_names[j] == "time") {
            times <- as.data.frame(qsm$rundata$time)
            times <- as.data.frame(t(times))
            rownames(times) <- NULL
            colnames(times) <- c(
              "cover.sets.1", # Initial cover set time (s)
              "tree.sets.1", # Initial tree set time (s)
              "initial.segments.1", # Initial segments time (s)
              "final.segments.1", # Initial Final segments time (s)
              "cover.sets.2", # Second cover set time (s)
              "tree.sets.2", # Second tree set time (s)
              "initial.segments.2", # Second segments time (s)
              "final.segments.2", # Second final segments time (s)
              "cylinders", # Cylinder reconstruction time (s)
              "branch.data", # Branch and data time (s)
              "distances", # Distances time (s)
              "total" # Total QSM run time (s)
            )
            rundata <- c(rundata, list(times))
          }

          # Extracts the date info
          if (rundata_names[j] == "date") {
            start_date <- as.data.frame(
              as.POSIXct(
                paste(as.integer(qsm$rundata$date[1, ]), collapse = "-"),
                format = "%Y-%m-%d-%H-%M-%S"
              )
            )

            end_date <- as.data.frame(
              as.POSIXct(
                paste(as.integer(qsm$rundata$date[2, ]), collapse = "-"),
                format = "%Y-%m-%d-%H-%M-%S"
              )
            )

            dates <- cbind(start_date, end_date)
            colnames(dates) <- c("start.date", "end.date")
            rundata <- c(rundata, list(dates))
          }

          # Extracts TreeQSM version info
          if (rundata_names[j] == "version") {
            version <- as.data.frame(qsm$rundata$version)
            colnames(version) <- "version"
            rundata <- c(rundata, list(version))
          }
        }

        # Combines all of the rundata together
        rundata <- do.call(cbind, rundata)
      }

      # Pmdistance Data --------------------------------------------------------
      pmdistance <- qsm$pmdistance
      pmdistance_names <- names(qsm$pmdistance)

      if (is.null(pmdistance_names)) {
        pmdistance <- list()
      } else {
        # Converts to data frames
        for (j in 1:length(pmdistance)) {
          name <- pmdistance_names[j]
          temp <- as.data.frame(pmdistance[[j]])
          colnames(temp) <- name
          pmdistance[[j]] <- temp
        }

        names(pmdistance) <- pmdistance_names
      }

      # Triangulation Data -----------------------------------------------------
      triangulation <- qsm$triangulation
      triangulation_names <- names(qsm$triangulation)

      if (is.null(triangulation_names)) {
        triangulation <- list()
      } else {
        if (length(triangulation$vert) != 0) {
          # Converts to data frames
          for (j in 1:length(triangulation)) {
            name <- triangulation_names[j]
            temp <- as.data.frame(triangulation[[j]])

            if (name == "vert") {
              colnames(temp) <- c("vert.1", "vert.2", "vert.3")
            } else if (name == "facet") {
              colnames(temp) <- c("facet.1", "facet.2", "facet.3")
            } else {
              (
                colnames(temp) <- name
              )
            }

            triangulation[[j]] <- temp
          }

          names(triangulation) <- triangulation_names
        }
      }

      # QSM Final Output -------------------------------------------------------
      qsm <- list(cylinder, branch, treedata, rundata, pmdistance, triangulation)
      names(qsm) <- c("cylinder", "branch", "treedata", "rundata", "pmdistance", "triangulation")
      out[[i]] <- qsm
    }

    qsm <- if (length(out) == 1) out[[1]] else out

    ############################################################################
    ###### TreeQSM 2.0 Structure ###############################################
    ############################################################################
  } else if (version == 2.0) {
    # Cylinder Data ------------------------------------------------------------
    radius <- as.vector(qsm$Rad)
    length <- as.vector(qsm$Len)
    start <- as.data.frame(qsm$Sta)
    colnames(start) <- c("start.x", "start.y", "start.z")
    axis <- as.data.frame(qsm$Axe)
    colnames(axis) <- c("axis.x", "axis.y", "axis.z")
    parent <- as.vector(qsm$CPar)
    extension <- as.vector(qsm$CExt)
    added <- as.numeric(as.vector(qsm$Added))
    UnmodRadius <- radius
    branch <- as.vector(qsm$BoC[, 1])
    BranchOrder <- as.vector(qsm$BoC[, 2])
    PositionInBranch <- as.vector(qsm$BoC[, 3])

    # Fixes ordering on extensions
    extension[extension == 0] <- NA
    extension <- extension - 1
    extension[is.na(extension)] <- 0

    # Binds QSM elements into a data frame
    cylinder <- cbind(
      radius,
      length,
      start,
      axis,
      parent,
      extension,
      added,
      UnmodRadius,
      branch,
      BranchOrder,
      PositionInBranch
    )

    # Tree Data ----------------------------------------------------------------
    metrics <- qsm$TreeData[1:15]
    names <- c(
      "TotVol", # Total tree volume (L)
      "TrunkVol", # Total trunk volume (L)
      "BranVol", # Total branch volume (L)
      "TotHei", # Total tree height (m)
      "TrunkLen", # Total trunk length (m)
      "BranLen", # Total branch length (m)
      "b", # Total number of branches
      "BO", # Maximum branch order
      "TotArea", # Total area (m2)
      "DBH", # DBH cylinder (cm)
      "DBHTri", # DBH triangulation (cm)
      "Vtcyl", # Trunk volume of over 1/3 diameter part cylinders (L)
      "Vtrunk", # Trunk volume of over 1/3 diameter part triangulation (L)
      "maxL", # Trunk length of over 1/3 diameter part cylinders (m)
      "Htri" # Trunk length of over 1/3 diameter part triangulation (m)
    )

    # Extracts Tree Metrics
    treedata <- list()

    for (i in 1:length(metrics)) {
      temp <- as.data.frame(metrics[i])
      colnames(temp) <- names[i]
      treedata <- c(treedata, list(temp))
      names(treedata)[i] <- names[i]
    }

    # Dynamically extracts branch order metrics
    branch_metrics <- qsm$TreeData[16:length(qsm$TreeData)]
    branch_order <- length(branch_metrics) / 3

    for (i in 1:length(branch_metrics)) {
      if (i <= branch_order) {
        temp <- as.data.frame(branch_metrics[i])
        name <- paste0("TotBO", i)
        colnames(temp) <- name
        treedata <- c(treedata, list(temp))
        names(treedata)[i + 15] <- name
      } else if (i > branch_order * 1 & i <= branch_order * 2) {
        temp <- as.data.frame(branch_metrics[i])
        name <- paste0("VolBO", (i - branch_order))
        colnames(temp) <- name
        treedata <- c(treedata, list(temp))
        names(treedata)[i + 15] <- name
      } else if (i > branch_order * 2 & i <= branch_order * 3) {
        temp <- as.data.frame(branch_metrics[i])
        name <- paste0("LenBO", (i - branch_order * 2))
        colnames(temp) <- name
        treedata <- c(treedata, list(temp))
        names(treedata)[i + 15] <- name
      }
    }

    # QSM Final Output ---------------------------------------------------------
    qsm <- list(cylinder, treedata)
    names(qsm) <- c("cylinder", "treedata")
  } else {
    message <- paste(
      "`version` is invalid.",
      "i Only TreeQSM v2.0 - v2.4.1 are supported in `import_treeqsm()`.",
      sep = "\n"
    )
    abort(message, class = "data_format_error")
  }

  return(qsm)
}

#' @title Import TreeQSM
#'
#' @description `import_qsm()` is deprecated and will be removed in a future
#'  version. Use `import_treeqsm()` instead.
#' @param ... function inputs
#' @return Returns a list
#' @export
import_qsm <- function(...) {
  warn(paste(
    "`import_qsm()` was deprecated in rTwig 1.4.0.",
    "i Please use `import_treeqsm()` instead.",
    sep = "\n"
  ))
  suppressMessages(import_treeqsm(...))
}
