#' Set up Project Paths
#'
#' This function creates a list of paths that the analysis will use to store inputs and outputs.
#' It ensures that each directory exists, creating them if they do not.
#'
#' @param params List of parameters with directory paths and project settings.
#' @return A list of paths used throughout the analysis.
#' @export
set_up_paths <- function(params, results_location_arg = NULL) {
  # TODO see if set_up_paths functions can be simplified and combined
  paths <- list()
  # Other important system paths to specify in config
  paths$wikipathways <- params$wikipathways_directory
  # For project structure
  paths$root <- params$projectdir
  paths$inputs <- file.path(paths$root, "inputs")
  paths$output <- file.path(paths$root, "output")
  paths$raw <- file.path(paths$inputs, "raw")
  paths$processed <- file.path(paths$output, "processed")
  paths$metadata <- file.path(paths$inputs, "metadata")
  paths$contrasts <- file.path(paths$inputs, "contrasts")
  paths$results <- file.path(paths$output, "analysis", results_location_arg)
  paths$BMD_output <- file.path(paths$results, "BMD_and_biomarker_files")
  paths$RData <- file.path(paths$results, "RData")
  paths$record <- file.path(paths$results, "Pipeline_record")
  paths$pathway_analysis <- file.path(paths$results, "pathway_analysis")
  lapply(paths, function(x) if (!dir.exists(x)) dir.create(x, recursive = TRUE))
  return(paths)
}

#' Set up Paths for DEG Output and BMD Output
#'
#' This function updates a list of paths with directories for DEG lists and BMD outputs.
#' It creates directories for each facet if they do not exist.
#'
#' @param paths List of existing paths.
#' @param params List of parameters with directory paths and project settings.
#' @param facets Character vector of facets to generate paths for.
#' @param display_facets Character vector of display facets.
#' @return Updated list of paths with DEG and BMD output paths.
#' @export
set_up_paths_2 <- function(paths, params, facets, display_facets) {
  if (is.na(params$group_facet) || is.null(params$group_facet)) {
    paths$DEG_output <- file.path(paths$results, "DEG_lists")
    paths$biosets_output <- paths$BMD_output
  } else {
    # make multiple outputs for different facets
    for (f in facets) {
      paths$DEG_output[[f]] <- file.path(paths$results, "DEG_lists", paste0(f))
      paths$biosets_output[[f]] <- file.path(paths$BMD_output, paste0(f))
    }
  }
  lapply(paths$DEG_output, function(x) if (!dir.exists(x)) dir.create(x, recursive = TRUE))
  lapply(paths$biosets_output, function(x) if (!dir.exists(x)) dir.create(x, recursive = TRUE))
  return(paths)
}

#' Set up Pathway Analysis Paths
#'
#' This function updates a list of paths with directories for pathway analysis outputs.
#' It creates directories for each display facet if they do not exist.
#'
#' @param paths List of existing paths.
#' @param params List of parameters with directory paths and project settings.
#' @param display_facets Character vector of display facets for pathway analysis.
#' @return Updated list of paths with pathway analysis paths.
#' @export
set_up_paths_3 <- function(paths, params, display_facets) {
  if (is.na(params$display_group_facet) || is.null(params$display_group_facet)) {
    paths$pathway_analysis <- file.path(paths$results, "pathway_analysis")
  } else {
    # make multiple outputs for different facets
    paths$pathway_analysis <- c()
    for (f in display_facets) {
      paths$pathway_analysis[[f]] <- file.path(paths$results, "pathway_analysis", paste0(f))
    }
  }
  lapply(paths$pathway_analysis, function(x) if (!dir.exists(x)) dir.create(x, recursive = TRUE))
  return(paths)
}
