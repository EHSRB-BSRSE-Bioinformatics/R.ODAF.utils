#' Set up Project Paths
#'
#' This function creates a list of paths that the analysis will use to store inputs and outputs.
#' It ensures that each directory exists, creating them if they do not.
#'
#' @param params List of parameters with directory paths and project settings.
#' @param results_location_arg Character string specifying subfolder as destination for results in faceted reports.
#' @param metadata Data frame containing metadata for the samples.
#' @param make_deseq2_dirs Logical indicating whether to create DESeq2 subdirectories.
#' @param make_report_dirs Logical indicating whether to create report subdirectories.
#' @importFrom here here
#' @return A list of paths used throughout the analysis.
#' @export
set_up_filepaths <- function(params,
                             results_location_arg = NULL,
                             metadata = NULL,
                             make_deseq2_dirs = FALSE,
                             make_report_dirs = FALSE) {
  paths <- list()
  # For project structure
  message("Determining project directory...")
  if (is.na(params$projectdir)) {
    params$projectdir <- here::here()
    paths$projectdir <- here::here()
  } else {
    paths$projectdir <- file.path(params$projectdir)
  }
  message("Setting up or checking initial paths...")
  paths$inputs <- file.path(paths$projectdir, "inputs")
  paths$output <- file.path(paths$projectdir, "output")
  paths$raw <- file.path(paths$inputs, "raw")
  paths$processed <- file.path(paths$output, "processed")
  paths$metadata <- file.path(paths$inputs, "metadata")
  paths$contrasts <- file.path(paths$inputs, "contrasts")
  paths$qc <- file.path(paths$output, "QC")
  paths$details <- file.path(paths$qc, "details")
  if (!is.null(results_location_arg)) {
    paths$results <- file.path(paths$output, "analysis", results_location_arg)
    paths$BMD_output <- file.path(paths$results, "BMD_and_biomarker_files")
    paths$RData <- file.path(paths$results, "RData")
    paths$record <- file.path(paths$results, "Pipeline_record")
    paths$pathway_analysis <- file.path(paths$results, "pathway_analysis")
  }
  message("Creating directories if they don't exist...")
  lapply(paths, function(x) if (!dir.exists(x)) dir.create(x, recursive = TRUE))

  # Only create subdirectories if enabled
  if (make_deseq2_dirs) {
    message("Creating subdirectories for DESeq2...")
    paths <- create_deseq_subdirs(paths, metadata, params)
  }
  if (make_report_dirs) {
    message("Creating subdirectories for reports...")
    paths <- create_report_subdirs(paths, metadata, params)
  }
  return(paths)
}

create_deseq_subdirs <- function(paths, metadata, params) {
  paths$wikipathways <- file.path(params$wikipathways_directory)
  # When DESeq2 facets are not set, create a single output folder for DESeq2 DEG lists
  if (is.na(params$deseq_facet) || is.null(params$deseq_facet)) {
    paths$DEG_output <- file.path(paths$results, "DEG_lists")
    paths$biosets_output <- paths$BMD_output
  } else {
    facets <- get_facets(metadata, params)
    # Make an output folder for each facet
    for (f in facets){
      paths$DEG_output[[f]] <- file.path(paths$results, "DEG_lists", paste0(f))
      paths$biosets_output[[f]] <- file.path(paths$BMD_output, paste0(f))
    }
  }
  lapply(paths$DEG_output, function(x) if (!dir.exists(x)) dir.create(x, recursive = TRUE))
  lapply(paths$biosets_output, function(x) if (!dir.exists(x)) dir.create(x, recursive = TRUE))
  return(paths)
}

create_report_subdirs <- function(paths, metadata, params) {
  # TODO - maybe we should add a check that the DESeq2 directories are already made? Doesn't make sense to run this in a directory structure where that hasn't been done.
  paths$wikipathways <- file.path(params$wikipathways_directory)
  # Make directory for DESeq2 Reports
  paths$reports_dir <- file.path(paths$results, "reports")
  paths$pathway_analysis <- file.path(paths$results, "pathway_analysis")
  # When report facets are not set, create a single output folder for reports
  if (is.na(params$reports_facet) || is.null(params$reports_facet)) {
    if (!dir.exists(paths$DEG_output)) {
      dir.create(paths$DEG_output, recursive = TRUE)
    } else {
      facets <- get_facets(metadata, params)
      # Make a report output folder for each facet
      paths$pathway_analysis <- c()
      for (f in facets){
        paths$pathway_analysis[[f]] <- file.path(paths$results, "pathway_analysis", paste0(f))
        paths$DEG_output[[f]] <- file.path(paths$results, "reports", paste0(f))
      }
    }
    if (!dir.exists(paths$reports_dir)) {
      dir.create(paths$reports_dir, recursive = TRUE)
    }
    lapply(paths$pathway_analysis, function(x) if (!dir.exists(x)) dir.create(x, recursive = TRUE))
    lapply(paths$DEG_output, function(x) if (!dir.exists(x)) dir.create(x, recursive = TRUE))
    return(paths)
  }
}
