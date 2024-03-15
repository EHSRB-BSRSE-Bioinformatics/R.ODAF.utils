# #' Set up Project Paths
# #'
# #' This function creates a list of paths that the analysis will use to store inputs and outputs.
# #' It ensures that each directory exists, creating them if they do not.
# #' It uses a structured approach to create nested directories as needed.
# #'
# #' @param params List of parameters with directory paths and project settings.
# #' @param results_location_arg Character string specifying subfolder as destination for results in faceted reports.
# #' @return A list of paths used throughout the analysis.
# #' @export
# set_up_paths <- function(params,
#                          exp_metadata,
#                          results_location_arg = NULL) {
#   base_paths <- c("inputs", "output")
#   input_subpaths <- c("raw", "metadata", "contrasts")
#   output_subpaths <- c("processed") # 'analysis' subpath will be conditionally assigned below

#   # Other important system paths to specify in config
#   paths <- assemble_paths(params[["projectdir"]], base_paths, "projectdir")
#   paths[["inputs"]] <- assemble_paths(paths[["inputs"]], input_subpaths, "inputs")
#   paths[["output"]] <- assemble_paths(paths[["output"]], output_subpaths, "outputs")

#   # Conditionally set the 'results' path based on results_location_arg
#   if (is.null(results_location_arg)) {
#     paths[["results"]] <- file.path(paths[["output"]], "analysis")
#   } else {
#     paths[["results"]] <- file.path(paths[["output"]], "analysis", results_location_arg)
#   }

#   # Define subpaths that depend on 'results'
#   results_subpaths <- c("BMD_and_biomarker_files", "RData", "Pipeline_record", "pathway_analysis")
#   paths[["results"]] <- assemble_paths(paths[["results"]], results_subpaths)

#   # Conditionally add DEG_output and biosets_output
#   if (is.na(params[["deseq_facet"]]) || is.null(params[["deseq_facet"]])) {
#     paths[["DEG_output"]] <- file.path(paths[["results"]], "DEG_lists")
#     paths[["biosets_output"]] <- paths[["BMD_output"]]
#   } else {
#     # make multiple outputs for different facets
#     for (f in facets) {
#       paths[["DEG_output"]][[f]] <- file.path(paths[["results"]], "DEG_lists", paste0(f))
#       paths[["biosets_output"]][[f]] <- file.path(paths[["BMD_output"]], paste0(f))
#     }
#   }

#   # Additional conditional setup for display facets, if provided
#   if (!is.na(params[["display_group_facet"]]) && !is.null(params[["display_facets"]])) {
#       paths[["pathway_analysis"]] <- assemble_paths(paths[["results"]], params[["display_facets"]])
#   }

#   # Create the directories if they don't exist
#   create_dirs_recursively(paths)
#   return(paths)
# }





# if (is.na(params[["deseq_facet"]]) || is.null(params[["deseq_facet"]])) {
#     paths[["DEG_output"]] <- file.path(paths[["results"]], "DEG_lists")
#     paths[["biosets_output"]] <- paths[["BMD_output"]]
#   } else {
#     # make multiple outputs for different facets
#     for (f in facets) {
#       paths[["DEG_output"]][[f]] <- file.path(paths[["results"]], "DEG_lists", paste0(f))
#       paths[["biosets_output"]][[f]] <- file.path(paths[["BMD_output"]], paste0(f))
#     }
#   }










# #' Extend existing list of paths with new subpaths.
# #' If the base_path is a string, it will be added as a top-level path with the same name as the first subpath.
# assemble_paths <- function(base_path, subpaths, base_path_key) {
#   paths <- list()
  
#   # Always include the base path under the specified key
#   paths[[base_path_key]] <- base_path
  
#   # Add or update the subpaths
#   for (subpath in subpaths) {
#     paths[[subpath]] <- file.path(base_path, subpath)
#   }
  
#   return(paths)
# }


# #' Create directories from paths recursively.
# create_dirs_recursively <- function(paths) {
#   lapply(paths, function(x) {
#     if (is.list(x)) {
#       create_dirs_recursively(x)
#     } else {
#       if (!dir.exists(x)) dir.create(x, recursive = TRUE)
#     }
#   })
#   return(paths)
# }
















# #' Set up Project Paths
# #'
# #' This function creates a list of paths that the analysis will use to store inputs and outputs.
# #' It ensures that each directory exists, creating them if they do not.
# #'
# #' @param params List of parameters with directory paths and project settings.
# #' @param results_location_arg Character string specifying subfolder as destination for results in faceted reports.
# #' @param facets Character vector of facets to generate paths for.
# #' @param report_facets Character vector of display facets for pathway analysis.
# #' @return A list of paths used throughout the analysis.
# #' @export
# set_up_paths <- function(params, results_location_arg = NULL, facets, report_facets) {
#   # TODO see if set_up_paths functions can be simplified and combined
#   paths <- list()
#   # Other important system paths to specify in config
#   paths[["wikipathways"]] <- params[["wikipathways_directory"]]
#   # For project structure
#   paths[["projectdir"]] <- params[["projectdir"]]
#   paths[["inputs"]] <- file.path(paths[["projectdir"]], "inputs")
#   paths[["output"]] <- file.path(paths[["projectdir"]], "output")
#   paths[["qc"]] <- file.path(paths[["output"]], "QC")
#   paths[["details"]] <- file.path(paths[["qc"]], "details")
#   paths[["raw"]] <- file.path(paths[["inputs"]], "raw")
#   paths[["processed"]] <- file.path(paths[["output"]], "processed")
#   paths[["metadata"]] <- file.path(paths[["inputs"]], "metadata")
#   paths[["contrasts"]] <- file.path(paths[["inputs"]], "contrasts")
#   if (is.null(results_location_arg)) {
#     paths[["results"]] <- file.path(paths[["output"]], "analysis")
#   } else {
#     paths[["results"]] <- file.path(paths[["output"]], "analysis", results_location_arg)
#   }
#   paths[["BMD_output"]] <- file.path(paths[["results"]], "BMD_and_biomarker_files")
#   paths[["RData"]] <- file.path(paths[["results"]], "RData")
#   paths[["record"]] <- file.path(paths[["results"]], "Pipeline_record")
#   paths[["pathway_analysis"]] <- file.path(paths[["results"]], "pathway_analysis")
#   lapply(paths, function(x) if (!dir.exists(x)) dir.create(x, recursive = TRUE))

#   if (is.na(params[["deseq_facet"]]) || is.null(params[["deseq_facet"]])) {
#     paths[["DEG_output"]] <- file.path(paths[["results"]], "DEG_lists")
#     paths[["biosets_output"]] <- paths[["BMD_output"]]
#   } else {
#     # make multiple outputs for different facets
#     for (f in facets) {
#       paths[["DEG_output"]][[f]] <- file.path(paths[["results"]], "DEG_lists", paste0(f))
#       paths[["biosets_output"]][[f]] <- file.path(paths[["BMD_output"]], paste0(f))
#     }
#   }
#   lapply(paths[["DEG_output"]], function(x) if (!dir.exists(x)) dir.create(x, recursive = TRUE))
#   lapply(paths[["biosets_output"]], function(x) if (!dir.exists(x)) dir.create(x, recursive = TRUE))

#   if (is.na(params[["reports_facet"]]) || is.null(params[["reports_facet"]])) {
#     paths[["pathway_analysis"]] <- file.path(paths[["results"]], "pathway_analysis")
#   } else {
#     # make multiple outputs for different facets
#     paths[["pathway_analysis"]] <- c()
#     for (f in report_facets) {
#       paths[["pathway_analysis"]][[f]] <- file.path(paths[["results"]], "pathway_analysis", paste0(f))
#     }
#   }
#   lapply(paths[["pathway_analysis"]], function(x) if (!dir.exists(x)) dir.create(x, recursive = TRUE))

#   return(paths)
# }

# #' Set up Paths for DEG Output and BMD Output
# #'
# #' This function updates a list of paths with directories for DEG lists and BMD outputs.
# #' It creates directories for each facet if they do not exist.
# #'
# #' @param paths List of existing paths.
# #' @param params List of parameters with directory paths and project settings.
# #' @param facets Character vector of facets to generate paths for.
# #' @param report_facets Character vector of display facets.
# #' @return Updated list of paths with DEG and BMD output paths.
# #' @export
# set_up_paths_2 <- function(paths, params, facets, report_facets) {
#   if (is.na(params[["deseq_facet"]]) || is.null(params[["deseq_facet"]])) {
#     paths[["DEG_output"]] <- file.path(paths[["results"]], "DEG_lists")
#     paths[["biosets_output"]] <- paths[["BMD_output"]]
#   } else {
#     # make multiple outputs for different facets
#     for (f in facets) {
#       paths[["DEG_output"]][[f]] <- file.path(paths[["results"]], "DEG_lists", paste0(f))
#       paths[["biosets_output"]][[f]] <- file.path(paths[["BMD_output"]], paste0(f))
#     }
#   }
#   lapply(paths[["DEG_output"]], function(x) if (!dir.exists(x)) dir.create(x, recursive = TRUE))
#   lapply(paths[["biosets_output"]], function(x) if (!dir.exists(x)) dir.create(x, recursive = TRUE))
#   return(paths)
# }

# #' Set up Pathway Analysis Paths
# #'
# #' This function updates a list of paths with directories for pathway analysis outputs.
# #' It creates directories for each display facet if they do not exist.
# #'
# #' @param paths List of existing paths.
# #' @param params List of parameters with directory paths and project settings.
# #' @param report_facets Character vector of display facets for pathway analysis.
# #' @return Updated list of paths with pathway analysis paths.
# #' @export
# set_up_paths_3 <- function(paths, params, report_facets) {
#   if (is.na(params[["reports_facet"]]) || is.null(params[["reports_facet"]])) {
#     paths[["pathway_analysis"]] <- file.path(paths[["results"]], "pathway_analysis")
#   } else {
#     # make multiple outputs for different facets
#     paths[["pathway_analysis"]] <- c()
#     for (f in report_facets) {
#       paths[["pathway_analysis"]][[f]] <- file.path(paths[["results"]], "pathway_analysis", paste0(f))
#     }
#   }
#   lapply(paths[["pathway_analysis"]], function(x) if (!dir.exists(x)) dir.create(x, recursive = TRUE))
#   return(paths)
# }
