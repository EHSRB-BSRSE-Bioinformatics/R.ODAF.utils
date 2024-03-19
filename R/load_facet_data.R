#' Prepare Data For Report
#'
#' This function selects the appropriate data preparation function based on the
#' facet and display facet parameters and applies it to load necessary data objects
#' for the analysis report.
#'
#' @param paths A list of paths including the RData directory.
#' @param params A list of parameters used for the analysis.
#' @return An environment containing prepared data objects for reporting.
#' @export
prepare_data_for_report <- function(paths, params) {
  # Load data file into a new environment
  data_env <- new.env()
  data_file <- file.path(paths$RData, paste0(params$project_title, "_DEG_data.RData"))
  loaded_data <- load(data_file, envir = data_env)

  # Create an environment to store the result of the data preparation
  result_env <- new.env()

  # Case 1: DESeq2 on all samples; make reports for all samples.
  # Both "deseq_facet" and "reports_facet" are NA (unset).
  # (i.e., all groups and data in a single report)
  if (is.na(params$deseq_facet) && is.na(params$reports_facet)) {
    result_env <- prepare_data_case1(
      data_env$ddsList,
      data_env$overallResListAll,
      data_env$overallResListDEGs,
      data_env$rldList,
      data_env$mergedDEGsList,
      data_env$exp_metadata,
      data_env$contrasts
    )
  } else if (is.na(params$deseq_facet) && !is.na(params$reports_facet)) {
    result_env <- prepare_data_case2(params, data_env, ...)
  } else if (!is.na(params$deseq_facet) && !is.na(params$reports_facet)) {
    result_env <- prepare_data_case3(params, data_env, ...)
  } else {
    result_env <- prepare_data_case4(params, data_env, ...)
  }

  return(result_env)
}
