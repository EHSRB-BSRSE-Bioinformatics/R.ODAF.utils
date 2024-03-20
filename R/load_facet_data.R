load_facet_data <- function(paths, params, output_env) {
  facet_env <- prepare_data_for_report(paths, params)
  # List all object names within the source environment
  obj_names <- ls(envir = facet_env)

  # Loop over object names, retrieve the objects, and assign them to the global environment
  for (obj_name in obj_names) {
    obj_value <- get(x = obj_name, envir = facet_env)
    assign(x = obj_name, value = obj_value, envir = output_env)
  }
}



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
  load(data_file, envir = data_env)

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
  # Case 2: DESeq2 on all samples; but, make faceted reports.
  # "deseq_facet" is NA, but "reports_facet" is set.
  } else if (is.na(params$deseq_facet) && !is.na(params$reports_facet)) {
    result_env <- prepare_data_case2(
      params,
      data_env$ddsList,
      data_env$overallResListAll,
      data_env$overallResListDEGs,
      data_env$rldList,
      data_env$mergedDEGsList,
      data_env$exp_metadata,
      data_env$designList,
      data_env$contrasts
    )
  # Case 3: DESeq2 is faceted; reports are faceted.
  # The two facets must match.
  } else if (!is.na(params$deseq_facet) && !is.na(params$reports_facet)) {
    if(params$deseq_facet != params$reports_facet) {
      stop("Error: reports_facet must match deseq_facet, otherwise DESeq2 results get mixed and matched.")
    }
    result_env <- prepare_data_case3(
      params,
      data_env$ddsList,
      data_env$overallResListAll,
      data_env$overallResListDEGs,
      data_env$rldList,
      data_env$mergedDEGsList,
      data_env$designList,
      data_env$contrastsList
    )
  # Case 4: DESeq2 is faceted, reports are not: this one doesn't make sense, since it could mislead end-users.
  } else {
    stop("Making a single report for faceted data not supported. Did you forget to set reports_facet?")
  }
  # filter the regularized data a couple ways for different displays    
  rld_DEGs <- rld[row.names(assay(rld)) %in% mergedDEGs]

  rv = rowVars(assay(rld), useNames = FALSE)
  select = order(rv, decreasing = TRUE)[1:params$nBest]
  rld_top <- rld[select,]
  select_heatmap = order(rv, decreasing = TRUE)[1:params$nHeatmapDEGs]
  rld_top_heatmap <- rld[select_heatmap,]

  allResults <- annotate_deseq_table(resultsListAll, params, filter_results = F)
  significantResults <- annotate_deseq_table(resultsListDEGs, params, filter_results = F)

  ordered_contrast_strings <- contrasts %>% mutate(contrast_string = paste(V1,'vs',V2,sep=" ")) %>% pull(contrast_string)

  allResults$contrast <- factor(allResults$contrast, levels = ordered_contrast_strings)
  significantResults$contrast <- factor(significantResults$contrast, levels = ordered_contrast_strings)
  return(result_env)
}
