#' Prepare Data for Case 1: No Facet, No Display Facet
#'
#' @param ddsList List containing DESeq2 datasets.
#' @param overallResListAll List containing all results.
#' @param overallResListDEGs List containing DEGs results.
#' @param rldList List containing regularized log2 transformed data.
#' @param mergedDEGsList List containing merged DEGs across all contrasts.
#' @param exp_metadata Experimental metadata data frame.
#' @param contrasts Contrasts data frame.
#' @return Environment with prepared data for the report.
#' @export
prepare_data_case1 <- function(ddsList, overallResListAll, overallResListDEGs, rldList, mergedDEGsList, exp_metadata, contrasts) {
  environment(
    dds = ddsList[['all']],
    resultsListAll = overallResListAll[['all']],
    resultsListDEGs = overallResListDEGs[['all']],
    rld = rldList[['all']],
    mergedDEGs = mergedDEGsList[['all']],
    exp_metadata_subset = exp_metadata,
    contrasts_subset = contrasts
  )
}

#' Prepare Data for Case 2: No Facet, Yes Display Facet
#'
#' @description The specifics of the preparation logic need to be filled in.
#' @param params List of parameters used for the analysis.
#' @param ... Other necessary parameters ...
#' @return Environment with prepared data for the report.
#' @export
prepare_data_case2 <- function(params, ...) {
  environment(
    # ... custom logic for preparing data ...
  )
}

#' Prepare Data for Case 3: Yes Facet, Yes Display Facet
#'
#' @description The specifics of the preparation logic need to be filled in.
#' @param params List of parameters used for the analysis.
#' @param ... Other necessary parameters ...
#' @return Environment with prepared data for the report.
#' @export
prepare_data_case3 <- function(params, ...) {
  environment(
    # ... custom logic for preparing data ...
  )
}

#' Prepare Data for Case 4: Invalid Configuration
#'
#' @description Stops the execution if the configuration is invalid.
#' @param params List of parameters used for the analysis.
#' @return None; function will stop execution with an error message.
#' @export
prepare_data_case4 <- function(params) {
  stop("Making a single report for faceted data not supported. Did you forget to set reports_facet?")
}