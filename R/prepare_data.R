#' Prepare Data for Case 1: No DESeq2 Facet, No Report Facet
#' 
#' This will generate a single report for all samples. All samples will have been processed together in DESeq2.
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
prepare_data_case1 <- function(ddsList,
                               overallResListAll,
                               overallResListDEGs,
                               rldList,
                               mergedDEGsList,
                               exp_metadata,
                               contrasts) {
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

#' Prepare Data for Case 2: No DESeq2 Facet, Yes Report Facet
#'
#' DESeq2 is on all samples; but, faceted reports should be made.
#' "deseq_facet" is NA, but "reports_facet" is set.
#'
#' @description The specifics of the preparation logic need to be filled in.
#' @param params List of parameters used for the analysis.
#' @param ... Other necessary parameters ...
#' @return Environment with prepared data for the report.
#' @export
prepare_data_case2 <- function(params,
                               ddsList,
                               overallResListAll,
                               overallResListDEGs,
                               rldList,
                               mergedDEGsList,
                               exp_metadata,
                               designList,
                               contrasts) {
  environment(
   # The data isn't already faceted but we need
   # to extract (subset) the data for the
   # facet we need
   display_group_filter <- params$display_group_filter,
   dds_all <- ddsList[['all']],
   resultsListAll_all <- overallResListAll[['all']],
   resultsListDEGs_all <- overallResListDEGs[['all']],
   rld_all <- rldList[['all']],
# TODO - can designList be substituted with exp_metadata here?
   metadata_subset <- subset_metadata(designList[['all']], design_to_use, contrasts, params$reports_facet, display_group_filter),
   exp_metadata_subset <- metadata_subset$exp_metadata,
   contrasts_subset <- metadata_subset$contrasts,
   dds_subset <- subset_data(dds_all, exp_metadata_subset),
   rld_subset <- subset_data(rld_all, exp_metadata_subset),
   contrast_strings <- contrasts_subset %>%
     mutate(contrast_string = paste(V1, V2, sep="_vs_")) %>% pull(contrast_string),
   resultsListAll_subset <- resultsListAll_all[contrast_strings],
   resultsListDEGs_subset <- resultsListDEGs_all[contrast_strings],
   exp_metadata <- exp_metadata_subset,
   contrasts <- contrasts_subset,
   dds <- dds_subset,
   resultsListAll <- resultsListAll_subset,
   resultsListDEGs <- resultsListDEGs_subset,
   rld <- rld_subset,
   # note, in this case the calculated merged DEGs will be for the whole experiment, not the display facet
   # so let's recalculate them
   mergedDEGs <- unique(unlist(sapply(resultsListDEGs, rownames), use.names = FALSE))
   # TODO: add some tests here to make sure everything worked properly
  )
}

#' Prepare Data for Case 3: Yes Facet, Yes Display Facet
#'
#' @description The specifics of the preparation logic need to be filled in.
#' @param params List of parameters used for the analysis.
#' @param ... Other necessary parameters ...
#' @return Environment with prepared data for the report.
#' @export
prepare_data_case3 <- function(params,
                               ddsList,
                               overallResListAll,
                               overallResListDEGs,
                               rldList,
                               mergedDEGsList,
                               designList,
                               contrastsList) {
  environment(
   display_group_filter <- params$display_group_filter,
   dds <- ddsList[[display_group_filter]],
   resultsListAll <- overallResListAll[[display_group_filter]],
   resultsListDEGs <- overallResListDEGs[[display_group_filter]],
   rld <- rldList[[display_group_filter]],
   mergedDEGs <- mergedDEGsList[[display_group_filter]],
   exp_metadata_subset <- designList[[display_group_filter]],
   contrasts_subset <- contrastsList[[display_group_filter]]
  )
}
