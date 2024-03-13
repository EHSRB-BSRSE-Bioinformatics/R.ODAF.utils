#' Retrieve and Filter DESeq2 Analysis Results
#'
#' This function annotates DESeq2 results with differential expression data,
#' performs filter-based post-processing on the DEGs, and returns a comprehensive
#' list of outcomes including filtered and unfiltered results.
#'
#' @param dds DESeq2DataSet object that has been run through the DESeq pipeline.
#' @param exp_metadata Data frame containing experimental metadata.
#' @param contrasts Matrix where each row is a contrast with two conditions.
#' @param design Character string specifying the primary design variable in the metadata.
#' @param params List containing analysis parameters like adjusted p-value cutoffs.
#' @param current_group_filter Optional current group filter string being applied.
#' @return A list of data frames or lists containing the analysis results for DEGs, including
#'         merged DEGs across contrasts, and various filter-based subsets of DEGs.
#' @export
#' @importFrom DESeq2 results lfcShrink counts
#' @examples
#' results <- get_DESeq_results(dds, metadata, contrasts_matrix, "condition", params, "treatment_group")
get_DESeq_results <- function(dds,
                              exp_metadata,
                              contrasts,
                              design,
                              params,
                              current_group_filter) {
  # Initial setup for DESeq2 contrasts
  resListAll <- list()
  resListFiltered <- list()
  resListDEGs <- list()
  filtered_table <- data.frame()
  Counts  <- DESeq2::counts(dds, normalized = TRUE)
  CPMdds  <- edgeR::cpm(DESeq2::counts(dds, normalized = TRUE))
  mergedDEGs <- c()

  for (x in 1:nrow(contrasts)) {  # For all comparisons to be done
    condition1 <- contrasts[x, 2] # Control
    condition2 <- contrasts[x, 1] # Experimental

    contrast_string <- paste(condition2, "vs", condition1, sep = "_")

    message(contrast_string)

    exp_metadata_subset <- as.matrix(exp_metadata[exp_metadata[, design] %in% c(condition1, condition2),])

    # sanity checks
    stopifnot(exprs = {
      nrow(exp_metadata_subset) > 0
    })

    # Filter results using R-ODAF filters
    Filter <- matrix(data = NA, ncol = 3, nrow = nrow(Counts))
    rownames(Filter) <- rownames(Counts) # genes
    colnames(Filter) <- c("relevance", "quantile", "spike")

    # Apply the "Relevance" condition
    message(paste0("Filtering genes: 75% of at least one group need to be above ", params$MinCount, " CPM"))
    SampPerGroup <- table(exp_metadata_subset[, design])
    gene_relevance_filter <- apply_relevance_filter(CPMdds, exp_metadata_subset, SampPerGroup, params$MinCount)
    Filter[, "relevance"] <- gene_relevance_filter

    compte <- Counts[gene_relevance_filter,]

    Filter <- Filter[rownames(Filter) %in% rownames(compte), , drop = F]

    #save all genes that are present regardless of counts
    dfGenes <- data.frame(Ensembl_Gene_ID = rownames(Counts))

    intitial_count <- nrow(dds)
    num_relevance_filtered <- nrow(dds) - nrow(Filter)
    message(paste0("Relevance filtering removed ", num_relevance_filtered,
                   " genes from the ", nrow(dds)," assessed. ",
                   nrow(Filter), " genes remaining"))

    # run the DEseq p-value calculation
    message("Obtaining the DESeq2 results")
    currentContrast <- c(design, condition2, condition1)
    bpparam <- MulticoreParam(params$cpus)
    res <- DESeq2::results(dds[rownames(compte),],
                           parallel = TRUE,
                           BPPARAM = bpparam,
                           contrast = currentContrast,
                           alpha = params$alpha,
                           pAdjustMethod = 'fdr',
                           cooksCutoff = params$cooks) # If Cooks cutoff disabled - manually inspect.

    res <- lfcShrink(dds,
                     contrast = currentContrast,
                     res = res,
                     BPPARAM = bpparam,
                     type = "ashr")

    resListAll[[contrast_string]] <- res

    DEsamples <- subset(res, res$padj < params$alpha)
    if (nrow(DEsamples) == 0) {
      print("No significant results were found for this contrast. Moving on...")
      next
    }
    DECounts <- Counts[gene_relevance_filter,]

    message("Check median against third quantile")
    message("AND")
    message("Check for the presence of a spike")

    quant_spike_results <- apply_quant_spike_filters(DECounts,
                                                     exp_metadata_subset,
                                                     SampPerGroup,
                                                     design)

    Filter[rownames(DECounts), "quantile"] <- quant_spike_results[, "quantilePass"]
    Filter[rownames(DECounts), "spike"] <- quant_spike_results[, "spikePass"]

    # Extract the final list of DEGs

    message(paste0("Filtering by linear fold-change: linear FC needs to be above ", params$linear_fc_filter_DEGs))

    allCounts_all_filters <- res[rowSums(Filter) == 3 ,]
    DECounts_real <- DEsamples[rowSums(Filter) == 3 & !is.na(DEsamples$padj) &  abs(DEsamples$log2FoldChange) > log2(params$linear_fc_filter_DEGs) ,]
    DECounts_no_quant <- DEsamples[Filter[, 2] == 0 ,] # save these to output later
    DECounts_spike <- DEsamples[Filter[, 3] == 0 ,] # save these to output later
    #TODO: output quantile rule failing and spike failing genes

    message(paste0("A total of ", nrow(DECounts_real),
                   " DEGs were selected (out of ", nrow(DECounts) ,"), after ", nrow(DECounts_no_quant),
                   " genes(s) removed by the quantile rule, ", nrow(DECounts_spike),
                   " gene(s) with a spike, and linear fold-change filtering was applied"))
    message("DESeq2 Done")

    mergedDEGs <- c(mergedDEGs, rownames(DECounts_real))

    filtered_table <- rbind(filtered_table, data.frame(facet = current_group_filter,
                                                       contrast = contrast_string,
                                                       initial = intitial_count,
                                                       relevance_filtered = num_relevance_filtered,
                                                       quantile_filtered = nrow(DECounts_no_quant),
                                                       spike_filtered = nrow(DECounts_spike),
                                                       passed_all_filters = nrow(DECounts_real)))

    # TODO: the sapply at the end should be handling this, why doesn't it work?
    if (nrow(DECounts_real) > 0){
      resListDEGs[[contrast_string]] <- DECounts_real
    }
    if (nrow(DECounts_real) > 0){
      resListFiltered[[contrast_string]] <- allCounts_all_filters
    }
  }
  # If there are no significant results - remove the empty contrast from the list:
  resListAll <- resListAll[!sapply(resListAll, is.null)]
  resListFiltered <- resListFiltered[!sapply(resListFiltered, is.null)]
  resListDEGs <- resListDEGs[!sapply(resListDEGs, is.null)]

  mergedDEGs <- unique(mergedDEGs)
  return(
    list(
      dfGenes=dfGenes,
      resListAll=resListAll,
      resListFiltered=resListFiltered,
      resListDEGs=resListDEGs,
      mergedDEGs=mergedDEGs,
      filtered_table=filtered_table
    )
  )
}


#' Apply R-ODAF Quantile and Spike Filters to DESeq2 counts
#'
#' This function applies post-processing filters to differentially expressed genes (DEGs).
#' It checks whether DEGs pass the median vs third quantile check and spike condition.
#'
#' @param DECounts Matrix or data frame with DEGs and their corresponding count data.
#' @param exp_metadata_subset Data frame containing the experimental metadata subset for the current comparison.
#' @param SampPerGroup Named vector with the count of samples per group.
#' @param design The design variable that separates groups in the metadata subset.
#' @importFrom data.table setDT data.table
#' @importFrom stats quantile median
#' @return A matrix with logical values indicating whether each gene passed the quantile
#'         and spike filters, respectively ('1' indicates that the gene passed the filter).
#' @export
#' @examples
#' filter_matrix <- apply_quant_spike_filters(DECounts, metadata_subset, SampPerGroup, "condition")

apply_quant_spike_filters <- function(DECounts, exp_metadata_subset, SampPerGroup, design) {
# Convert to data.table
data.table::setDT(DECounts, keep.rownames = "gene")
data.table::setDT(exp_metadata_subset)

# Calculate the quantile threshold for each group
quantile_thresholds <- exp_metadata_subset[, .(threshold = stats::quantile(counts, 0.75)),
                                           by = .(group = get(design)),
                                           .(counts = DECounts[[get(design)]])]

# Merge the thresholds back into the DECounts data.table
DECounts <- merge(DECounts, quantile_thresholds, by.x = "gene", by.y = "group")

# Apply the median > quantile threshold and spike checks
DECounts[, `:=` (quantilePass = stats::median(counts) > threshold,
                 spikePass = max(counts) / sum(counts) < 1.4 * (SampPerGroup[group])^(-0.66)),
         by = .(gene)]

# Return results
Filter <- DECounts[, .(quantilePass, spikePass), by = .(gene)]
return(Filter)
}

#' Apply Relevance Filter to Genes Based on Counts
#'
#' This function filters genes based on a minimum count threshold. For a gene to pass the filter,
#' a specified proportion of its counts must be above the threshold in at least one of the conditions.
#'
#' @param CPMdds Matrix of normalized counts as CPM (Counts Per Million).
#' @param exp_metadata_subset Data frame containing the experimental metadata subset for the current comparison.
#' @param SampPerGroup Named vector with the count of samples per condition/group.
#' @param MinCount The minimum count threshold to consider a gene as relevant.
#' @param threshold_proportion The proportion of samples within a group that must exceed the count threshold.
#' @return Logical vector indicating whether each gene has passed the relevance filter.
#' @export
apply_relevance_filter <- function(CPMdds, exp_metadata_subset, SampPerGroup, MinCount, threshold_proportion = 0.75) {
  gene_relevance <- apply(CPMdds, 1, function(gene_counts, exp_metadata_subset, SampPerGroup) {
    counts_pass <- sapply(names(SampPerGroup), function(group) {
      sampleNamesGroup <- exp_metadata_subset[exp_metadata_subset[, design] == group, "original_names"]
      sum(gene_counts[sampleNamesGroup] >= MinCount) >= threshold_proportion * SampPerGroup[group]
    })
    any(counts_pass) # Pass if any group has enough relevant counts
  }, exp_metadata_subset, SampPerGroup)

  return(gene_relevance)
}
