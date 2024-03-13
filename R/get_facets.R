#' Get Unique Facets from Metadata
#'
#' This function extracts unique values from a specified column in the metadata,
#' excluding any groups specified by the user and an additional skip value.
#'
#' @param metadata A data frame containing the experimental metadata.
#' @param exclude A character vector of group names to be excluded from the facets.
#' @param display_facet A character string specifying the column name in `metadata`
#'   from which to extract the facets.
#' @param skip_extra A character string specifying an additional group name to be
#'   excluded from the facets.
#' @return A character vector of unique facet values after exclusion.
#' @export
#' @examples
#' # Assuming `exp_metadata` is a data frame with a column named "treatment":
#' facets <- get_facets(metadata = exp_metadata,
#'     exclude = c("control"),
#'     display_facet = "treatment",
#'     skip_extra = "DMSO")
#' print(facets)
get_facets <- function(metadata = exp_metadata,
                       exclude = params$exclude_groups,
                       display_facet = params$display_group_facet,
                       skip_extra = "DMSO") {
    facets <- metadata %>%
        filter(!(!!sym(display_facet)) %in%
            c(exclude, skip_extra)) %>%
        pull(display_facet) %>%
        unique()
    message(paste0("Making multiple reports based on ",
        display_facet, "..."))
    return(facets)
}
