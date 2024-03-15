#' Replace NULL Values in Configuration List with NAs
#'
#' This function replaces `NULL` values in a configuration list with `NA`
#' to avoid issues with unassigned list elements during the configuration setup.
#'
#' @param params A list of configuration parameters.
#' @return The configuration list with `NULL` values replaced by `NA`.
#' @export
replace_nulls_in_config <- function(params) {
  params_new <- list()
  for (name in names(params)) {
    param <- params[[name]]
    if (is.null(param)) {
      params_new[[name]] <- NA
    } else {
      params_new[[name]] <- param
    }
  }
  return(params_new)
}
