#' Render an R Markdown Report
#'
#' This function renders an R Markdown report to the specified output file,
#' using the provided parameters. It generates a random temporary directory
#' for storing intermediate files during the rendering process and cleans up
#' afterwards.
#'
#' @param report_in The path to the R Markdown source file.
#' @param report_out The path where the rendered report should be saved.
#' @param render_pars A list of parameters to pass to the R Markdown document.
#' @return Invisible NULL. The function is called for its side effect of rendering a report.
#' @importFrom stringi stri_rand_strings
#' @export
render_report <- function(report_in, report_out, render_pars) {
  message("Generating report...")
  random_tmp <- file.path("/tmp", paste0("intermediates_", stringi::stri_rand_strings(1, 10)))
  rmarkdown::render(input = report_in,
    encoding = "UTF-8",
    output_file = report_out,
    # output_dir = random_tmp,
    params = render_pars,
    envir = new.env(),
    clean = TRUE,
    run_pandoc = TRUE,
    intermediates_dir = random_tmp)
  system(paste0("rm -rf ", random_tmp))
}
