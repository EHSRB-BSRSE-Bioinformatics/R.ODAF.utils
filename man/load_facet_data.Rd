% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load_facet_data.R
\name{load_facet_data}
\alias{load_facet_data}
\title{Prepare Data For Report}
\usage{
load_facet_data(paths, params, facet_override = NA, filter_override = NA)
}
\arguments{
\item{paths}{A list of paths including the RData directory.}

\item{params}{A list of parameters used for the analysis.}

\item{facet_override}{A character string to override the facet parameter. Mostly used for troubleshooting.}
}
\value{
An environment containing prepared data objects for reporting.
}
\description{
This function selects the appropriate data preparation function based on the
facet and display facet parameters and applies it to load necessary data objects
for the analysis report.
}
