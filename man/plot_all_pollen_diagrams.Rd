% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_all_pollen_diagrams.R
\name{plot_all_pollen_diagrams}
\alias{plot_all_pollen_diagrams}
\title{Plot pollen diagram for all records}
\usage{
plot_all_pollen_diagrams(
  data_source,
  dir,
  min_n_occur = 3,
  max_taxa = 20,
  y_var = c("age", "depth"),
  date
)
}
\arguments{
\item{data_source}{Data.frame with \code{dataset_id}, \code{counts_harmonised}, \code{levels},
and \code{region}}

\item{dir}{Path to the data storage folder}

\item{min_n_occur}{Min occurrence of taxa to be displayed}

\item{max_taxa}{Number of taxa to plotted per one page}

\item{y_var}{The name of the variable to be used as Y-axis in the chart}

\item{date}{Date to be used to save the figures}
}
\description{
Plot pollen diagram for all records
}
