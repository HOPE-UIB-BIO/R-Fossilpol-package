% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/proc_neo_get_chron_priority.R
\name{proc_neo_get_chron_priority}
\alias{proc_neo_get_chron_priority}
\title{Select chronology table based on the preferences}
\usage{
proc_neo_get_chron_priority(data_source, dataset_name, chron_order_table)
}
\arguments{
\item{data_source}{A list of lists with chronologies from Neotoma}

\item{dataset_name}{Name of the dataset}

\item{chron_order_table}{Data.frame with preferred order of chronology types}
}
\value{
Data.frame with the selected chronology
}
\description{
Look into all chronologies, select the best one
by the \code{chron_order}
}
\keyword{internal}
