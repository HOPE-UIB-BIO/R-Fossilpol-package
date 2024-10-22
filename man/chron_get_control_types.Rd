% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chron_get_control_types.R
\name{chron_get_control_types}
\alias{chron_get_control_types}
\title{Get the selected chronology control control point types}
\usage{
chron_get_control_types(data_source, dir)
}
\arguments{
\item{data_source}{Data.frame with \code{chron_control}}

\item{dir}{Path to the data storage folder}
}
\value{
Named list with \code{chroncontrol_included_types} and
\code{radiocarbon_control_types}
}
\description{
Use \code{stopcheck_table} to get the valid chronology control point
types.
}
