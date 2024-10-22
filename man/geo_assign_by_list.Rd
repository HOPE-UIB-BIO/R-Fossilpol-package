% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geo_assign_by_list.R
\name{geo_assign_by_list}
\alias{geo_assign_by_list}
\title{Assign series of geographical variables from shapefiles and/or tifs}
\usage{
geo_assign_by_list(data_source, dir, optional_info_to_assign)
}
\arguments{
\item{data_source}{Data.frame to add values to.}

\item{dir}{The directory with shapefile/tifs}

\item{optional_info_to_assign}{Data,frame with information about layers to be
assigned. See \code{geo_assign_value}}
}
\description{
Assign series of geographical variables from shapefiles and/or tifs
}
\seealso{
geo_assign_value
}
