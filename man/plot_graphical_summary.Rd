% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_graphical_summary.R
\name{plot_graphical_summary}
\alias{plot_graphical_summary}
\title{Plot graphical summary of dataset compilation}
\usage{
plot_graphical_summary(
  data_source,
  grouping_variable = NULL,
  drop_na = FALSE,
  col_default = "gray50",
  col_light = "gray90",
  col_dark = "gray30",
  point_size = 3,
  line_width = 1,
  line_size = 0.1,
  text_size = 16,
  legend_position = c("none", "bottom", "top", "left", "right"),
  color_palette = NULL
)
}
\arguments{
\item{data_source}{Data.frame to containing the dataset compilation}

\item{grouping_variable}{Optional name of variable to use to group and color data}

\item{drop_na}{Logical. If \code{grouping_variable} selected, shoudl NA values be
ommited?}

\item{col_default}{Character. Name of a default color}

\item{col_light}{Character. Name of a lighter color}

\item{col_dark}{Character. Name of a darker color}

\item{point_size}{Numeric. Size of a point}

\item{line_width}{Numeric. Line width}

\item{line_size}{Numeric. Width of a line for axis}

\item{text_size}{Numeric. Text size for axis}

\item{legend_position}{Character. If legend present, where it should be
placed? Default is "none", which will not display legend.
See \code{legend.position} in \code{\link[ggplot2:theme]{ggplot2::theme()}}}

\item{color_palette}{A vector with cuntom color palette. Only used when
\code{grouping_variable} is selected}
}
\description{
Create three plots of graphical summary of data compilation.
When \code{grouping_variable} is specified, the selected variable will be used for
grouping and coloring.
Types of graph:
\itemize{
\item Map - map of geographical location with each record represented as a point
\item Count of data - A lollipop plot with a number of records in each group
\item age length - age limits of records, with ech record represented by a
single line
}
}
\seealso{
\code{\link[=plot_map_of_data]{plot_map_of_data()}}, \code{\link[=plot_age_lenght_of_data]{plot_age_lenght_of_data()}},
\code{\link[=plot_count_of_data]{plot_count_of_data()}},
}
