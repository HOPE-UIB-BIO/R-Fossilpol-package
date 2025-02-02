% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chron_predict_ages.R
\name{chron_predict_ages}
\alias{chron_predict_ages}
\title{Predict ages for each level based on chronology age-depth model}
\usage{
chron_predict_ages(data_source, sample_data, target_age_quantile = 0.95)
}
\arguments{
\item{data_source}{Bchron output}

\item{sample_data}{Data.frame with the \code{sample_id} and \code{depth} of levels}

\item{target_age_quantile}{Quantile to calculate upper and lower quantile
between. Must be between 0 and 1.}
}
\value{
List of two: ages and age_position
}
\description{
Predict ages for each level based on Bchron including the
95th quantile. In addition, produce the age uncertainties.
}
