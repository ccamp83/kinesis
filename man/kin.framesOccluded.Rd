% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions_dataset_fix.r
\name{kin.framesOccluded}
\alias{kin.framesOccluded}
\title{Count Occluded Frames}
\usage{
kin.framesOccluded(dataset)
}
\arguments{
\item{dataset}{A data frame containing kinematic data with fingersOccluded column}
}
\value{
A data frame with added framesOccluded counter
}
\description{
Creates an incremental counter of consecutive occluded frames within each trial.
}
