% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/package.R
\docType{package}
\name{kinesis-package}
\alias{kinesis}
\alias{kinesis-package}
\title{Kinematic Data Processing Functions}
\description{
A collection of functions for processing and fixing kinematic datasets, particularly
focused on motion capture data with finger tracking.
}
\details{
Functions included in this collection:
\itemize{
\item \code{kin.trialN}: Ensures trial numbers start from 1
\item \code{kin.frameN}: Creates frame counters within trials
\item \code{kin.fingersOccluded}: Flags frames where fingers are occluded
\item \code{kin.framesOccluded}: Counts consecutive occluded frames
\item \code{kin.time}: Converts frame numbers to time based on refresh rate
\item \code{kin.globalTime}: Calculates global time across all trials
\item \code{kin.signal.missing}: Identifies missing or static signal frames
}
}
\keyword{internal}
