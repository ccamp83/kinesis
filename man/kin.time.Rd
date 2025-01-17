% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions_dataset_fix.r
\name{kin.time}
\alias{kin.time}
\title{Convert Frames to Time}
\usage{
kin.time(dataset, refreshRate = 85, time.unit = 1)
}
\arguments{
\item{dataset}{A data frame containing kinematic data}

\item{refreshRate}{Nominal refresh rate of the screen in Hz (default: 85)}

\item{time.unit}{Time unit conversion factor (default: 1)}
}
\value{
A data frame with added time column
}
\description{
Converts frame numbers to time based on screen refresh rate.
}
