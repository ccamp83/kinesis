% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.r
\name{sortdataframe}
\alias{sortdataframe}
\title{Sort Data Frame}
\usage{
sortdataframe(x, decreasing = FALSE, by = 1, ...)
}
\arguments{
\item{x}{A data frame}

\item{decreasing}{Logical; should the sort be decreasing?}

\item{by}{Column names or numbers to sort by}

\item{...}{Additional arguments passed to order()}
}
\value{
Sorted data frame
}
\description{
Sorts a data frame by specified columns
}
