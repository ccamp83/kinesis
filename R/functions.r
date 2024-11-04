#' Length of Unique Elements
#' 
#' Returns the number of unique elements in a vector
#' @param x A vector
#' @return Integer representing count of unique elements
#' @export
lengthunique <- function(x){
    length(unique(x))
}

#' Minimum Value with NA Handling
#' 
#' Wrapper for min() with na.rm=TRUE by default
#' @param x A numeric vector
#' @return Minimum value in the vector
#' @export
kin.min <- function(x) { min(x, na.rm=T) }

#' Maximum Value with NA Handling
#' 
#' Wrapper for max() with na.rm=TRUE by default
#' @param x A numeric vector
#' @return Maximum value in the vector
#' @export
kin.max <- function(x) { max(x, na.rm=T) }

#' Locate Local Maxima
#' 
#' Identifies points that are local maxima in a curve
#' @param x Numeric vector of x coordinates
#' @param y Numeric vector of y coordinates
#' @return Logical vector indicating which points are local maxima
#' @export
lmax.locate <- function(x, y)
{
    lmaxmin <- which(diff(sign(diff(y)))==-2)+1
    return(x %in% x[lmaxmin])
}

#' Mean with NA Handling
#' 
#' Wrapper for mean() with na.rm=TRUE by default
#' @param x A numeric vector
#' @return Mean value of the vector
#' @export
kin.mean <- function(x) {mean(x, na.rm=T)}

#' Sort Data Frame
#' 
#' Sorts a data frame by specified columns
#' @param x A data frame
#' @param decreasing Logical; should the sort be decreasing?
#' @param by Column names or numbers to sort by
#' @param ... Additional arguments passed to order()
#' @return Sorted data frame
#' @export
sortdataframe <- function(x, decreasing=FALSE, by=1, ... ){
    f <- function(...) order(...,decreasing=decreasing)
    i <- do.call(f,x[by])
    x[i,,drop=FALSE]
}

#' Standard Error
#' 
#' Calculates the standard error of a vector
#' @param x A numeric vector
#' @param na.rm Logical; should NA values be removed?
#' @return Standard error value
#' @export
kin.se <- function (x, na.rm = TRUE) {
    sqrt(var(x, na.rm = na.rm)/length(x[complete.cases(x)]))
}

#' Find Local Minima
#' 
#' Identifies indices of local minima in a vector
#' @param x A numeric vector
#' @return Vector of indices where local minima occur
#' @export
minima <- function(x) which(x - data.table::shift(x, 1) < 0  & x - data.table::shift(x, 1, type='lead') < 0)

#' Find Local Maxima
#' 
#' Identifies indices of local maxima in a vector
#' @param x A numeric vector
#' @return Vector of indices where local maxima occur
#' @export
maxima <- function(x) which(x - data.table::shift(x, 1) > 0  & x - data.table::shift(x, 1, type='lead') > 0)