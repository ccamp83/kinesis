####################################################################################	1.
#' @export
lengthunique <- function(x){
	length(unique(x))
}

####################################################################################  2.

# min and max functions with na.rm = TRUE by default (for usage in ddply and graphs)
#' @export
kin.min <- function(x) { min(x, na.rm=T) }
#' @export
kin.max <- function(x) { max(x, na.rm=T) }

####################################################################################  3.
# function to locate the local maxima (used in kin.extract)
#' @export
lmax.locate <- function(x, y)
{
  lmaxmin <- which(diff(sign(diff(y)))==-2)+1
  return(x %in% x[lmaxmin])
}

####################################################################################  4.
# mean without NAs by default
#' @export
kin.mean <- function(x) {mean(x, na.rm=T)}

####################################################################################  5.
# function to sort a data.frame by column(s) name or number
#' @export
sortdataframe <- function(x, decreasing=FALSE, by=1, ... ){
  f <- function(...) order(...,decreasing=decreasing)
  i <- do.call(f,x[by])
  x[i,,drop=FALSE]
}

####################################################################################  17.
# standard error (from package sciplot)
#' @export
se <- function (x, na.rm = TRUE) {sqrt(var(x, na.rm = na.rm)/length(x[complete.cases(x)]))}
