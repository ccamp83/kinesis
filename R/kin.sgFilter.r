#' Apply Savitzky-Golay Filter
#' 
#' @description
#' Applies a Savitzky-Golay smoothing filter to a numeric vector, with built-in handling for NA values.
#' This function wraps the sgolayfilt function from the 'signal' package.
#' 
#' @param x Numeric vector to be filtered
#' @param p Integer indicating the order of the polynomial used to fit the data (default: 4)
#' @param m Integer indicating the order of derivatives to calculate (default: 0)
#' @param ts Numeric value for the sampling interval (default: 1)
#' 
#' @return A numeric vector of the same length as the input, containing the filtered values
#' 
#' @examples
#' \dontrun{
#' data <- c(1, 2, NA, 4, 5, 6)
#' filtered_data <- kin.sgFilter(data)
#' }
#' 
#' @export
kin.sgFilter <- function(x, p = 4, m = 0, ts = 1)
{
  output <- rep(NA, length(x))
  output[which(!is.na(x))] <- signal::sgolayfilt(na.omit(x), p = p, m = m, ts = ts)
  return(output)
}
