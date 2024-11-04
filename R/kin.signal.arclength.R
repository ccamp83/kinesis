#' Calculate Cumulative Arc Length of 3D Motion Data
#' 
#' This function calculates the cumulative arc length of a 3D motion trajectory
#' using Savitzky-Golay filtered derivatives.
#' 
#' @param data A matrix or data frame with 3 columns representing X, Y, and Z coordinates
#' 
#' @return A numeric vector containing the cumulative arc length at each point
#' 
#' @details 
#' The function computes the arc length by:
#' 1. Calculating derivatives using Savitzky-Golay filtering for each dimension
#' 2. Computing the Euclidean distance between consecutive points
#' 3. Calculating the cumulative sum of these distances
#' 
#' @examples
#' xyz_data <- matrix(rnorm(300), ncol=3)
#' arc_length <- kin.signal.arclength(xyz_data)
#' 
#' @export
kin.signal.arclength <- function(data)
{
  Xdiff <- kin.sgFilter(data[,1], m=1)
  Ydiff <- kin.sgFilter(data[,2], m=1)
  Zdiff <- kin.sgFilter(data[,3], m=1)
  arclength <- sqrt(Xdiff^2 + Ydiff^2 + Zdiff^2)

  return(cumsum(arclength))
}
