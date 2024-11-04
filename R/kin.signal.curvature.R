#' Calculate the Curvature of a 3D Trajectory
#' 
#' @description
#' Computes the curvature of a 3D trajectory using velocity and acceleration components
#' derived from position data using Savitzky-Golay filtering.
#' 
#' @param data A matrix or data frame with 3 columns representing X, Y, and Z coordinates
#' @param deltaTime Numeric value representing the time interval between samples
#' @param normalize Logical value indicating whether to normalize the curvature values 
#' to a range of [0,1] (default: TRUE)
#' 
#' @return A numeric vector containing the calculated curvature values. If normalize=TRUE,
#' values are scaled between 0 and 1.
#' 
#' @details
#' The function calculates curvature using the formula:
#' k = |x'y'' - y'x''| / (x'^2 + y'^2)^(3/2)
#' where x' and y' are velocities, and x'' and y'' are accelerations.
#' 
#' Velocities and accelerations are computed using Savitzky-Golay filtering via the
#' kin.sgFilter function.
#' 
#' @examples
#' \dontrun{
#' # Create sample 3D trajectory data
#' t <- seq(0, 2*pi, length.out=100)
#' data <- cbind(sin(t), cos(t), t)
#' 
#' # Calculate curvature with normalized output
#' curv <- kin.signal.curvature(data, deltaTime=0.1, normalize=TRUE)
#' }
#' 
#' @export
kin.signal.curvature <- function(data, deltaTime, normalize = T)
{
  Xvel <- kin.sgFilter(data[,1], m=1, ts = deltaTime)
  Yvel <- kin.sgFilter(data[,2], m=1, ts = deltaTime)
  Zvel <- kin.sgFilter(data[,3], m=1, ts = deltaTime)
  Xacc <- kin.sgFilter(data[,1], m=2, ts = deltaTime)
  Yacc <- kin.sgFilter(data[,2], m=2, ts = deltaTime)
  Zacc <- kin.sgFilter(data[,3], m=2, ts = deltaTime)

  k <- abs(Xvel * Yacc - Yvel * Xacc) / ((Xvel^2 + Yvel^2)^(3/2))

  if(normalize)
  {
    k <- (k - min(k, na.rm=T)) / (max(k, na.rm=T) - min(k, na.rm=T))
  }
  return(k)
}
