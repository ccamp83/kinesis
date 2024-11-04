#' Smooth Signal Using Smoothing Splines
#' 
#' Applies smoothing spline filtering to a signal using \code{smooth.spline}.
#' 
#' @param x Numeric vector containing the x-coordinates of the input signal
#' @param y Numeric vector containing the y-coordinates of the input signal
#' @param x2 Optional numeric vector of x-coordinates where predictions should be made. 
#'          If NULL (default), predictions are made at the input x-coordinates
#' @param spar Smoothing parameter, typically in (0,1]. Default is 0.05
#' @param deriv Integer specifying the order of derivative wanted. Default is 0
#' 
#' @return A numeric vector containing the smoothed signal values
#' 
#' @details 
#' This function applies smoothing spline filtering to a signal using base R's 
#' \code{smooth.spline} function. The smoothing parameter \code{spar} controls 
#' the trade-off between smoothness and goodness of fit, with larger values 
#' producing smoother results.
#' 
#' @examples
#' x <- seq(0, 10, length.out = 100)
#' y <- sin(x) + rnorm(100, sd = 0.1)
#' smoothed <- kin.ssFilter(x, y, spar = 0.5)
#' 
#' @export
kin.ssFilter <- function(x, y, x2 = NULL, spar = 5e-2, deriv = 0)
{
  sm <- smooth.spline(x, y, spar = spar)
  if(is.null(x2))
  {
    x2 <- x
  }
  smoothed_signal <- predict(sm, x2, deriv = deriv)$y

  return(smoothed_signal)
}
