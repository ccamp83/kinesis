# kin.ssFilter
#' @export
kin.ssFilter <- function(x, y, x2 = NULL, spar = .5, deriv = 0)
{
  sm <- smooth.spline(x, y, spar = spar)
  if(is.null(x2))
  {
    x2 <- x
  }
  smoothed_signal <- predict(sm, x2, deriv = deriv)$y

  return(smoothed_signal)
}
