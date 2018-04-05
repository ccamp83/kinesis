#' @export
kin.signal.arclength <- function(data)
{
  Xdiff <- kin.sgFilter(data[,1], m=1)
  Ydiff <- kin.sgFilter(data[,2], m=1)
  Zdiff <- kin.sgFilter(data[,3], m=1)
  arclength <- sqrt(Xdiff^2 + Ydiff^2 + Zdiff^2)

  return(cumsum(arclength))
}
