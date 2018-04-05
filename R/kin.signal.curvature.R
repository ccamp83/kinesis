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
