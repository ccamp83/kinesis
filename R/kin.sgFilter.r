# kin.bwFilter ----
#' apply Savitzky-Golay filter
#' @description This function is a wrapper for signal::sgolayfilt with a modification to handle NAs
#' @export
kin.sgFilter <- function(x, p = 4, m = 0, ts = 1)
{
  output <- rep(NA, length(x))
  output[which(!is.na(x))] <- signal::sgolayfilt(na.omit(x), p = p, m = m, ts = ts)
  return(output)
}

