# kin.bwFilter ----
#' apply Butterworth filter
#' @param s: signal
#' @param  n: filter order (see ?butter for details)
#' @param  W: critical frequencies of the filter (see ?butter for details)
#' @param  type: filter type (see ?butter for details)
#' @export
kin.bwFilter <- function(s, n = 2, cutoff_freq = 10,type="low"){
  bf <- butter(n, W = 1/cutoff_freq, type=type)
  b <- filtfilt(bf, s)
  return(b)
}
