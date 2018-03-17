# kin.bwFilter ----
#' apply Butterworth filter
#' @param s: signal
#' @param  n: filter order (see ?butter for details)
#' @param  W: critical frequencies of the filter (see ?butter for details)
#' @param  type: filter type (see ?butter for details)
#' @export
kin.bwFilter <- function(s, n = 2, cutoff_freq = 10,type="low"){

  # extend the original vector at the start and at the end
  # to give the filter room to tune in
  s_temp <- c(rep(s[1], cutoff_freq*2), s, rep(s[length(s)], cutoff_freq*2))

  # set the filter
  bf <- butter(n, W = 1/cutoff_freq, type=type)
  # apply the filter
  b <- filtfilt(bf, s_temp)

  # trim initial and final segments introduced before
  b <- b[(cutoff_freq*2+1):(length(b) - (cutoff_freq*2))]
  return(b)
}
