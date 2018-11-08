# kin.onset.time
#' @export
kin.find.onsetTime <- function(x, onset_threshold = 0)
{
  x.flag.vel <- ifelse(x > onset_threshold, 1, 0)
  # cumulative count of 0s
  x.flag.vel.count <- x.flag.vel * unlist(lapply(rle(x.flag.vel)$lengths, seq_len))
  # gather all missing data in groups
  onset.check <- data.frame(
    frames = which(x > onset_threshold)
    ,
    count = x.flag.vel.count[x.flag.vel.count!=0]
  )
  onset.check$group <- with(onset.check, c(0, cumsum(diff(count) != 1)))
  # group with the longest sequence
  onsetGroup <- subset(onset.check, count == max(onset.check$count))$group
  # movement onset is the first frame of the group with the longest sequence
  onsetFrame <- subset(onset.check, group == onsetGroup & count == 1)$frames

  return(onsetFrame)
}
