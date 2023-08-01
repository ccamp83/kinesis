# kin.find.traj.landmark
#' @export
kin.find.traj.landmark <- function(condition)
{
  tryCatch(
    {
      eval.cond <- eval(parse(text = condition))
      x.flag.vel <- ifelse(eval.cond, 1, 0)
      # cumulative count of 0s
      x.flag.vel.count <- x.flag.vel * unlist(lapply(rle(x.flag.vel)$lengths, seq_len))
      # gather all missing data in groups
      landmark.check <- data.frame(
        frames = which(eval.cond)
        ,
        count = x.flag.vel.count[x.flag.vel.count!=0]
      )
      landmark.check$group <- with(landmark.check, c(0, cumsum(diff(count) != 1)))
      # group with the longest sequence
      landmarkGroup <- subset(landmark.check, count == max(landmark.check$count))$group
      # movement landmark is the first frame of the group with the longest sequence
      landmarkFrame <- subset(landmark.check, group == landmarkGroup & count == 1)$frames

      return(landmarkFrame)
    },
    error = function(e) return(NA),
    warning = function(w) return(NA)
  )
}
