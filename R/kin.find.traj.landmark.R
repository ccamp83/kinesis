#' Find Landmark Frame in Trajectory Data
#' 
#' @description
#' Identifies a landmark frame in trajectory data based on a specified condition.
#' Returns the first frame of the longest consecutive sequence where the condition is met.
#' 
#' @param condition A character string containing an R expression that evaluates to a logical vector.
#'                  The expression should reference variables in the current environment.
#' 
#' @return An integer representing the landmark frame number, or NA if no landmark is found
#'         or if an error occurs during execution.
#' 
#' @details
#' The function works by:
#' 1. Evaluating the provided condition
#' 2. Finding consecutive sequences where the condition is TRUE
#' 3. Identifying the longest such sequence
#' 4. Returning the first frame of that sequence
#' 
#' @examples
#' \dontrun{
#' # Example with velocity data
#' velocity <- c(0.1, 0.2, 0.5, 0.7, 0.8, 0.9, 0.7, 0.4, 0.2)
#' landmark <- kin.find.traj.landmark("velocity > 0.5")
#' 
#' # Example with position data
#' position <- c(1, 2, 5, 8, 10, 12, 13, 14, 15)
#' landmark <- kin.find.traj.landmark("position > 10")
#' }
#' 
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
