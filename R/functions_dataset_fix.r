# functions to fix dataset

# kin.trialN: trialN from 1 to N ----
#' @export
kin.trialN <- function(dataset)
{
  if("trialN"%in%names(dataset))
  {
    if(0 %in% unique(dataset$trialN))
    {
      dataset$trialN <- dataset$trialN+1
      print("TrialN fixed. Trials sequence now starts from 1.")
    }else
    {
      print("Trials sequence already starts from 1.")
    }
  }else
  {
    print("No 'trialN' column found in dataset.")
  }
  return(dataset)
}

# kin.frameN: frames counter ----
#' @export
kin.frameN <- function(dataset)
{
  dataset <- ddply(dataset, .(trialN), mutate, frameN=seq(1:length(trialN)))
  return(dataset)
}

# kin.fingersOccluded: flag the frames where either of the two fingers is invisible ----
#' @export
kin.fingersOccluded <- function(dataset)
{
  datatemp <- ddply(dataset, .(trialN), mutate,
                    indexVisibility=c(-999,abs(diff(indexXraw))),
                    thumbVisibility=c(-999,abs(diff(thumbXraw))),
                    indexXraw = ifelse(indexVisibility<0.000001,NA,indexXraw),
                    indexYraw = ifelse(indexVisibility<0.000001,NA,indexYraw),
                    indexZraw = ifelse(indexVisibility<0.000001,NA,indexZraw),
                    thumbXraw = ifelse(thumbVisibility<0.000001,NA,thumbXraw),
                    thumbYraw = ifelse(thumbVisibility<0.000001,NA,thumbYraw),
                    thumbZraw = ifelse(thumbVisibility<0.000001,NA,thumbZraw),
                    fingersOccluded=ifelse(indexVisibility*thumbVisibility<0.000001,1,0) # if the increment is exactly zero, two frames have exactly the same coordinate, which means that the marker was not recorded
  )
  # update fingers raw coords
  dataset$indexXraw <- datatemp$indexXraw
  dataset$indexYraw <- datatemp$indexYraw
  dataset$indexZraw <- datatemp$indexZraw
  dataset$thumbXraw <- datatemp$thumbXraw
  dataset$thumbYraw <- datatemp$thumbYraw
  dataset$thumbZraw <- datatemp$thumbZraw
  dataset$fingersOccluded <- ifelse(is.na(datatemp$fingersOccluded), 1,  datatemp$fingersOccluded) # drop index- and thumbVisibility and include only fingersOccluded
  return(dataset)
}

# kin.framesOccluded: incremental counter of occluded frames within each trial ----
#' @export
kin.framesOccluded <- function(dataset)
{
  dataset <- ddply(dataset, .(trialN), mutate, framesOccluded = fingersOccluded * unlist(lapply(rle(fingersOccluded)$lengths, seq_len)))
  return(dataset)
}

# kin.time: multiplies each frame by a constant equal to the (nominal) refresh rate of the screen (Warning: this is just an artificial fix, actual refresh rate is variable - always make sure that actual time steps are in the original output file) ----
#' @export
kin.time <- function(dataset, refreshRate = 85, time.unit = 1)
{
  dataset <- ddply(dataset, .(trialN), mutate, time = frameN * time.unit / refreshRate) # in milliseconds
  return(dataset)
}

# kin.globalTime ----
#' @export
kin.globalTime <- function(dataset)
{
  millisecPerFrame = median(diff(dataset$time))
  # assign millisecPerFrame to global environment for looping (temporarily)
  assign("millisecPerFrame", millisecPerFrame, envir = .GlobalEnv)
  dataset <- ddply(dataset, .(trialN), mutate, globalTime = frameN*millisecPerFrame)
  # remove millisecPerFrame from global environment
  remove(millisecPerFrame, envir = .GlobalEnv)

  return(dataset)
}
