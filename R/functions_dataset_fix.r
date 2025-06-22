#' Kinematic Data Processing Functions
#' 
#' @description
#' A collection of functions for processing and fixing kinematic datasets, particularly
#' focused on motion capture data with finger tracking.
#'
#' @details
#' Functions included in this collection:
#' \itemize{
#'   \item \code{kin.trialN}: Ensures trial numbers start from 1
#'   \item \code{kin.frameN}: Creates frame counters within trials
#'   \item \code{kin.fingersOccluded}: Flags frames where fingers are occluded
#'   \item \code{kin.framesOccluded}: Counts consecutive occluded frames
#'   \item \code{kin.time}: Converts frame numbers to time based on refresh rate
#'   \item \code{kin.globalTime}: Calculates global time across all trials
#'   \item \code{kin.signal.missing}: Identifies missing or static signal frames
#' }
#'
#' @name functions_dataset_fix
NULL

#' Fix Trial Numbers
#' 
#' @description
#' Ensures trial numbers start from 1 by adding 1 to all trial numbers if any are 0.
#' 
#' @param dataset A data frame containing kinematic data
#' @return A data frame with corrected trial numbers
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

#' Create Frame Counter
#' 
#' @description
#' Creates sequential frame numbers within each trial.
#' 
#' @param dataset A data frame containing kinematic data
#' @return A data frame with added frame numbers
#' @export
kin.frameN <- function(dataset)
{
  dataset <- eval(substitute(
    ddply(dataset, .(trialN), mutate, frameN=seq(1:length(trialN)))
    , list(trialN = as.name(kinesis_parameters$dataCols[5]))))
  names(dataset)[names(dataset) == "frameN"] <- kinesis_parameters$dataCols[2]
  return(dataset)
}

#' Flag Occluded Fingers
#' 
#' @description
#' Identifies frames where either finger marker is occluded based on position changes.
#' 
#' @param dataset A data frame containing kinematic data with raw finger positions
#' @return A data frame with occluded frames flagged and NA values for occluded positions
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

#' Count Occluded Frames
#' 
#' @description
#' Creates an incremental counter of consecutive occluded frames within each trial.
#' 
#' @param dataset A data frame containing kinematic data with fingersOccluded column
#' @return A data frame with added framesOccluded counter
#' @export
kin.framesOccluded <- function(dataset)
{
  dataset <- ddply(dataset, .(trialN), mutate, framesOccluded = fingersOccluded * unlist(lapply(rle(fingersOccluded)$lengths, seq_len)))
  return(dataset)
}

#' Convert Frames to Time
#' 
#' @description
#' Converts frame numbers to time based on screen refresh rate.
#' 
#' @param dataset A data frame containing kinematic data
#' @param refreshRate Nominal refresh rate of the screen in Hz (default: 85)
#' @param time.unit Time unit conversion factor (default: 1)
#' @return A data frame with added time column
#' @export
kin.time <- function(dataset, refreshRate = 85, time.unit = 1)
{
  dataset <- eval(substitute(
    ddply(dataset, .(trialN), mutate, time = frameN * kinesis_parameters$time.unit / kinesis_parameters$refreshRate)
    , list(trialN = as.name(kinesis_parameters$dataCols[5]),
           frameN = as.name(kinesis_parameters$dataCols[2]))))
  names(dataset)[names(dataset) == "time"] <- kinesis_parameters$dataCols[3]
  return(dataset)
}

#' Calculate Global Time
#' 
#' @description
#' Calculates global time across all trials based on median frame duration.
#' 
#' @param dataset A data frame containing kinematic data with time column
#' @return A data frame with added globalTime column
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

#' Identify Missing Signals
#' 
#' @description
#' Identifies missing or static signals in kinematic data.
#' 
#' @param x A numeric vector containing signal data
#' @param criterion Values to be considered as missing (default: NULL)
#' @param delete.static.positions If TRUE, marks unchanging positions as missing (default: FALSE)
#' @return A vector with missing values marked as NA
#' @export
kin.signal.missing <- function(x, criterion=NULL, delete.static.positions = F)
{
  v <- x

  if(delete.static.positions){
    v <- c(v[1], ifelse(abs(c(NA, diff(v))) < .000001, NA, v)[-1])
  }
  v[v%in%criterion] <- NA
  cat(sum(is.na(v)), " missing frames detected.\n", sep = "")
  return(v)
}
