#' Check the data file and provide fixes if available
#' @param dataset an object of the type data.frame
#' @param refreshRate the refresh rate used during the motion capture (in hertz)
#' @param time.unit the unit of measurement in which time is expressed in the 'time' column of the dataset given to the function. 1 = seconds, 10 = deciseconds, 100 = centiseconds, 1000 = milliseconds, ... Default to 1000
#' @examples
#' libraries()
#'
#' ### restoring missing columns
#'
#' head(rtgData_bad) # dataset provided by this package
#' rtgChecked <- data.check(rtgData_bad) # subjName is given without quotes. When asked to type the subject name, run the next line as is
#' test_subject
#' head(rtgChecked)
#'
#' ### time.unit
#'
#' rtgData <- data.check(rtgData) # dataset provided by this package
#' # time column in rtgData is in milliseconds. Note that data.check allows to specify different time units as well
#' head(rtgData)
#'
#' # instead, should the dataset have time in seconds
#' # the function will return frameT as a vector of NAs
#' data(rtgData) # reload dataset
#' rtgData$time <- rtgData$time / 1000 # change time to seconds
#' rtgData <- data.check(rtgData)
#' rtgData$frameT # always check that frameT looks good
#'
#' # use time.unit to fix it
#' data(rtgData) # reload dataset
#' rtgData$time <- rtgData$time / 1000 # change time to seconds
#' rtgData <- data.check(rtgData, time.unit = 1)
#' rtgData$frameT
#'
#' @export
data.check <- function(dataset, refreshRate = 85, time.unit = 1000, ...)
{
  indCols <- c("indexXraw","indexYraw","indexZraw")
  thuCols <- c("thumbXraw","thumbYraw","thumbZraw")

  if(!all(c(is.element("trialN", names(dataset)), is.element(indCols, names(dataset)))) ||
     !all(c(is.element("trialN", names(dataset)), is.element(thuCols, names(dataset)))))
  {
    stop("data.check expected a column called 'trialN' and at least one of the following triples: \n
         indexXraw, indexYraw, indexZraw \n         thumbXraw, thumbYraw, thumbZraw \n
         but could not find them in the dataset \n")
  } else
  {
    reqCols <- c("subjName", "frameN", "time", "fingersOccluded",
                 "framesOccluded", "globalTime","frameT")
    missingCols <- reqCols[reqCols %in% names(dataset) == F]
    if (length(missingCols) > 0) {
      cat("The following columns do not exist:\n")
      cat(missingCols, sep = ", ")
      cat("\n\nFixing...\n\n")
      if ("subjName" %in% missingCols) {
        cat("Please type subject name:\n")
        name <- readline()
        dataset$subjName <- sprintf("%s", name)
        cat("subjName added.\n")
      }
      if ("frameN" %in% missingCols) {
        dataset <- kin.frameN(dataset)
        cat("frameN added.\n")
      }
      if ("time" %in% missingCols) {
        dataset <- kin.time(dataset, refreshRate)
        cat("time added.\n")
      }
      if ("fingersOccluded" %in% missingCols) {
        dataset <- kin.fingersOccluded(dataset)
        cat("fingersOccluded added.\n")
      }
      if ("framesOccluded" %in% missingCols) {
        dataset <- kin.framesOccluded(dataset)
        cat("framesOccluded added.\n")
      }

      if(any(ddply(dataset, .(trialN), summarise,
                   any(as.logical(fingersOccluded)) - any(as.logical(framesOccluded)))[2] < 0)) {
        cat("fingersOccluded does not look right: fixing...\n")
        dataset <- kin.fingersOccluded(dataset)
      }

      if(any(ddply(dataset, .(trialN), summarise,
                   any(as.logical(fingersOccluded)) - any(as.logical(framesOccluded)))[2] > 0)) {
        cat("framesOccluded does not look right: fixing...\n")
        dataset <- kin.framesOccluded(dataset)
      }

      if ("globalTime" %in% missingCols) {
        isGlobalTimeMissing <- T
        dataset <- kin.globalTime(dataset)
        cat("globalTime added.\n")
      } else
      {
        isGlobalTimeMissing <- F
      }

      if ("frameT" %in% missingCols) {
        if(isGlobalTimeMissing)
        {
          dataset <- ddply(dataset, .(trialN), mutate,
                           frameT = c(NA, diff(time)))
          cat("frameT added.\n")
        } else
        {
          dataset <- ddply(dataset, .(trialN), mutate,
                           frameT = c(NA, diff(globalTime)))
          cat("frameT added.\n")
        }
        theorframeT <- round(time.unit/refreshRate, 2) # in millisecs
        # we establish that frameT can be accepted only if equal to the theoretical frameT or multiple of it (only greater cases)
        # we remove all values that are smaller than 90% of the theoretical frameT
        if(isGlobalTimeMissing)
        {
          dataset$frameT <- with(dataset, ifelse(frameT < theorframeT*.9, theorframeT, frameT))
          warning(paste("globalTime missing: incorrect frameT will be replaced by the theoretical frame time (", theorframeT, " msec)", sep=''), call. = F)
        } else
        {
          dataset$frameT <- with(dataset, ifelse(frameT < theorframeT*.9, NA, frameT))
        }

        if(sum(is.na(dataset$frameT)) == length(dataset$frameT))
          warning("frameT is a vector of NAs. Check that time and refresh rate have the same unit of measurement")
      }

      cat("\nDatabase fixed successfully.")
    }
    else {
      if(any(ddply(dataset, .(trialN), summarise,
                   any(as.logical(fingersOccluded)) - any(as.logical(framesOccluded)))[2] < 0)) {
        cat("checking 'fingersOccluded' column ...\n")
        dataset <- kin.fingersOccluded(dataset)
      }

      if(any(ddply(dataset, .(trialN), summarise,
                   any(as.logical(fingersOccluded)) - any(as.logical(framesOccluded)))[2] > 0)) {
        cat("checking 'framesOccluded' column ...\n")
        dataset <- kin.framesOccluded(dataset)
      }

      cat("\nDatabase looks good.")
    }
    return(dataset)
  }
}
