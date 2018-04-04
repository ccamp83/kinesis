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
data.check <- function(dataset, refreshRate = 85, time.unit = 1, ...)
{
  assign("refreshRate", refreshRate, envir = .GlobalEnv)
  assign("time.unit", time.unit, envir = .GlobalEnv)

  reqCols <- c("subjName","frameN", "time","deltaTime")
  missingCols <- reqCols[!reqCols %in% names(dataset)]
  if (length(missingCols) > 0) {
    cat("The following columns do not exist:\n")
    cat(missingCols, sep = ", ")
    cat("\n\nFixing...\n\n")

    # Fix subjName
    if ("subjName" %in% missingCols) {
      cat("Please type subject name:\n")
      dataset$subjName <- readline()
      cat("subjName added.\n")
    }

    # Fix frameN
    if ("frameN" %in% missingCols) {
      dataset <- kin.frameN(dataset)
      cat("frameN added.\n")
    }

    # Fix frameT
    if ("deltaTime" %in% missingCols) {
      # if time does not exists, create deltaTime
      if("time" %in% missingCols){
        dataset <- ddply(dataset, .(trialN), mutate,
                         frameT = time.unit / refreshRate)
      } else {
        # else deltaTime is delta time
        dataset <- ddply(dataset, .(trialN), mutate,
                         frameT = c(NA, diff(time)))
      }
      cat("deltaTime added.\n")
    }

    # Fix time
    if ("time" %in% missingCols) {
      dataset <- kin.time(dataset, refreshRate, time.unit)
      cat("time added.\n")
    }

    cat("\nDatabase fixed successfully.")
  }
  else {

    cat("\nDatabase looks good.")
  }

  # remove refreshRate & time.unit from global environment
  remove(refreshRate, envir = .GlobalEnv)
  remove(time.unit, envir = .GlobalEnv)

  return(dataset)
}

