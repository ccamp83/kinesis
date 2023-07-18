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
data.check <- function(dataset, refreshRate = 85, time.unit = 1, check.only = F, ...)
{
  # assign refreshRate & time.unit to global environment for looping inside ddply (temporary)
  assign("refreshRate", refreshRate, envir = kinesis_parameters)
  assign("time.unit", time.unit, envir = kinesis_parameters)

  # get required columns
  reqCols <- kinesis_parameters$dataCols
  # look for missing columns
  missingCols <- reqCols[!reqCols %in% names(dataset)]

  #### Fix missing columns (if any)
  if (length(missingCols) > 0) {
    cat("The following columns do not exist:\n")
    cat(missingCols, sep = ", ")

    if(!check.only)
    {
      cat("\n\nFixing...\n\n")

      # Fix subjName
      if (reqCols[1] %in% missingCols) {
        cat("Please type subject name:\n")
        dataset$subjName <- readline()
        names(dataset)[names(dataset) == "subjName"] <- reqCols[1]
        cat(reqCols[1], " added.\n", sep = "")
      }

      # Fix frameN
      if (reqCols[2] %in% missingCols) {
        dataset <- kin.frameN(dataset)
        cat(reqCols[2], " added.\n", sep = "")
      }

      # Fix time
      if (reqCols[3] %in% missingCols) {
        dataset <- kin.time(dataset, kinesis_parameters$refreshRate, kinesis_parameters$time.unit)
        cat(reqCols[3], " added.\n", sep = "")
      }

      # Fix trialN
      if (reqCols[5] %in% missingCols) {
        # if trialN is missing, it is assumed that there is one trial
        dataset$trialN <- 1
        names(dataset)[names(dataset) == "trialN"] <- reqCols[5]
        cat(reqCols[5], " added.\n", sep = "")
      }

      # Fix deltaTime
      if (reqCols[4] %in% missingCols) {
        # if time does not exists, create deltaTime
        if(reqCols[3] %in% missingCols){
          dataset <- eval(substitute(
            ddply(dataset, .(trialN), mutate,
                  frameT = kinesis_parameters$time.unit / kinesis_parameters$refreshRate)
            , list(trialN = as.name(kinesis_parameters$dataCols[5]))))
        } else {
          # else deltaTime is delta time
          dataset <- eval(substitute(
            ddply(dataset, .(trialN), mutate,
                  frameT = c(NA, diff(time)))
            , list(trialN = as.name(kinesis_parameters$dataCols[5]),
                   time = as.name(kinesis_parameters$dataCols[3]))))
        }
        names(dataset)[names(dataset) == "frameT"] <- reqCols[4]
        cat(reqCols[4], " added.\n", sep = "")
      }

      cat("\nDatabase fixed successfully.")
    } else
    {
      opt <- options(show.error.messages=FALSE)
      on.exit(options(opt))
      stop()
    }
  }
  else {

    cat("\nDatabase looks good.")
  }

  return(dataset)
}

