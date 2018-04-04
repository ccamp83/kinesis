# there are four columns that are expected
get("dataCols", kinesis_parameters)
# user can set them as well
kin.setDataCols(subjName = "subjID", frameN = "frame", time = "t", deltaTime = "frameT")
get("dataCols", kinesis_parameters)
# they can be reset by calling the setter function empty
kin.setDataCols()
get("dataCols", kinesis_parameters)

# user can have them under different names
data.check.test <- function(dataset, refreshRate = 85, time.unit = 1, ...)
{
  # dependencies
  require(plyr)

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

    # Fix trialN
    if (reqCols[5] %in% missingCols) {
      # if trialN is missing, it is assumed that there is one trial
      dataset$trialN <- 1
      names(dataset)[names(dataset) == "trialN"] <- reqCols[5]
      cat(reqCols[5], " added.\n", sep = "")
    }

    cat("\nDatabase fixed successfully.")
  }
  else {

    cat("\nDatabase looks good.")
  }

  return(dataset)
}

kin.setDataCols(subjName = "subjID", frameN = "frame", time = "t", deltaTime = "frametime")
testD <- data.check.test(rtgData_bad)
test

names(testD)
head(testD)
