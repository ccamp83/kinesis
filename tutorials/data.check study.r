dataset <- rtgData

data.check.test <- function(dataset, refreshRate = 85, time.unit = 1, ...)
{
  # cat("Please input the name of the column coding for the trials:")
  # trialCol <- readline()
  #
  # cat("Number of signals present in the dataset:")
  # n_sgn <- as.numeric(as.character(readline()))
  #
  # sgnCols <- NULL
  # for(sgn in 1:n_sgn)
  # {
  #   names_sgn_temp <- NULL
  #   for(coord in c("x","y","z")){
  #     cat("Name of column coding for ",coord," coord of signal #", sgn, ": ", sep="")
  #     names_sgn_temp <- c(names_sgn_temp, readline())
  #   }
  #   sgnCols[[sgn]] <- names_sgn_temp
  #   rm(names_sgn_temp)
  # }
  #
  # # cleanup
  # rm(sgn)
  # rm(coord)
  #
  # reqCols <- c("subjName", "frameN", "frameT", "time", "signalOccluded")
  # reqCols_check <- NULL
  # for(rCol in reqCols)
  # {
  #   cat("Is ",rCol," column present? YES: type its name / NO: type 0 (zero):", sep="")
  #   reqCols_check <- c(reqCols_check, readline())
  # }
  #

  # # create working dataset
  # wdataset <- dataset[names(dataset)%in%c(trialCol, unlist(sgnCols))]

  # assign refreshRate & time.unit to global environment for looping inside ddply (temporary)
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
