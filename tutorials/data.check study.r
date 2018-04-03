dataset <- rtgData

data.check <- function(dataset, refreshRate = 85, time.unit = 1, ...)
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
    # assign refreshRate & time.unit to global environment for looping inside ddply (temporary)
    assign("refreshRate", refreshRate, envir = .GlobalEnv)
    assign("time.unit", time.unit, envir = .GlobalEnv)

    reqCols <- c("subjName","frameN", "time","frameT")
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
      if ("frameT" %in% missingCols) {
        # if time does not exists, create frameT
        if("time" %in% missingCols){
          dataset <- ddply(dataset, .(trialN), mutate,
                           frameT = time.unit / refreshRate)
        } else {
          # else frameT is delta time
          dataset <- ddply(dataset, .(trialN), mutate,
                           frameT = c(NA, diff(time)))
        }
        cat("frameT added.\n")
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
  }
