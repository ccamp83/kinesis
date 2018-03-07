dataset <- rtgData

data.check <- function(dataset, refreshRate = 85, time.unit = 1000, ...)
{
  cat("Please input the name of the column coding for the trials:")
  trialCol <- readline()

  cat("Number of signals present in the dataset:")
  n_sgn <- as.numeric(as.character(readline()))

  sgnCols <- NULL
  for(sgn in 1:n_sgn)
  {
    names_sgn_temp <- NULL
    for(coord in c("x","y","z")){
      cat("Name of column coding for ",coord," coord of signal #", sgn, ": ", sep="")
      names_sgn_temp <- c(names_sgn_temp, readline())
    }
    sgnCols[[sgn]] <- names_sgn_temp
    rm(names_sgn_temp)
  }

  # cleanup
  rm(sgn)
  rm(coord)

  reqCols <- c("subjName", "frameN", "frameT", "time", "signalOccluded")
  reqCols_check <- NULL
  for(rCol in reqCols)
  {
    cat("Is ",rCol," column present? YES: type its name / NO: type 0 (zero):", sep="")
    reqCols_check <- c(reqCols_check, readline())
  }


  # create working dataset
  wdataset <- dataset[names(dataset)%in%c(trialCol, unlist(sgnCols))]

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
        cat("Please type subject name:")
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
