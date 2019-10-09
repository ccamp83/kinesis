# kin.signal.analysis
#' @export
kin.signal.analysis <- function(signal, signal.name = "signal", start, end, maxFrames = 20,
                                rotate = T, f = T, t = T, s = T)
{
  tryCatch(
    {
      # check that time is present
      hasTimeCol <- "time" %in% names(signal)
      if(!hasTimeCol)
      {
        message("Time column not present.\n")
        stop()
      } else
      {
        timeCol <- signal$time
      }

      # make backup
      signal.backup <- signal

      # check what columns match the signal name
      signalCols <- names(signal)[grepl(signal.name, names(signal))]
      # if one column is missing, set it to zero
      if(length(signalCols) == 2)
      {
        # expected columns
        expectedCols <- paste0(signal.name, c("X","Y","Z"))
        for(col in expectedCols)
        {
          isColFound <- length(names(signal)[grepl(col, names(signal))])>0
          if(!isColFound)
          {
            message(paste0("The following column is missing: ", col, ". Will be set to zero.\n"))
            signal <- cbind(signal, 0)
            names(signal)[length(names(signal))] <- col
          }
        }
      }
      # reorder columns
      # grab the columns with the signal name
      signal.nameCols <- names(signal)[grepl(signal.name, names(signal))]
      # reorder
      signal.nameCols <- sort(signal.nameCols)

      # all other columns
      otherCols <- names(signal)[!grepl(signal.name, names(signal))]

      # retain only signal columns
      signal <- signal[signal.nameCols]

      # name of signal
      names(signal) <- paste0(signal.name, c("X","Y","Z"), "raw")

      # repair
      signalRep <- as.data.frame(apply(signal, 2, kin.signal.repair, maxFrames = maxFrames))
      names(signalRep) <- paste(signal.name, c("X","Y","Z"), "rep", sep = "")
      # filter
      signalSS <- as.data.frame(apply(signalRep, 2, kin.ssFilter, x = timeCol))
      names(signalSS) <- paste(signal.name, c("X","Y","Z"), "ss", sep = "")
      # translate
      M <- matrix(rep(start, nrow(signalSS)), ncol = 3, byrow = T) # replicate origin to create a dataset to subtract to the signal
      signalTra <- as.data.frame(signalSS - M) # translate
      names(signalTra) <- paste(signal.name, c("X","Y","Z"), "tra", sep = "")
      # rotate
      if(rotate)
      {
        signalRot <- as.data.frame(kin.rotate.trajectory(signalTra, end - start, f = f, t = t, s = s))
        names(signalRot) <- paste(signal.name, c("X","Y","Z"), "rot", sep = "")
      } else
        signalRot <- signalTra
      # vel, acc
      signalVel <- as.data.frame(apply(signalRot, 2, kin.ssFilter, x = timeCol, deriv = 1)) # 3D velocities
      names(signalVel) <- paste(signal.name, c("X","Y","Z"), "vel", sep = "")
      signalVel$vel_temp <- sqrt(signalVel[,1]^2 + signalVel[,2]^2 + signalVel[,3]^2)
      signalVel$acc_temp <- kin.ssFilter(timeCol, signalVel$vel_temp, deriv = 1, spar = 0)
      names(signalVel)[4:5] <- paste(signal.name, c("Vel","Acc"), sep = "")

      # merge
      signal <- cbind(signalRot, signalVel, time = timeCol)
      names(signal)[1:3] <- paste0(signal.name, c("X","Y","Z"))

      return(signal)
    },
    error = function(e) return(NA)
  )
}
