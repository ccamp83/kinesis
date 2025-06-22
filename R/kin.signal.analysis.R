#' @title Analyze trajectory data
#' @description Repair (check for missing frames), smooth, derive, rotate trajectory data
#' @param signal dataset to be analyzed. It must contain a time column and three columns for the x, y, z positions of the object (signal) that will be analyzed. See details for the expected names of these columns
#' @param signal.name character indicating the name of the object (signal)
#' @param start 3-elements-long vector indicating the x, y, z coordinates of the start position of the trajectory. Default to c(0,0,0), no translation is applied.
#' @param end 3-elements-long vector indicating the x, y, z coordinates of the end position of the trajectory. Default to c(0,0,0), no rotation is applied.
#' @param maxFrames integer number of missing frames to be substituted with linear interpolation
#' @param filter filter used for smoothing: "ss" (smoothing spline), "bw" (low-pass Butterworth - default), "sg" (Savitzky-Golay)
#' @param splinepar parameter for smoothing spline (requires filter = "ss". See ?smooth.spline for details)
#' @param bw.cutoff cutoff frequency for the Butterworth filter (requires filter = "bw". See ?kin.bwFilter)
#' @param rotateF logical: should the trajectory be aligned to the frontoparallel plane?
#' @param rotateT logical: should the trajectory be aligned to the transversal plane?
#' @param rotateS logical: should the trajectory be aligned to the sagittal plane?
#' @details
#' The names of the input dataset (signal parameter) must adhere to the following standards:
#' i) the name of the time column must be identical to what specified through kin.setDataCols()
#' ii) the other three columns names must be string of the type nameXraw, nameYraw, nameZraw (where "name" must match the signal.name parameter)
#' @export
kin.signal.analysis <- function(signal, signal.name = "signal", start = c(0,0,0), end = c(0,0,0), maxFrames = 20,
                                filter = "bw",
                                splinepar = 5e-2,
                                bw.cutoff = 10,
                                rotateF = T, rotateT = T, rotateS = T, ...)
{
  tryCatch(
    {
      # check that time column is present
      timeColName <- .kinesis_env$dataCols[3]
      hasTimeCol <- timeColName %in% names(signal)
      if(!hasTimeCol)
      {
        message("Time column not present.\n")
        stop()
      } else
      {
        timeCol <- signal[,timeColName]
      }

      # make backup
      signal.backup <- signal

      # check which columns match the signal name
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
      signalFil <- NULL
      if(filter == "ss")
      {
        signalFil <- as.data.frame(apply(signalRep, 2, kin.ssFilter, x = timeCol, spar = splinepar))
        names(signalFil) <- paste(signal.name, c("X","Y","Z"), "ss", sep = "")
      } else if (filter == "bw")
      {
        signalFil <- as.data.frame(apply(signalRep, 2, kin.bwFilter, cutoff_freq = bw.cutoff))
        names(signalFil) <- paste(signal.name, c("X","Y","Z"), "bw", sep = "")
      } else if (filter == "sg")
      {
        signalFil <- as.data.frame(apply(signalRep, 2, kin.sgFilter, ts = timeCol))
        names(signalFil) <- paste(signal.name, c("X","Y","Z"), "sg", sep = "")
      }
      # translate
      M <- matrix(rep(start, nrow(signalFil)), ncol = 3, byrow = T) # replicate origin to create a dataset to subtract to the signal
      signalTra <- as.data.frame(signalFil - M) # translate
      names(signalTra) <- paste(signal.name, c("X","Y","Z"), "tra", sep = "")
      # rotate
      signalRot <- as.data.frame(kin.rotate.trajectory(signalTra, end - start, f = rotateF, t = rotateT, s = rotateS))
      names(signalRot) <- paste(signal.name, c("X","Y","Z"), "rot", sep = "")
      # vel, acc
      signalVel <- as.data.frame(apply(signalRot, 2, kin.ssFilter, x = timeCol, spar = splinepar, deriv = 1)) # 3D velocities
      names(signalVel) <- paste(signal.name, c("X","Y","Z"), "vel", sep = "")
      signalVel$vel_temp <- sqrt(signalVel[,1]^2 + signalVel[,2]^2 + signalVel[,3]^2)
      signalVel$acc_temp <- kin.ssFilter(timeCol, signalVel$vel_temp, deriv = 1, spar = 0)
      names(signalVel)[4:5] <- paste(signal.name, c("Vel","Acc"), sep = "")

      # merge
      signal <- cbind(signalRot, signalVel, time = timeCol)
      names(signal)[1:3] <- paste0(signal.name, c("X","Y","Z"))

      return(signal)
    },
    error = function(e) {
      message(paste("Error:", e))
      return(NA)
      }
  )
}
