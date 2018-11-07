# kin.signal.analysis
#' @export
kin.signal.analysis <- function(signal, signal.name = "signal", start, end, maxFrames = 20, deltaTime,
                                f = T, t = T, s = T)
{
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
  signalSG <- as.data.frame(apply(signalRep, 2, kin.sgFilter, ts = deltaTime))
  names(signalSG) <- paste(signal.name, c("X","Y","Z"), "sg", sep = "")
  # translate
  M <- matrix(rep(start, nrow(signalSG)), ncol = 3, byrow = T) # replicate origin to create a dataset to subtract to the signal
  signalTra <- as.data.frame(signalSG - M) # translate
  names(signalTra) <- paste(signal.name, c("X","Y","Z"), "tra", sep = "")
  # rotate
  signalRot <- as.data.frame(kin.rotate.trajectory(signalTra, end - start, f = f, t = t, s = s))
  names(signalRot) <- paste(signal.name, c("X","Y","Z"), "rot", sep = "")
  # vel, acc
  signalVel <- as.data.frame(apply(signalRot, 2, kin.sgFilter, m=1, ts = deltaTime)) # 3D velocities
  names(signalVel) <- paste(signal.name, c("X","Y","Z"), "vel", sep = "")
  signalVel$vel_temp <- kin.sgFilter(sqrt(signalVel[,1]^2 + signalVel[,2]^2 + signalVel[,3]^2), ts = deltaTime)
  signalVel$acc_temp <- kin.sgFilter(kin.sgFilter(signalVel$vel_temp, m = 1, ts = deltaTime), p = 12, ts = deltaTime)
  names(signalVel)[4:5] <- paste(signal.name, c("Vel","Acc"), sep = "")
  # merge
  signal <- cbind(signalRot, signalVel)
  names(signal)[1:3] <- paste0(signal.name, c("X","Y","Z"))

  return(signal)
}
