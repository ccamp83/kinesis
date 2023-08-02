#' @export
kin.grasp.analysis <- function(data, signals, ...)
{
  # check that deltaTime column is present
  dTimeColName <- kinesis_parameters$dataCols[4]
  hasdTimeCol <- dTimeColName %in% names(signal)
  if(!hasdTimeCol)
  {
    message("deltaTime column not present.\n")
    stop()
  } else
  {
    dTimeCol <- signal[,dTimeColName]
  }

  signalsCols <- paste0(signals, rep(c("X","Y","Z"), length(signals)))
  temp <- data[signalsCols]

  # Grip Aperture
  # GA is the 3D euclidean distance between the index and the thumb
  GA <- eval(substitute(

    with(temp, sqrt((X1 - X2)^2 + (Y1 - Y2)^2 + (Z1 - Z2)^2))

    ,list(X1 = as.name(signalsCols[1]),
          Y1 = as.name(signalsCols[2]),
          Z1 = as.name(signalsCols[3]),
          X2 = as.name(signalsCols[4]),
          Y2 = as.name(signalsCols[5]),
          Z2 = as.name(signalsCols[6])
    )
  ))

  # 3D Grip Position (raw)
  # GP is the midway position between index and thumb
  GPX <- (temp[,1] + temp[,4])/2
  GPY <- (temp[,2] + temp[,5])/2
  GPZ <- (temp[,3] + temp[,6])/2

  # Vel, Acc
  GPVel <- as.data.frame(apply(data.frame(GPX, GPY, GPZ), 2, kin.sgFilter, m=1, ts = dTimeCol)) # 3D velocities
  names(GPVel) <- paste0(names(GPVel), "vel")
  GPVel$GPVel <- kin.sgFilter(sqrt(GPVel[,1]^2 + GPVel[,2]^2 + GPVel[,3]^2), ts = dTimeCol)
  GPVel$GPAcc <- kin.sgFilter(kin.sgFilter(GPVel$GPVel, m = 1, ts = dTimeCol), ts = dTimeCol)

  # 3D Grip Orientation (raw)
  # GOF is the angle between the parallel projection of GA and the x axis on the Frontal (coronal) plane
  GOF <- atan2( (temp[,2]-temp[,5]), (temp[,1]-temp[,4]) )
  GOF[GOF<0] <- GOF[GOF<0]+2*pi
  # GOT is the angle between the parallel projection of GA and the z axis on the Transverse plane
  GOT <- atan2( (temp[,3]-temp[,6]), (temp[,1]-temp[,4]) )
  GOT[GOT<0] <- GOT[GOT<0]+2*pi
  # GOS is the angle between the parallel projection of GA and the z axis on the Sagittal plane
  GOS <- atan2( (temp[,2]-temp[,5]), (temp[,3]-temp[,6]) )
  GOS[GOS<0] <- GOS[GOS<0]+2*pi

  output <- data.frame(GA, GPX, GPY, GPZ, GPVel, GOF, GOT, GOS)
}
