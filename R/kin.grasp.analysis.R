#' @export
kin.grasp.analysis <- function(data, signals, splinepar = 5e-2, ...)
{
  # check that deltaTime column is present
  timeColName <- kinesis_parameters$dataCols[3]
  hastimeCol <- timeColName %in% names(data)
  if(!hastimeCol)
  {
    message("time column not present.\n")
    stop()
  } else
  {
    timeCol <- data[,timeColName]
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
  GPVel <- as.data.frame(apply(data.frame(GPX, GPY, GPZ), 2, kin.ssFilter, x = timeCol, spar = splinepar, deriv = 1)) # 3D velocities
  names(GPVel) <- paste0(names(GPVel), "vel")
  GPVel$GPVel <- sqrt(GPVel[,1]^2 + GPVel[,2]^2 + GPVel[,3]^2)
  GPVel$GPAcc <- kin.ssFilter(timeCol, GPVel$GPVel, deriv = 1, spar = 0)

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
