#' @title Smooth and derive kinematics
#' @description Smooth and derive kinematics
#' @param dataset an object of the type data.frame
#' @param lambda smoothing factor. If not specified, it is = 5e-04
#' @param frames.to.interpolate the minimum amount of frames across which to interpolate during the spline fit in case of missing data. Default to 18
#' @param index logical: is the index included in the dataset?
#' @param thumb logical: is the thumb included in the dataset?
#' @param reverse.Zcoords logical: should the Z coordinates be multiplied by -1?
#' @examples
#' libraries()
#'
#' data(rtgData)
#' rtgData <- data.check(rtgData)
#' system.time(testData <- kin.SmoothAndDerive(rtgData))
#'
#' view.summary.trials(21:40, testData, print=F)
#'
#' detach(rtgData)
#' @export
kin.SmoothAndDerive <- function(dataset, lambda = NULL, frames.to.interpolate = 18, index=T, thumb=T, reverse.Zcoords = T, ...)
{
  # assign frames.to.interpolate to global environment for looping (temporarily)
  assign("frames.to.interpolate", frames.to.interpolate, envir = .GlobalEnv)

  if(reverse.Zcoords)
  {
    reverse.constant <- -1
  } else
  {
    reverse.constant <- 1
  }

  if(is.null(lambda))
    lambda <- 5e-04

  # assign lambda and x to global environment for looping (temporarily)
  assign("lambda", lambda, envir = .GlobalEnv)

  if(index)
  {
    # fingers z coords are reversed in sign
    dataset$indexZraw <- reverse.constant*dataset$indexZraw

    # smooth and derive
    dataset <- ddply(dataset, .(trialN), mutate,
                  # index
                  indexX=kin.smooth.repair(frameN,indexXraw, lam = lambda, maxFrames= frames.to.interpolate, fingersOccluded=fingersOccluded, framesOccluded=framesOccluded),
                  indexXvel=kin.derive(frameN,indexX,d=1) / frameT,
                  indexY=kin.smooth.repair(frameN,indexYraw, lam = lambda, maxFrames= frames.to.interpolate, fingersOccluded=fingersOccluded, framesOccluded=framesOccluded),
                  indexYvel=kin.derive(frameN,indexY,d=1) / frameT,
                  indexZ=kin.smooth.repair(frameN,indexZraw, lam = lambda, maxFrames= frames.to.interpolate, fingersOccluded=fingersOccluded, framesOccluded=framesOccluded),
                  indexZvel=kin.derive(frameN,indexZ,d=1) / frameT,
                  indexVel=sqrt(indexXvel^2 + indexYvel^2 + indexZvel^2),
                  indexAcc=kin.derive(frameN,indexVel,d=1) / frameT,
                  indexJerk=kin.derive(frameN,indexAcc,d=2) / frameT,
                  indexAllKinRaw=sqrt(indexVel^2 + indexAcc^2 + indexJerk^2)
    )
  }

  if(thumb)
  {
    dataset$thumbZraw <- reverse.constant*dataset$thumbZraw
    # smooth and derive
    dataset <- ddply(dataset, .(trialN), mutate,
                  # thumb
                  thumbX=kin.smooth.repair(frameN,thumbXraw, lam = lambda, maxFrames= frames.to.interpolate, fingersOccluded=fingersOccluded, framesOccluded=framesOccluded),
                  thumbXvel=kin.derive(frameN,thumbX,d=1) / frameT,
                  thumbY=kin.smooth.repair(frameN,thumbYraw, lam = lambda, maxFrames= frames.to.interpolate, fingersOccluded=fingersOccluded, framesOccluded=framesOccluded),
                  thumbYvel=kin.derive(frameN,thumbY,d=1) / frameT,
                  thumbZ=kin.smooth.repair(frameN,thumbZraw, lam = lambda, maxFrames= frames.to.interpolate, fingersOccluded=fingersOccluded, framesOccluded=framesOccluded),
                  thumbZvel=kin.derive(frameN,thumbZ,d=1) / frameT,
                  thumbVel=sqrt(thumbXvel^2 + thumbYvel^2 + thumbZvel^2),
                  thumbAcc=kin.derive(frameN,thumbVel,d=1) / frameT,
                  thumbJerk=kin.derive(frameN,thumbAcc,d=2) / frameT,
                  thumbAllKinRaw=sqrt(thumbVel^2 + thumbAcc^2 + thumbJerk^2)
    )
  }

  # if both fingers are available then we move on to calculating the grip aperture (GA)
  if(index && thumb)
  {
    # Grip Aperture (raw)
    # GA is the 3D euclidean distance between the index and the thumb
    dataset$GAraw <- with(dataset,
                       sqrt(
                         (indexXraw - thumbXraw)^2 +
                           (indexYraw - thumbYraw)^2 +
                           (indexZraw - thumbZraw)^2
                       )
    )

    # 3D Grip Position (raw)
    # GP is the midway position between index and thumb
    dataset$GPXraw <- with(dataset, (indexXraw + thumbXraw)/2)
    dataset$GPYraw <- with(dataset, (indexYraw + thumbYraw)/2)
    dataset$GPZraw <- with(dataset, (indexZraw + thumbZraw)/2)

    # smooth and derive
    dataset <- ddply(dataset, .(trialN), mutate,
                  # grip aperture
                  GA=kin.smooth.repair(frameN, GAraw, lam = lambda, maxFrames= frames.to.interpolate, fingersOccluded=fingersOccluded, framesOccluded=framesOccluded),
                  GAvel=kin.derive(frameN,GA,d=1) / frameT,
                  GAacc=kin.derive(frameN,GA,d=2) / frameT,
                  GAjerk=kin.derive(frameN,GAacc,d=1) / frameT,
                  GAallKinRaw=sqrt(GAvel^2 + GAacc^2 + GAjerk^2),

                  # grip position
                  GPX=kin.smooth.repair(frameN,GPXraw, lam = lambda, maxFrames= frames.to.interpolate, fingersOccluded=fingersOccluded, framesOccluded=framesOccluded),
                  GPXvel=kin.derive(frameN,GPX,d=1) / frameT,
                  GPY=kin.smooth.repair(frameN,GPYraw, lam = lambda, maxFrames= frames.to.interpolate, fingersOccluded=fingersOccluded, framesOccluded=framesOccluded),
                  GPYvel=kin.derive(frameN,GPY,d=1) / frameT,
                  GPZ=kin.smooth.repair(frameN,GPZraw, lam = lambda, maxFrames= frames.to.interpolate, fingersOccluded=fingersOccluded, framesOccluded=framesOccluded),
                  GPZvel=kin.derive(frameN,GPZ,d=1) / frameT,
                  GPvel=sqrt(GPXvel^2 + GPYvel^2 + GPZvel^2),
                  GPacc=kin.derive(frameN,GPvel,d=1) / frameT,
                  GPjerk=kin.derive(frameN,GPacc,d=2) / frameT,
                  GPallKinRaw=sqrt(GPvel^2 + GPacc^2 + GPjerk^2),

                  # global momentum
                  fingersAllKinRaw=sqrt(indexAllKinRaw^2 + thumbAllKinRaw^2 + GAallKinRaw^2 + GPallKinRaw^2)
    )
  }

  # 3D Grip Orientation (raw)
  # GOF is the angle between the parallel projection of GA and the x axis on the Frontal (coronal) plane
  dataset$GOFraw <- with(dataset, atan( (indexYraw-thumbYraw) / (indexXraw-thumbXraw) )*180/pi )
  # GOS is the angle between the parallel projection of GA and the z axis on the Sagittal plane
  dataset$GOSraw <- with(dataset, atan( (indexYraw-thumbYraw) / (indexZraw-thumbZraw) )*180/pi )
  # GOT is the angle between the parallel projection of GA and the z axis on the Transverse plane
  dataset$GOTraw <- with(dataset, atan( (indexXraw-thumbXraw) / (indexZraw-thumbZraw) )*180/pi )

  dataset <- ddply(dataset, .(trialN), mutate,
                   # grip orientation
                   GOF=kin.smooth.repair(frameN,GOFraw, lam = lambda, maxFrames= frames.to.interpolate, fingersOccluded=fingersOccluded, framesOccluded=framesOccluded) / frameT,
                   GOS=kin.smooth.repair(frameN,GOSraw, lam = lambda, maxFrames= frames.to.interpolate, fingersOccluded=fingersOccluded, framesOccluded=framesOccluded) / frameT,
                   GOT=kin.smooth.repair(frameN,GOTraw, lam = lambda, maxFrames= frames.to.interpolate, fingersOccluded=fingersOccluded, framesOccluded=framesOccluded) / frameT
  )

  # remove lambda from global environment
  remove(lambda, envir = .GlobalEnv)
  # remove frames.to.interpolate from global environment
  remove(frames.to.interpolate, envir = .GlobalEnv)

  cat("Smoothing completed.\n")
  return(dataset)

}
