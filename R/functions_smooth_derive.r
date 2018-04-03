# functions to smooth, derive and rescale kinematics
# they exploit built-in r functions, to be applied with ddply

# kin.smooth ----
#' smooth data points.
#' @param x: predictor
#' @param  y: variable to be smoothed
#' @param  l: lambda - smoothing parameter (default to .2)
#' @export
kin.smooth <- function(x,y,l=.2){
  fit <- sreg(x, y, lambda=l)
  return(predict(fit, x))
}

# kin.derive ----
#' smooth then calculate derivatives of a given variable with respect to a specific predictor. Inputs:
#' @param x: predictor
#' @param y: variable to be smoothed
#' @param l: lambda - smoothing parameter (default to .2)
#' @param d: order of derivative
#' @export
kin.derive <- function(x,y,l=.2,d){predict(sreg(x,y,lambda=l), deriv=d)[match(x,sreg(x,y, lambda=l)$x)]}

# kin.rescale ----
#' rescale the values of a vector within a specified range. Inputs:
#' @param x: vector to be rescaled
#' @param a: range minimum
#' @param b: range maximum
#' @export
kin.rescale <- function(x,a,b) {
  rg <- c(a,b)
  rescale(x, to = rg, from = range(x, na.rm = TRUE)) }

# kin.smooth.repair ----
#' @export
kin.smooth.repair <- function(x, y.raw, lam = 1e-18, maxFrames = 18, fingersOccluded, framesOccluded)
{
  y.raw[which(fingersOccluded==1)] <- NA

  # assign lam temporary in the global environment (temporarily)
  assign("lam", lam, envir = .GlobalEnv)

  if(any(is.na(y.raw)))
  {
    # which rows contain NAs?
    missing.frames <- attr(na.omit(y.raw), "na.action")

    fit <- sreg(x[-missing.frames], na.omit(y.raw),lambda=lam)

    occluded.frames.check <- data.frame(
      f = (fingersOccluded*x)[fingersOccluded*x != 0]
      ,
      occluded = (fingersOccluded*framesOccluded)[fingersOccluded*framesOccluded != 0]
    )
    occluded.frames.check$group <- with(occluded.frames.check, c(0, cumsum(diff(f) != 1)))

    # assign maxFrames temporarily to the global environment
    assign("maxFrames", maxFrames, envir = .GlobalEnv)

    occluded.frames.check <- ddply(occluded.frames.check, .(group), mutate,
                                   repairable = ifelse(max(occluded) <= maxFrames, 'repair', 'discard'))

    frames.to.interpolate <- match(occluded.frames.check$f[occluded.frames.check$repairable=='repair'], x)

    y <- predict(fit, x) * ifelse(fingersOccluded == 1, NA, 1)
    y[frames.to.interpolate] <- predict(fit, x[frames.to.interpolate])

    # remove maxFrames from global environment
    remove(maxFrames, envir = .GlobalEnv)
    # remove lam from global environment
    remove(lam, envir = .GlobalEnv)

    return(y)

  }else
  {
    fit <- sreg(x, y.raw,lambda=lam)
    y <- predict(fit, x)

    # remove lam from the global environment
    remove(lam, envir = .GlobalEnv)

    return(y)
  }
}

# kin.signal.repair ----
#' @export
kin.signal.repair <- function(y.raw, maxFrames = 18)
{
  # first check if the signal needs to be assessed
  # are there NAs?
  na.detected <- any(is.na(y.raw))
  # if not, check the signal
  if(!na.detected)
  {
    y.raw <- kin.signal.missing(y.raw)
    na.detected <- any(is.na(y.raw))
  }
  # if yes, analyze the signal
  if(na.detected)
  {
    cat("Reconstructing missing data...")

    # which positions contain NAs?
    missing.frames <- which(is.na(y.raw))

    # flag the NAs in the vector
    y.raw.flag.na <- ifelse(is.na(y.raw), 1, 0)
    # cumulative count of NAs
    y.raw.flag.na.count <- y.raw.flag.na * unlist(lapply(rle(y.raw.flag.na)$lengths, seq_len))

    # create predictor
    x <- 1:length(y.raw)

    # fit the signal without NAs
    fit <- fields::sreg(x[-missing.frames], na.omit(y.raw),lambda=1e-18)

    # gather all missing data in groups
    missing.frames.check <- data.frame(
      missing.frames
      ,
      count = y.raw.flag.na.count[y.raw.flag.na.count!=0]
    )
    missing.frames.check$group <- with(missing.frames.check, c(0, cumsum(diff(count) != 1)))

    # group-by-group decide whether to repair
    assign("maxFrames", maxFrames, envir = .GlobalEnv) # assign maxFrames temporarily to the global environment
    missing.frames.check <- ddply(missing.frames.check, .(group), mutate,
                                  decision = ifelse(max(count) <= maxFrames, 'repair', 'discard'))

    # extract frames for interpolation
    frames.to.interpolate <- match(missing.frames.check$missing.frames[missing.frames.check$decision=='repair'], x)

    # repair the signal
    y <- y.raw
    y[frames.to.interpolate] <- predict(fit, x[frames.to.interpolate])

    # remove maxFrames from global environment
    remove(maxFrames, envir = .GlobalEnv)

    cat("Done.\n")
    return(y)

  } else
  {
    cat("Signal looks good.\n")
    return(y.raw)
  }
}

# kin.signal.analysis
#' @export
kin.signal.analysis <- function(signal, signal.name = "signal", start, end, maxFrames = 20, deltaTime)
{
  # make backup
  signal.backup <- signal
  # name of signal
  names(signal) <- paste(signal.name, c("X","Y","Z"), "raw", sep = "")

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
  signalRot <- as.data.frame(kin.rotate.trajectory(signalTra, end - start))
  names(signalRot) <- paste(signal.name, c("X","Y","Z"), "rot", sep = "")
  # vel, acc
  signalVel <- as.data.frame(apply(signalRot, 2, kin.sgFilter, m=1, ts = deltaTime)) # 3D velocities
  names(signalVel) <- paste(signal.name, c("X","Y","Z"), "vel", sep = "")
  signalVel$vel_temp <- kin.sgFilter(sqrt(signalVel[,1]^2 + signalVel[,2]^2 + signalVel[,3]^2), ts = deltaTime)
  signalVel$acc_temp <- kin.sgFilter(kin.sgFilter(signalVel$vel_temp, m = 1, ts = deltaTime), p = 12, ts = deltaTime)
  names(signalVel)[4:5] <- paste(signal.name, c("Vel","Acc"), sep = "")
  # merge
  signal <- cbind(signalRot, signalVel)
  names(signal)[1:3] <- paste(signal.name, c("X","Y","Z"), sep = "")

  return(signal)
}

