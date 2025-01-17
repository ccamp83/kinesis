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
#' Smooth and repair kinematic data by interpolating missing values
#' @description This function smooths kinematic data and repairs gaps in the signal by interpolating
#' missing values, but only if the gaps are smaller than a specified maximum number of frames.
#' @param x Numeric vector containing the time or frame indices
#' @param y.raw Numeric vector containing the raw kinematic data to be smoothed and repaired
#' @param lam Smoothing parameter lambda (default: 1e-18)
#' @param maxFrames Maximum number of consecutive frames that can be interpolated (default: 18)
#' @param fingersOccluded Binary vector indicating which frames contain occluded fingers (1) or not (0)
#' @param framesOccluded Numeric vector containing the count of consecutive occluded frames
#' @return A numeric vector containing the smoothed and repaired kinematic data
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

#' Repair kinematic signal by interpolating missing values
#' @description This function analyzes a kinematic signal for missing values (NAs) and repairs gaps
#' in the signal through interpolation, but only if the gaps are smaller than a specified maximum
#' number of frames.
#' @param y.raw Numeric vector containing the raw kinematic data to be repaired
#' @param maxFrames Maximum number of consecutive frames that can be interpolated (default: 18)
#' @return A numeric vector containing the repaired kinematic data
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
