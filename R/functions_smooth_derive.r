# functions to smooth, derive and rescale kinematics
# they exploit built-in r functions, to be applied with ddply

#' smooth data points.
#' @param x: predictor
#' @param  y: variable to be smoothed
#' @param  l: lambda - smoothing parameter (default to .2)
#' @export
kin.smooth <- function(x,y,l=.2){
  fit <- sreg(x, y, lambda=l)
  return(predict(fit, x))
}

#' smooth then calculate derivatives of a given variable with respect to a specific predictor. Inputs:
#' @param x: predictor
#' @param y: variable to be smoothed
#' @param l: lambda - smoothing parameter (default to .2)
#' @param d: order of derivative
#' @export
kin.derive <- function(x,y,l=.2,d){predict(sreg(x,y,lambda=l), deriv=d)[match(x,sreg(x,y, lambda=l)$x)]}

#' rescale the values of a vector within a specified range. Inputs:
#' @param x: vector to be rescaled
#' @param a: range minimum
#' @param b: range maximum
#' @export
kin.rescale <- function(x,a,b) {
  rg <- c(a,b)
  rescale(x, to = rg, from = range(x, na.rm = TRUE)) }

#' @export
kin.smooth.repair <- function(x, y.raw, lam = .0005, maxFrames = 18, fingersOccluded, framesOccluded)
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

    # assign maxFrames temporary in the global environment (temporarily)
    assign("maxFrames", maxFrames, envir = .GlobalEnv)

    occluded.frames.check <- ddply(occluded.frames.check, .(group), mutate,
                                   repairable = ifelse(max(occluded) <= maxFrames, 'repair', 'discard'))

    frames.to.interpolate <- match(occluded.frames.check$f[occluded.frames.check$repairable=='repair'], x)

    y <- predict(fit, x) * ifelse(fingersOccluded == 1, NA, 1)
    y[frames.to.interpolate] <- predict(fit, x[frames.to.interpolate])

    # remove maxFrames from the global environment
    remove(maxFrames, envir = .GlobalEnv)
    # remove lam from the global environment
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

####################################################################################  13.
# function to smooth the GA (used in kin.SmoothAndDerive)
#' @export
kin.GA.smooth.repair <- function(x, y, trialN, lam = 1e-04, maxFrames = 17, fingersOccluded, framesOccluded, n.lmax, n.lmax.lim = 60)
{
  if(length(n.lmax) > 1)
    n.lmax <- unique(n.lmax)
  if(length(trialN) > 1)
    trialN <- unique(trialN)

  GA <- NULL
  if(n.lmax > n.lmax.lim)
  {
    cat("Trial #", trialN, ": too many local maxima (", n.lmax, "). Smoothing with lambda = ", lam, ".\n", sep='')
    GA <- kin.smooth.repair(x, y, lam, maxFrames, fingersOccluded, framesOccluded)
    return(GA)
  } else if(n.lmax > 5)
  {

    lmaxmin <- NULL

    while (n.lmax > 5)
    {
      GA <- kin.smooth.repair(x, y, lam, maxFrames, fingersOccluded, framesOccluded)
      lmaxmin <- which(diff(sign(diff(GA)))==-2)+1
      n.lmax <- length(lmaxmin)
      lam <- lam + .005
      if(lam > 10)
        stop(cat("smoothing parameter too big. Trial #", trialN))
    }
    return(GA)
  } else
    return(y)
}
