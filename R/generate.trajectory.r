#' Generate index and thumb trajectories accordind to Brenner and Smeets' model (1998)
#' @param start a vector consisting of the x, y and z coordinates of the start position
#' @param end a vector consisting of the x, y and z coordinates of the end position (target's geometrical center)
#' @param phi final grip orientation (in radians). Must be 0 <= phi <= 3/4*pi. In case the final end z coordinate is negative, phi is automatically changed in sign too (otherwise positive pi/2 would result in the index reaching closer than the thumb)
#' @param ap approaching parameter, as in Brenner & Smeets (1998)
#' @param MT movement time (in millisec)
#' @param r radius of the target object
#' @note If phi = pi/2 it becomes pi/2+0.00001 to avoid an unknown bug in kin.SmoothAndDerive().
#' Since Brenner and Smeets' model assumes unconstrained path for each digit, when phi is > pi*.64 (pi*16/25) the index takes an internal trajectory with respect to the thumb.
#' @examples 
#' libraries()
#' dig <- generate.trajectory(start=c(200,-300,0), end=c(0,0,400), phi=pi/4, ap=1000, MT=1500, r=40)
#' ggplot() + geom_point(aes(indexXraw, indexZraw), data=dig, color='red') + 
#'    geom_point(aes(thumbXraw, thumbZraw), data=dig, color='blue') + coord_fixed()
#' plot3d(dig[,c(1,3,2)], col='red', radius=4, type='s')
#' spheres3d(dig[,c(4,6,5)], col="blue", radius=4)
#' 
#' # can be used with the other function of the shapelab package too:
#' libraries()
#' dig <- generate.trajectory(start=c(200,-300,0), end=c(0,0,-400), phi=pi/4, ap=1000, MT=1500/11, r=40) 
#' # note the negative sign of the final z coord since kin.SmoothAndDerive will multiply z coord by -1 
#' # also note that MT was scaled by the refresh time (11ms in this case) to simulate optotrak sampling
#' dig$trialN <- 1
#' dig <- data.check(dig) # follows a bogus subjName (testName) as input to data.check - testName is not a function
#' testName
#' dig <- kin.SmoothAndDerive(dig)
#' head(dig)
#' kinDig <- kin.extract(dig)
#' view.single.trial(1, dig)
#' view.single.trial.GA(1, dig, kinDig)
#' view.single.trial.grasp3d.anim(1, dig, kinDig, type='s')
#' 
#' @export
generate.trajectory <- function(start, end, phi, ap, MT, r)
{
  ## Phi is checked first
  # stop if phi is outside of the required range
  phi.temp <- phi # use phi.temp since phi could become negative later, thus falsifying the following condition
  if(phi.temp < 0 | phi.temp > 3/4*pi)
    stop('Angle phi must be 0 <= phi <= 3/4*pi.')
  
  # if phi = pi/2 it becomes pi/2+0.00001 due to an unknown bug in kin.SmoothAndDerive
  if(phi == pi/2)
    phi <- phi + 0.00001
  
  # phi is also given the same sign of the final z coord, to avoid using negative angles phi in declaration
  phi <- sign(end[3]) * phi
  
  # Digits' initial positions
  xi <- start[1]
  yi <- start[2]
  zi <- start[3]
  
  # Digits' final positions
  xf <- end[1]
  yf <- end[2]
  zf <- end[3]
  
  # Time vector
  t <- 1:MT
  
  # Normalized time vector
  tr <- t/MT
  
  # Traveled distance (euclidean distance from starting point to geometrical center of target object)
  l <- sqrt((xf-xi)^2 + (zf-zi)^2)
  
  # Reaching angle
  theta <- acos(zf/l)
  
  # Index:
  index.traj <- function(phi, ap, tr, r, l, yf, yi, MT)
  {
    x <- cos(phi) * ( (1/2)*ap*(tr - 1)^2 + r*(6*(tr^2) - 15*tr + 10) ) * tr^3
    z <- ( (1/2)*ap*sin(phi)*(tr - 1)^2 + (l + r*sin(phi))*(6*(tr^2) - 15*tr + 10) ) * tr^3
    y <- yi + (yf - yi) * ( 10*(tr^3) - 15*(tr^4) + 6*(tr^5) )
    
    return(matrix(c(x,y,z), ncol=3))
  }
  index <- index.traj(phi-theta, ap, tr, r, l, yf, yi, MT)
  index <- data.frame(rotate3d(index, theta, 0, 1, 0))
  names(index) <- paste("index", c("X","Y","Z"), "raw", sep='')
  index$indexXraw <- index$indexXraw - (end[1]-start[1])
  index$indexZraw <- index$indexZraw - start[3]
  
  # Thumb:
  thumb.traj <- function(phi, ap, tr, r, l, yf, yi, MT)
  {
    x <- -cos(phi) * ( (1/2)*ap*(tr - 1)^2 + r*(6*(tr^2) - 15*tr + 10) ) * tr^3
    z <- ( -(1/2)*ap*sin(phi)*(tr - 1)^2 + (l - r*sin(phi))*(6*(tr^2) - 15*tr + 10) ) * tr^3
    y <- yi + (yf - yi) * ( 10*(tr^3) - 15*(tr^4) + 6*(tr^5) )
    
    return(matrix(c(x,y,z), ncol=3))
  }
  thumb <- thumb.traj(phi-theta, ap, tr, r, l, yf, yi, MT)
  thumb <- data.frame(rotate3d(thumb, theta, 0, 1, 0))
  names(thumb) <- paste("thumb", c("X","Y","Z"), "raw", sep='')
  thumb$thumbXraw <- thumb$thumbXraw - (end[1]-start[1])
  thumb$thumbZraw <- thumb$thumbZraw - start[3]
  
  digits <- cbind(index, thumb)

  return(digits)
}
