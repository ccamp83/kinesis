#' @title Rotate trajectory
#' @param data a rX3 matrix of x,y,z observations (data.frame format is automatically converted to matrix)
#' @param end a 3-length vector with x,y,z coordinates of the trajectory endpoint
#' @details
#' The trajectory is assumed to start at origin (0,0,0)
#' and terminate at a point (0,0,z)
#' with x axis: rightwards positive, leftwards negative
#'      y axis: upwards positive, downwards negative
#'      z axis: forwards positive, backwards negative
#' @export
kin.rotate.trajectory <- function(data, end, f = T, t = T, s = T)
{
  # if data is not a matrix, convert to matrix
  if(!is.matrix(data))
  {
    data = as.matrix(data)
  }

  if(is.null(end))
  {
    f <- t <- s <- F
  }

  # list the planes where the rotation will be performed
  rotationPlanes <- NULL
  if(f)
    rotationPlanes <- c(rotationPlanes, "f")
  if(t)
    rotationPlanes <- c(rotationPlanes, "t")
  if(s)
    rotationPlanes <- c(rotationPlanes, "s")

  # three-steps rotation in 3D
  for(plane in rotationPlanes)
  {
    # form the couples relative to each projection plane
    # frontoparallel plane (x,y) | trasversal plane (x,z) | sagittal plane (z,y)
    endCoord.x <- c(end[1], end[3], end[3])
    endCoord.y <- c(end[2], end[1], end[2])
    # compute angles
    theta <- atan2(endCoord.y, endCoord.x)
    # rotate the trajectory
    data <- switch(plane,
                   f = rotate3d(data, theta[1] + pi, 0, 0, 1),
                   t = rotate3d(data, theta[2], 0, 1, 0),
                   s = rotate3d(data, theta[3], 1, 0, 0)
    )
    # rotate end coordinates
    end <- switch(plane,
                  f = rotate3d(end, theta[1] + pi, 0, 0, 1),
                  t = rotate3d(end, theta[2], 0, 1, 0),
                  s = rotate3d(end, theta[3], 1, 0, 0)
                  )
  }

  return(data)
}
