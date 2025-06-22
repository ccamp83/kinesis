#' Extract Kinematic Parameters from Movement Data
#'
#' @description
#' Extracts various kinematic parameters from 3D movement data, including temporal,
#' spatial, and dynamic measures for both reaching and grasping movements.
#'
#' @param data A data frame containing movement data with time series information
#' @param signals Character vector specifying the signal names to analyze
#' @param grasp Logical value indicating whether to analyze grasping parameters (default: FALSE)
#'
#' @return A list containing:
#' \itemize{
#'   \item time_info: Data frame with movement onset, offset, and total movement time
#'   \item reach_parameters: Data frame with reaching kinematics including:
#'     \itemize{
#'       \item Final positions (FX, FY, FZ)
#'       \item Final velocities (FXVel, FYVel, FZVel, FVel)
#'       \item Peak dynamics (MVel, MAcc, MDec)
#'       \item Timing measures (timeMVel, timeMAcc, timeMDec)
#'       \item Path characteristics (pathLength, Xmax, Ymax, Zmax)
#'       \item Temporal measures (timeToXmax, timeToYmax, timeToZmax)
#'       \item Movement complexity (XlocMinN, YlocMinN, ZlocMinN, XlocMaxN, YlocMaxN, ZlocMaxN)
#'     }
#'   \item grasp_parameters: (If grasp=TRUE) Data frame with grasping kinematics including:
#'     \itemize{
#'       \item All reaching parameters
#'       \item Grip aperture measures (FGA, MGA)
#'       \item Grip timing measures (timeMGA, timeMVelToMGA, timeMGAToMDec)
#'       \item Grip orientation measures (FGOf, FGOt, FGOs)
#'     }
#' }
#'
#' @details
#' The function processes 3D movement data to extract comprehensive kinematic parameters.
#' It handles both reaching movements (default) and optionally grasping movements.
#' Time measures are normalized to movement onset. The function includes error handling
#' through tryCatch and returns NA if processing fails.
#'
#' @examples
#' \dontrun{
#' # Extract reaching parameters
#' reach_data <- kin.extract.parameters(movement_data, signals = c("hand"))
#'
#' # Extract both reaching and grasping parameters
#' grasp_data <- kin.extract.parameters(movement_data, signals = c("hand"), grasp = TRUE)
#' }
#'
#' @seealso
#' \code{\link{kin.signal.arclength}} for path length calculations
#'
#' @export
kin.extract.parameters <- function(data, signals, grasp = F)
{
  tryCatch(
    {
      signalsCols <- c(paste0(signals, rep(c("X","Y","Z","Vel","Acc"), length(signals))),
                       paste0(signals, rep(paste0(c("X","Y","Z"), "vel"), length(signals))))
      timeColName <- .kinesis_env$dataCols[3]
      temp <- data[c(timeColName,signalsCols)]

      time_info <- eval(substitute(

        data.frame(
          # --- general kinematics
          onset = min(temp$time),
          offset = max(temp$time)
        )
        , list(time = as.name(timeColName))
      ))

      time_info <- within(time_info,
                          {movTime = offset-onset})

      # subtract onset to time column so it starts from zero
      temp[timeColName] <- temp[timeColName] - time_info$onset

      #### analysis of reaching
      output.s <- NULL
      # loop for each signal
      for(s in signals)
      {
        param.s <- eval(substitute(

          data.frame(
            # ---- final 3D position
            FX = tail(temp$X, 1),
            FY = tail(temp$Y, 1),
            FZ = tail(temp$Z, 1),

            # ---- final dynamics
            FXVel = tail(temp$XVel, 1),
            FYVel = tail(temp$YVel, 1),
            FZVel = tail(temp$ZVel, 1),

            FVel = tail(temp$Vel, 1),
            FAcc = tail(temp$Acc, 1),

            # ---- reach dynamics
            # maximum velocity
            MVel = max(temp$Vel),
            # maximum acceleration
            MAcc = max(temp$Acc),
            # maximum deceleration
            MDec = min(temp$Acc),

            # time to maximum velocity
            timeMVel = temp$time[which.max(temp$Vel)],
            # time to maximum acceleration
            timeMAcc = temp$time[which.max(temp$Acc)],
            # time to maximum deceleration
            timeMDec = temp$time[which.min(temp$Acc)],

            # ---- spatial reach kinematics
            pathLength = max(kin.signal.arclength(temp[paste0(s, c("X","Y","Z"))])),
            Xmax = max(temp$X),
            Ymax = max(temp$Y),
            Zmax = max(temp$Z),

            # ---- time of spatial reach kinematics
            timeToXmax = temp$time[which.max(temp$X)],
            timeToYmax = temp$time[which.max(temp$Y)],
            timeToZmax = temp$time[which.max(temp$Z)],

            # ---- number of local minima
            XlocMinN = length(minima(temp$X)),
            YlocMinN = length(minima(temp$Y)),
            ZlocMinN = length(minima(temp$Z)),

            # ---- number of local maxima
            XlocMaxN = length(maxima(temp$X)),
            YlocMaxN = length(maxima(temp$Y)),
            ZlocMaxN = length(maxima(temp$Z))
          )

          , list(time = as.name(timeColName),
                 Acc = as.name(paste0(s, "Acc")),
                 Vel = as.name(paste0(s, "Vel")),
                 X = as.name(paste0(s, "X")),
                 Y = as.name(paste0(s, "Y")),
                 Z = as.name(paste0(s, "Z")),
                 XVel = as.name(paste0(s, "Xvel")),
                 YVel = as.name(paste0(s, "Yvel")),
                 ZVel = as.name(paste0(s, "Zvel"))
          )
        ))

        # other parameters
        param.s <- param.s %>% mutate(

          # time from max acceleration to max velocity
          timeMAccToMVel = timeMVel - timeMAcc,
          # time from max velocity to max deceleration
          timeMVelToMDec = timeMDec - timeMVel,
          # time from max deceleration to movement offset
          timeMDecToOffset = time_info$offset - timeMDec
          )

        param.s$signal <- factor(s)
        param.s <- param.s[c(ncol(param.s), 1:(ncol(param.s)-1))]

        output.s <- rbind(output.s, param.s)
      }
      # flush temp
      temp <- NULL

      # create output
      output <- list(time_info = time_info, reach_parameters = output.s)

      if(grasp)
      {
        #### analysis of grasping
        temp <- data[c(timeColName,c("GA",
                                                        "GPX","GPY","GPZ",
                                                        "GPXvel","GPYvel","GPZvel",
                                                        "GPVel","GPAcc",
                                                        "GOF","GOT","GOS"))]

        # subtract onset to time column so it starts from zero
        temp[timeColName] <- temp[timeColName] - time_info$onset

        output.g <- eval(substitute(

          data.frame(
            # ---- final 3D position
            FX = tail(temp$X, 1),
            FY = tail(temp$Y, 1),
            FZ = tail(temp$Z, 1),

            # ---- final dynamics
            FXVel = tail(temp$XVel, 1),
            FYVel = tail(temp$YVel, 1),
            FZVel = tail(temp$ZVel, 1),

            FVel = tail(temp$Vel, 1),
            FAcc = tail(temp$Acc, 1),

            # ---- reach dynamics
            # maximum velocity
            MVel = max(temp$Vel),
            # maximum acceleration
            MAcc = max(temp$Acc),
            # maximum deceleration
            MDec = min(temp$Acc),

            # time to maximum velocity
            timeMVel = temp$time[which.max(temp$Vel)],
            # time to maximum acceleration
            timeMAcc = temp$time[which.max(temp$Acc)],
            # time to maximum deceleration
            timeMDec = temp$time[which.min(temp$Acc)],

            # ---- spatial reach kinematics
            pathLength = max(kin.signal.arclength(temp[c("GPX","GPY","GPZ")])),
            Xmax = max(temp$X),
            Ymax = max(temp$Y),
            Zmax = max(temp$Z),

            # ---- time of spatial reach kinematics
            timeToXmax = temp$time[which.max(temp$X)],
            timeToYmax = temp$time[which.max(temp$Y)],
            timeToZmax = temp$time[which.max(temp$Z)],

            # ---- number of local minima
            XlocMinN = length(minima(temp$X)),
            YlocMinN = length(minima(temp$Y)),
            ZlocMinN = length(minima(temp$Z)),

            # ---- number of local maxima
            XlocMaxN = length(maxima(temp$X)),
            YlocMaxN = length(maxima(temp$Y)),
            ZlocMaxN = length(maxima(temp$Z))
          )

          , list(time = as.name(timeColName),
                 Acc = as.name("GPAcc"),
                 Vel = as.name("GPVel"),
                 X = as.name("GPX"),
                 Y = as.name("GPY"),
                 Z = as.name("GPZ"),
                 XVel = as.name("GPXvel"),
                 YVel = as.name("GPYvel"),
                 ZVel = as.name("GPZvel")
          )
        ))

        # other parameters
        output.g <- output.g %>% mutate(

          # time from max acceleration to max velocity
          timeMAccToMVel = timeMVel - timeMAcc,
          # time from max velocity to max deceleration
          timeMVelToMDec = timeMDec - timeMVel,
          # time from max deceleration to movement offset
          timeMDecToOffset = time_info$offset - timeMDec,

          # ---- final grip aperture
          FGA = tail(temp$GA, 1),
          # ---- maximum grip aperture
          MGA = max(temp$GA),
          # time to MGA
          timeMGA = temp$time[match(MGA, temp$GA)],
          # time from MVel to MGA
          timeMVelToMGA = timeMGA - timeMVel,
          # time from MGA to MDec
          timeMGAToMDec = timeMDec - timeMGA,
          # time from MGA to offset
          timeMGAToOffset = time_info$offset - timeMGA,
          # maximum velocity at MGA
          MGAVel = temp$GPVel[match(timeMGA, temp$time)],
          # maximum acceleration at MGA
          MGAAcc = temp$GPAcc[match(timeMGA, temp$time)],
          # ---- final grip orientation
          FGOf = tail(temp$GOF, 1),
          FGOt = tail(temp$GOT, 1),
          FGOs = tail(temp$GOS, 1)
          )

        output.g$signal <- factor("grasp")
        output.g <- output.g[c(ncol(output.g), 1:(ncol(output.g)-1))]
        output$grasp_parameters <- output.g
      }

      return(output)
    },
    error = function(e) {
      message(paste("Error:", e))
      return(NA)
    }
  )
}
