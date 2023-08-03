#' @export
kin.extract.parameters <- function(data, signals, grasp = F)
{
  tryCatch(
    {
      signalsCols <- c(paste0(signals, rep(c("X","Y","Z","Vel","Acc"), length(signals))),
                       paste0(signals, rep(paste0(c("X","Y","Z"), "vel"), length(signals))))
      temp <- data[c(kinesis_parameters$dataCols[3],signalsCols)]

      time_info <- eval(substitute(

        data.frame(
          # --- general kinematics
          onset = min(temp$time),
          offset = max(temp$time)
        )
        , list(time = as.name(kinesis_parameters$dataCols[3]))
      ))

      time_info <- within(time_info,
                          {movTime = offset-onset})

      #### analysis of reaching
      output.s <- NULL
      # loop for each signal
      for(s in signals)
      {
        param.s <- eval(substitute(

          data.frame(
            # ---- final 3D position
            FX = temp$X[temp$time == time_info$offset],
            FY = temp$Y[temp$time == time_info$offset],
            FZ = temp$Z[temp$time == time_info$offset],

            # ---- final dynamics
            FXVel = temp$XVel[temp$time == time_info$offset],
            FYVel = temp$YVel[temp$time == time_info$offset],
            FZVel = temp$ZVel[temp$time == time_info$offset],

            FVel = temp$Vel[temp$time == time_info$offset],
            FAcc = temp$Acc[temp$time == time_info$offset],

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

          , list(time = as.name(kinesis_parameters$dataCols[3]),
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
        param.s <- within(param.s,
                          {
                            # time from max acceleration to max velocity
                            timeMAccToMVel = timeMVel - timeMAcc
                            # time from max velocity to max deceleration
                            timeMVelToMDec = timeMDec - timeMVel
                            # time from max deceleration to movement offset
                            timeMDecToOffset = time_info$offset - timeMDec
                          })


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
        temp <- data[c(kinesis_parameters$dataCols[3],c("GA",
                                                        "GPX","GPY","GPZ",
                                                        "GPXvel","GPYvel","GPZvel",
                                                        "GPVel","GPAcc",
                                                        "GOF","GOT","GOS"))]
        output.g <- eval(substitute(

          data.frame(
            # ---- final grip aperture
            FGA = tail(temp$GA, 1),

            # ---- maximum grip aperture
            MGA = max(temp$GA),

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

            # ---- final grip orientation
            FGOf = tail(temp$GOF, 1),
            FGOt = tail(temp$GOT, 1),
            FGOs = tail(temp$GOS, 1),

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

          , list(time = as.name(kinesis_parameters$dataCols[3]),
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
        output.g <- within(output.g,
                           {
                             # time from max acceleration to max velocity
                             timeMAccToMVel = timeMVel - timeMAcc
                             # time from max velocity to max deceleration
                             timeMVelToMDec = timeMDec - timeMVel
                             # time from max deceleration to movement offset
                             timeMDecToOffset = time_info$offset - timeMDec

                             # time to MGA
                             timeMGA = temp$time[match(MGA, temp$GA)]
                             # time from timeMGA to offset
                             timeMGAToOffset = time_info$offset - timeMGA
                           })

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
