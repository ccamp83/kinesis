#' @export
kin.extract.parameters <- function(data, signals, grasp = F)
{
  tryCatch(
    {
      signalsCols <- paste0(signals, rep(c("X","Y","Z","Vel","Acc"), length(signals)))
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

            # ---- reach dynamics
            # maximum acceleration
            MAcc = max(temp$Acc),
            # maximum velocity
            MVel = max(temp$Vel),
            # maximum deceleration
            MDec = min(temp$Acc),

            # time to maximum acceleration
            timeMAcc = temp$time[which.max(temp$Acc)],
            # time to maximum velocity
            timeMVel = temp$time[which.max(temp$Vel)],
            # time to maximum deceleration
            timeMDec = temp$time[which.min(temp$Acc)],

            # ---- spatial reach kinematics
            pathLength = max(kin.signal.arclength(temp[paste0(s, c("X","Y","Z"))])),
            MdeviationX = max(temp$X),
            MdeviationY = max(temp$Y),
            MdeviationZ = max(temp$Z)
          )

          , list(time = as.name(kinesis_parameters$dataCols[3]),
                 Acc = as.name(paste0(s, "Acc")),
                 Vel = as.name(paste0(s, "Vel")),
                 X = as.name(paste0(s, "X")),
                 Y = as.name(paste0(s, "Y")),
                 Z = as.name(paste0(s, "Z"))
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
        temp <- data[c(kinesis_parameters$dataCols[3],c("GA","GPX","GPY","GPZ","GPVel","GPAcc"))]
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

              # ---- reach dynamics
              # maximum acceleration
              MAcc = max(temp$Acc),
              # maximum velocity
              MVel = max(temp$Vel),
              # maximum deceleration
              MDec = min(temp$Acc),

              # time to maximum acceleration
              timeMAcc = temp$time[which.max(temp$Acc)],
              # time to maximum velocity
              timeMVel = temp$time[which.max(temp$Vel)],
              # time to maximum deceleration
              timeMDec = temp$time[which.min(temp$Acc)],

              # ---- spatial reach kinematics
              pathLength = max(kin.signal.arclength(temp[c("GPX","GPY","GPZ")])),
              MdeviationX = max(temp$X),
              MdeviationY = max(temp$Y),
              MdeviationZ = max(temp$Z)
            )

            , list(time = as.name(kinesis_parameters$dataCols[3]),
                   Acc = as.name("GPAcc"),
                   Vel = as.name("GPVel"),
                   X = as.name("GPX"),
                   Y = as.name("GPY"),
                   Z = as.name("GPZ")
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
    error = function(e) return(NA)
  )
}
