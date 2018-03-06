#' Extract various kinematics from a grasp
#' @param dataset an object of the type data.frame
#' @param MGA.range optional: a two elements vector with the interval (in frames) within which to look for the MGA (the greatest value of grip aperture within the interval). If not specified then the range is the entire grasp
#' @param TGA.type character: "s" tells the function to extract the TGA based on a spatial threshold (default); "v" extracts the TGA based on a velocity threshold. In both cases the threshold value is specified by the parameter TGA.lim
#' @param TGA.lim the value of the threshold for the extraction of the TGA
#' @param graphs logic: should a summary graph be printed? (default to false)
#' @examples
#' libraries()
#' 
#' data(rtgData)
#' rtgData <- data.check(rtgData)
#' testData <- kin.SmoothAndDerive(rtgData)
#' system.time(testData.kin <- kin.extract(testData))
#' system.time(testData.kin2 <- kin.extract(testData, MGA.range=c(70,90)))
#' 
#' view.summary.trials.GA(21:40, testData, testData.kin, print=F)
#' view.summary.trials.GA(21:40, testData, testData.kin2, print=F)
#' 
#' detach(debugData)
#' @export
kin.extract <- function(dataset, MGA.range=NULL, TGA.type = "s", TGA.lim = 20, TGA.kin="g", graphs=F, ...)
{
  dataset$graspToObjXYdistance <- with(dataset, sqrt(GPX^2 + GPY^2))
  # find the closest distance to (0,0) that hand reached
  dataset <- ddply(dataset, .(trialN), mutate, 
                   minHandTargDist=min(graspToObjXYdistance, na.rm=T))
  
  # FGA will be explored within a region defined by a circle of radius 5cm from the smallest distance from the hand to the target
  dataset$graspInTargetRegion <- with(dataset, ifelse(graspToObjXYdistance < max(dataset$minHandTargDist)+50.0, 1, 0))
  
  # extraction of info about final grasp
  data.final <- ddply(subset(dataset, graspInTargetRegion==1), .(trialN), summarise, 
                      
                      # Final Grip Aperture
                      FGA = GA[match(cc.min(fingersAllKinRaw), fingersAllKinRaw)],
                      timeToFGA = time[match(FGA, GA)],
                      frameToFGA = frameN[match(FGA, GA)],
                      motionAtFGA = fingersAllKinRaw[match(FGA, GA)],
                      
                      # Final Grip Position
                      FGPx=GPX[match(FGA, GA)],
                      FGPy=GPY[match(FGA, GA)],
                      FGPz=GPZ[match(FGA, GA)],
                      
                      # Final Index Position
                      FIPx=indexX[match(FGA, GA)],
                      FIPy=indexY[match(FGA, GA)],
                      FIPz=indexZ[match(FGA, GA)],
                      
                      # Final Thumb Position
                      FTPx=thumbX[match(FGA, GA)],
                      FTPy=thumbY[match(FGA, GA)],
                      FTPz=thumbZ[match(FGA, GA)],
                      
                      # Final Grip Orientation
                      FGOf= GOF[match(FGA, GA)],
                      FGOs= GOS[match(FGA, GA)],
                      FGOt= GOT[match(FGA, GA)]
  )
  
  # bring data.final temporary in the global environment (not the best solution, TO REVIEW)
  assign("data.final", data.final, envir = .GlobalEnv)
  
  # now drop everything after the FGA
  dataset <- ddply(dataset, .(trialN), mutate, 
                   isMovementOver = ifelse(frameN <= data.final$frameToFGA[data.final$trialN==unique(trialN)], 0, 1)
  )
  
  dataset <- subset(dataset, isMovementOver == 0)
  
  # extraction of MGV
  data.MGV <- ddply(dataset, .(trialN), summarise,
                    
                    # Maximum Grip Velocity
                    MGV = cc.max(GPvel),
                    timeToMGV = time[match(MGV, GPvel)],
                    frameToMGV = frameN[match(MGV, GPvel)],
                    
                    # Grip Orientation at MGV
                    GOf.MGV = GOF[match(MGV, GPvel)],
                    GOs.MGV= GOS[match(MGV, GPvel)],
                    GOt.MGV= GOT[match(MGV, GPvel)],
                    
                    # Grip Position at MGV
                    GPx.MGV=GPX[match(MGV, GPvel)],
                    GPy.MGV=GPY[match(MGV, GPvel)],
                    GPz.MGV=GPZ[match(MGV, GPvel)],
                    
                    # Index Position at MGV
                    IPx.MGV=indexX[match(MGV, GPvel)],
                    IPy.MGV=indexY[match(MGV, GPvel)],
                    IPz.MGV=indexZ[match(MGV, GPvel)],
                    
                    # Thumb Position at MGV
                    TPx.MGV=thumbX[match(MGV, GPvel)],
                    TPy.MGV=thumbY[match(MGV, GPvel)],
                    TPz.MGV=thumbZ[match(MGV, GPvel)]
  )
  
  # extraction of MGACC
  data.MGACC <- ddply(dataset, .(trialN), summarise,
                      
                      # Maximum Grip Acceleration
                      MGACC = cc.max(GPacc),
                      timeToMGACC = time[match(MGACC, GPacc)],
                      frameToMGACC = frameN[match(MGACC, GPacc)],
                      
                      # Grip Orientation at MGACC
                      GOf.MGACC = GOF[match(MGACC, GPacc)],
                      GOs.MGACC= GOS[match(MGACC, GPacc)],
                      GOt.MGACC= GOT[match(MGACC, GPacc)],
                      
                      # Grip Position at MGACC
                      GPx.MGACC=GPX[match(MGACC, GPacc)],
                      GPy.MGACC=GPY[match(MGACC, GPacc)],
                      GPz.MGACC=GPZ[match(MGACC, GPacc)],
                      
                      # Index Position at MGACC
                      IPx.MGACC=indexX[match(MGACC, GPacc)],
                      IPy.MGACC=indexY[match(MGACC, GPacc)],
                      IPz.MGACC=indexZ[match(MGACC, GPacc)],
                      
                      # Thumb Position at MGACC
                      TPx.MGACC=thumbX[match(MGACC, GPacc)],
                      TPy.MGACC=thumbY[match(MGACC, GPacc)],
                      TPz.MGACC=thumbZ[match(MGACC, GPacc)]
  )
  
  data.other <- ddply(dataset, .(trialN), summarise,
                      
                      # Maximum Grip Aperture Acceleration
                      MGAacc = cc.max(GAacc),
                      timeToMGAacc = time[match(MGAacc, GAacc)],
                      frameToMGAacc = frameN[match(MGAacc, GAacc)],
                      
                      # Movement Time
                      movTime = cc.max(time)
  )
  
  # calculate MGAvel (this is done regardless of whichever chioce the user made)
  data.MGAvel <- ddply(dataset, .(trialN), summarise,
                       
                       # Maximum Grip Aperture Velocity
                       MGAvel = cc.max(GAvel),
                       timeToMGAvel = time[match(MGAvel, GAvel)],
                       frameToMGAvel = frameN[match(MGAvel, GAvel)]
  )
  
  # now we check whether the user defined a region of interest
  assign("MGA.range", MGA.range, envir = .GlobalEnv)
  
  # if no ROI is defined then all the trial is explored
  if(is.null(MGA.range))
  {
    dataset <- ddply(dataset, .(trialN), mutate, 
                     MGA.roi = 1)
  } else
  {
    dataset <- ddply(dataset, .(trialN), mutate, 
                     MGA.roi = ifelse(frameN >= MGA.range[1] & frameN <= MGA.range[2], 1, 0)
    )
  }
  
  # extraction of MGA
  data.mga <- ddply(subset(dataset, MGA.roi==1), .(trialN), summarise,
                    
                    # Maximum Grip Aperture
                    MGA = cc.max(GA),
                    timeToMGA = time[match(MGA, GA)],
                    frameToMGA = frameN[match(MGA, GA)],
                    motionAtMGA = fingersAllKinRaw[match(MGA, GA)],
                    
                    # Grip Orientation at MGA
                    GOf.MGA = GOF[match(MGA, GA)],
                    GOs.MGA= GOS[match(MGA, GA)],
                    GOt.MGA= GOT[match(MGA, GA)],
                    
                    # Grip Position at MGA
                    GPx.MGA=GPX[match(MGA, GA)],
                    GPy.MGA=GPY[match(MGA, GA)],
                    GPz.MGA=GPZ[match(MGA, GA)],
                    
                    # Grip Velocity at MGA
                    GPvel.MGA=GPvel[match(MGA, GA)],
                    
                    # Grip Acceleration at MGA
                    GPacc.MGA=GPacc[match(MGA, GA)],
                    
                    # Index Position at MGA
                    IPx.MGA=indexX[match(MGA, GA)],
                    IPy.MGA=indexY[match(MGA, GA)],
                    IPz.MGA=indexZ[match(MGA, GA)],
                    
                    # Thumb Position at MGA
                    TPx.MGA=thumbX[match(MGA, GA)],
                    TPy.MGA=thumbY[match(MGA, GA)],
                    TPz.MGA=thumbZ[match(MGA, GA)]
  )
  
  assign("data.mga", data.mga, envir = .GlobalEnv)
  
  # drop everything before the MGA
  dataset <- ddply(dataset, .(trialN), mutate, 
                   afterMGA = ifelse(frameN <= data.mga$frameToMGA[data.mga$trialN==unique(trialN)], 0, 1)
  )
  dataset <- subset(dataset, afterMGA == 1)
  
  if(TGA.type == "v")
  {
    assign("TGAprop.lim", TGA.lim, envir = .GlobalEnv)
    
    # now look for the very first frame at which the hand velocity (GPvel) drops below the speed threshold, according to the formula
    # desired_velocity = min_velocity + (max_velocity - min_velocity) * proportion
    dataset <- ddply(dataset, .(trialN), mutate,
                     # find the speed threshold in this trial
                     TGAvel.lim = min(GPvel, na.rm = T) + (max(GPvel, na.rm = T) - min(GPvel, na.rm = T)) * TGAprop.lim,
                     # mark all the frames when the GPvel is smaller than the speed threshold
                     TGAcheck = ifelse(!is.na(GPvel), GPvel <= TGAvel.lim, F),
                     # mark all the frames when the GPvel drops below the threshold
                     TGAspot = (unlist(lapply(rle(TGAcheck)$lengths, seq_len))*TGAcheck)==1,
                     # mark the very first frame when the GPvel drops below the threshold
                     TGAspot2 = cumsum(TGAspot[!is.na(TGAspot)])*TGAspot == 1
    )
    
    # extract the TGA
    data.tga <- ddply(dataset, .(trialN), summarise,
                      
                      # Terminal Grip Aperture
                      frameToTGA = frameN[unique(which(TGAspot2==T))], 
                      TGA = GA[match(frameToTGA, frameN)],
                      timeToTGA = time[match(frameToTGA, frameN)],
                      
                      # Grip Orientation at TGA
                      GOf.TGA = GOF[match(frameToTGA, frameN)],
                      GOs.TGA= GOS[match(frameToTGA, frameN)],
                      GOt.TGA= GOT[match(frameToTGA, frameN)],
                      
                      # Grip Position at TGA
                      GPx.TGA=GPX[match(frameToTGA, frameN)],
                      GPy.TGA=GPY[match(frameToTGA, frameN)],
                      GPz.TGA=GPZ[match(frameToTGA, frameN)],
                      
                      # Index Position at TGA
                      IPx.TGA=indexX[match(frameToTGA, frameN)],
                      IPy.TGA=indexY[match(frameToTGA, frameN)],
                      IPz.TGA=indexZ[match(frameToTGA, frameN)],
                      
                      # Thumb Position at TGA
                      TPx.TGA=thumbX[match(frameToTGA, frameN)],
                      TPy.TGA=thumbY[match(frameToTGA, frameN)],
                      TPz.TGA=thumbZ[match(frameToTGA, frameN)]
    )
  } else if(TGA.type == "s")
  {
    assign("TGAsp.lim", TGA.lim, envir = .GlobalEnv)
    
    if(TGA.kin == "t")
    {
      dataset <- ddply(dataset, .(trialN), mutate,
                       # mark all the frames when the thumb is closer to its final position than the spatial threshold
                       TGAcheck = ifelse(!is.na(thumbX), sqrt((thumbX - data.final$FTPx[data.final$trialN==unique(trialN)])^2 + 
                                                                (thumbY - data.final$FTPy[data.final$trialN==unique(trialN)])^2 + 
                                                                (thumbZ - data.final$FTPz[data.final$trialN==unique(trialN)])^2) <= TGAsp.lim, F),
                       # mark all the frames when the GPvel drops below the threshold
                       TGAspot = (unlist(lapply(rle(TGAcheck)$lengths, seq_len))*TGAcheck)==1,
                       # mark the very first frame when the GPvel drops below the threshold
                       TGAspot2 = cumsum(TGAspot[!is.na(TGAspot)])*TGAspot == 1
      )
    } else if(TGA.kin == "g")
    {
      dataset <- ddply(dataset, .(trialN), mutate,
                       # mark all the frames when the thumb is closer to its final position than the spatial threshold
                       TGAcheck = ifelse(!is.na(GPX), sqrt((GPX - data.final$FGPx[data.final$trialN==unique(trialN)])^2 + 
                                                           (GPY - data.final$FGPy[data.final$trialN==unique(trialN)])^2 + 
                                                           (GPZ - data.final$FGPz[data.final$trialN==unique(trialN)])^2) <= TGAsp.lim, F),
                       # mark all the frames when the GPvel drops below the threshold
                       TGAspot = (unlist(lapply(rle(TGAcheck)$lengths, seq_len))*TGAcheck)==1,
                       # mark the very first frame when the GPvel drops below the threshold
                       TGAspot2 = cumsum(TGAspot[!is.na(TGAspot)])*TGAspot == 1
      )
    }
    
    # extract the TGA
    data.tga <- ddply(dataset, .(trialN), summarise,
                      
                      # Terminal Grip Aperture
                      frameToTGA = frameN[unique(which(TGAspot2==T))], 
                      TGA = GA[match(frameToTGA, frameN)],
                      timeToTGA = time[match(frameToTGA, frameN)],
                      
                      # Grip Orientation at TGA
                      GOf.TGA = GOF[match(frameToTGA, frameN)],
                      GOs.TGA= GOS[match(frameToTGA, frameN)],
                      GOt.TGA= GOT[match(frameToTGA, frameN)],
                      
                      # Grip Position at TGA
                      GPx.TGA=GPX[match(frameToTGA, frameN)],
                      GPy.TGA=GPY[match(frameToTGA, frameN)],
                      GPz.TGA=GPZ[match(frameToTGA, frameN)],
                      
                      # Index Position at TGA
                      IPx.TGA=indexX[match(frameToTGA, frameN)],
                      IPy.TGA=indexY[match(frameToTGA, frameN)],
                      IPz.TGA=indexZ[match(frameToTGA, frameN)],
                      
                      # Thumb Position at TGA
                      TPx.TGA=thumbX[match(frameToTGA, frameN)],
                      TPy.TGA=thumbY[match(frameToTGA, frameN)],
                      TPz.TGA=thumbZ[match(frameToTGA, frameN)]
    )
  }
  
  # merge datasets
  data.list <- list(data.final, data.MGV, data.MGACC, data.other, data.MGAvel, data.mga, data.tga)
  data.extract <- Reduce(function(...) merge(..., by="trialN", all=TRUE), data.list)
  
  # remove data.final from the global environment
  remove(data.final, envir = .GlobalEnv)
  remove(MGA.range, envir = .GlobalEnv)
  remove(data.mga, envir = .GlobalEnv)
  if(TGA.type == "v")
  {
    remove(TGAprop.lim, envir = .GlobalEnv)
  } else if(TGA.type == "s")
  {
    remove(TGAsp.lim, envir = .GlobalEnv)
  }
  
  # Deceleration Phase
  data.extract$decelPhase <- with(data.extract, timeToFGA - timeToMGV)
  data.extract$accelDecelRatio <- with(data.extract, timeToMGV/decelPhase)
  
  if(graphs)
  {
    suppressMessages(data.extract.melt <- melt(data.extract[-1], variable.name="kin"))
    variables <- aes(value)
    suppressMessages(print(ggplot(variables, data=data.extract.melt) + geom_histogram(color='black', aes(fill=kin)) + facet_wrap(~kin, scales="free") + theme_bw() + guides(fill=F)))
  }
  cat("Kinematics extracted.\n")
  return(round(data.extract, digits=3))
}