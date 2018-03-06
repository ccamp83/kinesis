#' View single trial with Final Kinematics
#' @export
view.single.trial.GA <- function(tr, datatraj, datakin, details=F, frame=T)
{

  datatraj$indexOccluded <- with(datatraj, ifelse(is.na(indexXraw), 1, 0))
  datatraj$thumbOccluded <- with(datatraj, ifelse(is.na(thumbXraw), 1, 0))
  
  if(frame==F)
  {
    if(details)
    {
      
      #### first panel: overview of GA and its related kinematics
      
      # GA
      graph.GA <- ggplot() + geom_point(data=subset(datatraj, trialN==tr), aes(time, GA), size=4.5, na.rm=T) + 
        labs(title=sprintf("trial %d",tr), x=NULL, y="Grip Aperture") +
        geom_point(data=subset(datatraj, trialN==tr & fingersOccluded==1), aes(time, GA), size=4.5, color='yellow3') + 
        geom_point(data=subset(datakin, trialN==tr), aes(timeToMGA, MGA), color="red", size=9) +
        geom_point(data=subset(datakin, trialN==tr), aes(timeToFGA, FGA), color="green", size=6) +
        theme(axis.text.x=element_blank(), 
              axis.title.x=element_blank(),
              axis.ticks.x=element_blank()
        ) + theme_bw()
      
      # GAvel
      graph.GAvel <- ggplot() + geom_line(data=subset(datatraj, trialN==tr), aes(time, GAvel), color='brown', size=2, na.rm=T)  + 
        geom_point(data=subset(datakin, trialN==tr), aes(timeToMGAvel, MGAvel), color="darkgreen", size=6) +
        labs(x=NULL, y="Grip Aperture Velocity") + theme(axis.text.x=element_blank(), 
                                                         axis.title.x=element_blank(),
                                                         plot.title=element_blank(),
                                                         axis.ticks.x=element_blank()
        ) + theme_bw()
      
      # GAacc
      graph.GAacc <- ggplot() + geom_line(data=subset(datatraj, trialN==tr), aes(time, GAacc), color='darkmagenta', size=2, na.rm=T)  + 
        geom_point(data=subset(datakin, trialN==tr), aes(timeToMGAacc, MGAacc), color="darkgreen", size=6) +
        labs(x=NULL, y="Grip Aperture Acceleration") + theme(axis.text.x=element_blank(), 
                                                             axis.title.x=element_blank(),
                                                             plot.title=element_blank(),
                                                             axis.ticks.x=element_blank()
        ) + theme_bw()
      
      # putting them together
      GA <- ggplot_gtable(ggplot_build(graph.GA))
      GAvel <- ggplot_gtable(ggplot_build(graph.GAvel))
      GAacc <- ggplot_gtable(ggplot_build(graph.GAacc))
      maxWidth = unit.pmax(GA$widths[2:3], GAvel$widths[2:3], GAacc$widths[2:3])
      GA$widths[2:3] <- maxWidth
      GAvel$widths[2:3] <- maxWidth
      GAacc$widths[2:3] <- maxWidth
      
      #### second panel: fingers movement and related kinematics
      
      graph.fingers <- ggplot() + geom_point(data=subset(datatraj, trialN==tr), aes(time, indexZ), size=4.5, color='red', na.rm=T) + 
        geom_point(data=subset(datatraj, trialN==tr), aes(time, thumbZ), color='blue', size=3.5, na.rm=T) + 
        geom_point(data=subset(datatraj, trialN==tr), aes(time, GA), color='black', na.rm=T) + 
        geom_point(data=subset(datatraj, trialN==tr & indexOccluded==1), aes(time, indexZ), size=4.5, color='palegoldenrod') + 
        geom_point(data=subset(datatraj, trialN==tr & thumbOccluded==1), aes(time, thumbZ), color='paleturquoise', size=3.5) + 
        geom_point(data=subset(datatraj, trialN==tr & fingersOccluded==1), aes(time, GA), color='yellow3') + 
        geom_point(data=subset(datakin, trialN==tr), aes(timeToMGA, MGA), color="red", size=9) +
        geom_point(data=subset(datakin, trialN==tr), aes(timeToFGA, FGA), color="green", size=6) +
        geom_point(data=subset(datakin, trialN==tr), aes(timeToTGA, TGA), color="cyan", size=6) +
        geom_point(data=subset(datakin, trialN==tr), aes(timeToMGA, IPz.MGA), color="red", size=10) +
        geom_point(data=subset(datakin, trialN==tr), aes(timeToMGA, TPz.MGA), color="blue", size=10) +
        geom_point(data=subset(datakin, trialN==tr), aes(timeToFGA, FIPz), color="green4", size=9) +
        geom_point(data=subset(datakin, trialN==tr), aes(timeToFGA, FTPz), color="yellow3", size=9) +
        geom_point(data=subset(datakin, trialN==tr), aes(timeToTGA, IPz.TGA), color="cyan", size=9) +
        geom_point(data=subset(datakin, trialN==tr), aes(timeToTGA, TPz.TGA), color="cyan", size=9) + 
        labs(title=sprintf("trial %d",tr), x=NULL, y="Fingers Z Position") + 
        theme(axis.text.x=element_blank(), 
              axis.title.x=element_blank(),
              axis.ticks.x=element_blank()
        ) + theme_bw()
      
      # GPvel
      graph.GPvel <- ggplot() + geom_line(data=subset(datatraj, trialN==tr), aes(time, GPvel), color='brown', size=2, na.rm=T)  + 
        geom_point(data=subset(datakin, trialN==tr), aes(timeToMGV, MGV), color="darkgreen", size=6) +
        labs(x=NULL, y="Grip Position Velocity") + theme(axis.text.x=element_blank(), 
                                                         axis.title.x=element_blank(),
                                                         plot.title=element_blank(),
                                                         axis.ticks.x=element_blank()
        ) + theme_bw()
      
      # GPacc
      graph.GPacc <- ggplot() + geom_line(data=subset(datatraj, trialN==tr), aes(time, GPacc), color='darkmagenta', size=2, na.rm=T)  + 
        geom_point(data=subset(datakin, trialN==tr), aes(timeToMGACC, MGACC), color="darkgreen", size=6) +
        labs(x=NULL, y="Grip Position Acceleration") + theme(axis.text.x=element_blank(), 
                                                             axis.title.x=element_blank(),
                                                             plot.title=element_blank(),
                                                             axis.ticks.x=element_blank()
        ) + theme_bw()
      
      # putting them together
      fingers <- ggplot_gtable(ggplot_build(graph.fingers))
      GPvel <- ggplot_gtable(ggplot_build(graph.GPvel))
      GPacc <- ggplot_gtable(ggplot_build(graph.GPacc))
      maxWidth = unit.pmax(fingers$widths[2:3], GPvel$widths[2:3], GPacc$widths[2:3])
      fingers$widths[2:3] <- maxWidth
      GPvel$widths[2:3] <- maxWidth
      GPacc$widths[2:3] <- maxWidth
      
      return(grid.arrange(fingers, GA, GPvel, GAvel, GPacc, GAacc, ncol=2, nrow=3))
      
    }else
    {
      graph.fingers <- ggplot() + 
        geom_point(data=subset(datatraj, trialN==tr), aes(time, indexZ), size=4.5, color='red', na.rm=T) + 
        geom_point(data=subset(datatraj, trialN==tr), aes(time, thumbZ), color='blue', size=3.5, na.rm=T) + 
        geom_point(data=subset(datatraj, trialN==tr), aes(time, GA), color='black', na.rm=T) + labs(x="time", y="fingers Z") +
        geom_point(data=subset(datatraj, trialN==tr & indexOccluded==1), aes(time, indexZ), size=4.5, color='palegoldenrod') + 
        geom_point(data=subset(datatraj, trialN==tr & thumbOccluded==1), aes(time, thumbZ), color='paleturquoise', size=3.5) + 
        geom_point(data=subset(datatraj, trialN==tr & fingersOccluded==1), aes(time, GA), color='yellow3') + 
        geom_point(data=subset(datakin, trialN==tr), aes(timeToMGA, MGA), color="red", size=9) +
        geom_point(data=subset(datakin, trialN==tr), aes(timeToFGA, FGA), color="green", size=6) +
        geom_point(data=subset(datakin, trialN==tr), aes(timeToTGA, TGA), color="cyan", size=6) +
        geom_point(data=subset(datakin, trialN==tr), aes(timeToMGA, IPz.MGA), color="red", size=10) +
        geom_point(data=subset(datakin, trialN==tr), aes(timeToMGA, TPz.MGA), color="blue", size=10) +
        geom_point(data=subset(datakin, trialN==tr), aes(timeToFGA, FIPz), color="green4", size=9) +
        geom_point(data=subset(datakin, trialN==tr), aes(timeToFGA, FTPz), color="yellow3", size=9) + 
        geom_point(data=subset(datakin, trialN==tr), aes(timeToTGA, IPz.TGA), color="cyan", size=9) +
        geom_point(data=subset(datakin, trialN==tr), aes(timeToTGA, TPz.TGA), color="cyan", size=9) + 
        labs(title=sprintf("trial %d",tr), x=NULL, y="Fingers Z Position") + theme_bw()
      
      return(graph.fingers)
    }
  }else
  {
    # debug lines
    # datatraj <- allData
    # datakin <- kinData
    # tr <- 1
    if(details)
    {
      
      #### first panel: overview of GA and its related kinematics
      
      # GA
      graph.GA <- ggplot() + geom_point(data=subset(datatraj, trialN==tr), aes(frameN, GA), size=4.5, na.rm=T) + 
        labs(title=sprintf("trial %d",tr), x=NULL, y="Grip Aperture") +
        geom_point(data=subset(datatraj, trialN==tr & fingersOccluded==1), aes(frameN, GA), size=4.5, color='yellow3') + 
        geom_point(data=subset(datakin, trialN==tr), aes(frameToMGA, MGA), color="red", size=9) +
        geom_point(data=subset(datakin, trialN==tr), aes(frameToFGA, FGA), color="green", size=6) +
        theme(axis.text.x=element_blank(), 
              axis.title.x=element_blank(),
              axis.ticks.x=element_blank()
        ) + theme_bw()
      
      # GAvel
      graph.GAvel <- ggplot() + geom_line(data=subset(datatraj, trialN==tr), aes(frameN, GAvel), color='brown', size=2, na.rm=T)  + 
        geom_point(data=subset(datakin, trialN==tr), aes(frameToMGAvel, MGAvel), color="darkgreen", size=6) +
        labs(x=NULL, y="Grip Aperture Velocity") + theme(axis.text.x=element_blank(), 
                                                         axis.title.x=element_blank(),
                                                         plot.title=element_blank(),
                                                         axis.ticks.x=element_blank()
        ) + theme_bw()
      
      # GAacc
      graph.GAacc <- ggplot() + geom_line(data=subset(datatraj, trialN==tr), aes(frameN, GAacc), color='darkmagenta', size=2, na.rm=T)  + 
        geom_point(data=subset(datakin, trialN==tr), aes(frameToMGAacc, MGAacc), color="darkgreen", size=6) +
        labs(x=NULL, y="Grip Aperture Acceleration") + theme(axis.text.x=element_blank(), 
                                                             axis.title.x=element_blank(),
                                                             plot.title=element_blank(),
                                                             axis.ticks.x=element_blank()
        ) + theme_bw()
      
      # putting them together
      GA <- ggplot_gtable(ggplot_build(graph.GA))
      GAvel <- ggplot_gtable(ggplot_build(graph.GAvel))
      GAacc <- ggplot_gtable(ggplot_build(graph.GAacc))
      maxWidth = unit.pmax(GA$widths[2:3], GAvel$widths[2:3], GAacc$widths[2:3])
      GA$widths[2:3] <- maxWidth
      GAvel$widths[2:3] <- maxWidth
      GAacc$widths[2:3] <- maxWidth
      
      #### second panel: fingers movement and related kinematics
      
      graph.fingers <- ggplot() + geom_point(data=subset(datatraj, trialN==tr), aes(frameN, indexZ), size=4.5, color='red', na.rm=T) + 
        geom_point(data=subset(datatraj, trialN==tr), aes(frameN, thumbZ), color='blue', size=3.5, na.rm=T) + 
        geom_point(data=subset(datatraj, trialN==tr), aes(frameN, GA), color='black', na.rm=T) + 
        geom_point(data=subset(datatraj, trialN==tr & indexOccluded==1), aes(frameN, indexZ), size=4.5, color='palegoldenrod') + 
        geom_point(data=subset(datatraj, trialN==tr & thumbOccluded==1), aes(frameN, thumbZ), color='paleturquoise', size=3.5) + 
        geom_point(data=subset(datatraj, trialN==tr & fingersOccluded==1), aes(frameN, GA), color='yellow3') + 
        geom_point(data=subset(datakin, trialN==tr), aes(frameToMGA, MGA), color="red", size=9) +
        geom_point(data=subset(datakin, trialN==tr), aes(frameToFGA, FGA), color="green", size=6) +
        geom_point(data=subset(datakin, trialN==tr), aes(frameToTGA, TGA), color="cyan", size=6) +
        geom_point(data=subset(datakin, trialN==tr), aes(frameToMGA, IPz.MGA), color="red", size=10) +
        geom_point(data=subset(datakin, trialN==tr), aes(frameToMGA, TPz.MGA), color="blue", size=10) +
        geom_point(data=subset(datakin, trialN==tr), aes(frameToFGA, FIPz), color="green4", size=9) +
        geom_point(data=subset(datakin, trialN==tr), aes(frameToFGA, FTPz), color="yellow3", size=9) +
        geom_point(data=subset(datakin, trialN==tr), aes(frameToTGA, IPz.TGA), color="cyan", size=9) +
        geom_point(data=subset(datakin, trialN==tr), aes(frameToTGA, TPz.TGA), color="cyan", size=9) +
        labs(title=sprintf("trial %d",tr), x=NULL, y="Fingers Z Position") + 
        theme(axis.text.x=element_blank(), 
              axis.title.x=element_blank(),
              axis.ticks.x=element_blank()
        ) + theme_bw()
      
      # GPvel
      graph.GPvel <- ggplot() + geom_line(data=subset(datatraj, trialN==tr), aes(frameN, GPvel), color='brown', size=2, na.rm=T)  + 
        geom_point(data=subset(datakin, trialN==tr), aes(frameToMGV, MGV), color="darkgreen", size=6) +
        labs(x=NULL, y="Grip Position Velocity") + theme(axis.text.x=element_blank(), 
                                                         axis.title.x=element_blank(),
                                                         plot.title=element_blank(),
                                                         axis.ticks.x=element_blank()
        ) + theme_bw()
      
      # GPacc
      graph.GPacc <- ggplot() + geom_line(data=subset(datatraj, trialN==tr), aes(frameN, GPacc), color='darkmagenta', size=2, na.rm=T)  + 
        geom_point(data=subset(datakin, trialN==tr), aes(frameToMGACC, MGACC), color="darkgreen", size=6) +
        labs(x=NULL, y="Grip Position Acceleration") + theme(axis.text.x=element_blank(), 
                                                             axis.title.x=element_blank(),
                                                             plot.title=element_blank(),
                                                             axis.ticks.x=element_blank()
        ) + theme_bw()
      
      # putting them together
      fingers <- ggplot_gtable(ggplot_build(graph.fingers))
      GPvel <- ggplot_gtable(ggplot_build(graph.GPvel))
      GPacc <- ggplot_gtable(ggplot_build(graph.GPacc))
      maxWidth = unit.pmax(fingers$widths[2:3], GPvel$widths[2:3], GPacc$widths[2:3])
      fingers$widths[2:3] <- maxWidth
      GPvel$widths[2:3] <- maxWidth
      GPacc$widths[2:3] <- maxWidth
      
      return(grid.arrange(fingers, GA, GPvel, GAvel, GPacc, GAacc, ncol=2, nrow=3))
      
    }else
    {
      graph.fingers <- ggplot() + 
        geom_point(data=subset(datatraj, trialN==tr), aes(frameN, indexZ), size=4.5, color='red', na.rm=T) + 
        geom_point(data=subset(datatraj, trialN==tr), aes(frameN, thumbZ), color='blue', size=3.5, na.rm=T) + 
        geom_point(data=subset(datatraj, trialN==tr), aes(frameN, GA), color='black', na.rm=T) + labs(x="frame", y="fingers Z") +
        geom_point(data=subset(datatraj, trialN==tr & indexOccluded==1), aes(frameN, indexZ), size=4.5, color='palegoldenrod') + 
        geom_point(data=subset(datatraj, trialN==tr & thumbOccluded==1), aes(frameN, thumbZ), color='paleturquoise', size=3.5) + 
        geom_point(data=subset(datatraj, trialN==tr & fingersOccluded==1), aes(frameN, GA), color='yellow3') + 
        geom_point(data=subset(datakin, trialN==tr), aes(frameToMGA, MGA), color="red", size=9) +
        geom_point(data=subset(datakin, trialN==tr), aes(frameToFGA, FGA), color="green", size=6) +
        geom_point(data=subset(datakin, trialN==tr), aes(frameToTGA, TGA), color="cyan", size=6) +
        geom_point(data=subset(datakin, trialN==tr), aes(frameToMGA, IPz.MGA), color="red", size=10) +
        geom_point(data=subset(datakin, trialN==tr), aes(frameToMGA, TPz.MGA), color="blue", size=10) +
        geom_point(data=subset(datakin, trialN==tr), aes(frameToFGA, FIPz), color="green4", size=9) +
        geom_point(data=subset(datakin, trialN==tr), aes(frameToFGA, FTPz), color="yellow3", size=9) +
        geom_point(data=subset(datakin, trialN==tr), aes(frameToTGA, IPz.TGA), color="cyan", size=9) +
        geom_point(data=subset(datakin, trialN==tr), aes(frameToTGA, TPz.TGA), color="cyan", size=9) +
        labs(title=sprintf("trial %d",tr), x=NULL, y="Fingers Z Position") + theme_bw()
      
      return(graph.fingers)
    }
  }
}