#' View single trial
#' @export
view.single.trial <- function(tr, dataset, details=F)
{
  
  dataset$indexOccluded <- with(dataset, ifelse(is.na(indexXraw), 1, 0))
  dataset$thumbOccluded <- with(dataset, ifelse(is.na(thumbXraw), 1, 0))
  
  if(details)
  {
    
    #### first panel: overview of GA and its related kinematics
    
    # GA
    graph.GA <- ggplot() + 
      geom_point(data=subset(dataset, trialN==tr), aes(time, GA), size=4.5, na.rm=T) + 
      geom_point(data=subset(dataset, trialN==tr & fingersOccluded==1), aes(time, GA), size=4.5, color='yellow3') + 
      labs(title=sprintf("trial %d",tr), x=NULL, y="Grip Aperture") +
      theme(axis.text.x=element_blank(), 
            axis.title.x=element_blank(),
            axis.ticks.x=element_blank()
      ) + theme_bw()
    
    # GAvel
    graph.GAvel <- ggplot() + geom_line(data=subset(dataset, trialN==tr), aes(time, GAvel), color='brown', size=2, na.rm=T)  + 
      labs(x=NULL, y="Grip Aperture Velocity") + theme(axis.text.x=element_blank(), 
                           axis.title.x=element_blank(),
                           plot.title=element_blank(),
                           axis.ticks.x=element_blank()
      ) + theme_bw()
    
    # GAacc
    graph.GAacc <- ggplot() + geom_line(data=subset(dataset, trialN==tr), aes(time, GAacc), color='darkmagenta', size=2, na.rm=T)  + 
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
    
    graph.fingers <- ggplot() + 
      geom_point(data=subset(dataset, trialN==tr), aes(time, indexZ), size=4.5, color='red', na.rm=T) + 
      geom_point(data=subset(dataset, trialN==tr & indexOccluded==1), aes(time, indexZ), size=4.5, color='palegoldenrod') + 
      labs(title=sprintf("trial %d",tr), x=NULL, y="Fingers Z Position") + 
      geom_point(data=subset(dataset, trialN==tr), aes(time, thumbZ), color='blue', size=3.5, na.rm=T) + 
      geom_point(data=subset(dataset, trialN==tr & thumbOccluded==1), aes(time, thumbZ), color='paleturquoise', size=3.5) + 
      geom_point(data=subset(dataset, trialN==tr), aes(time, GA), color='black', na.rm=T) + 
      geom_point(data=subset(dataset, trialN==tr & fingersOccluded==1), aes(time, GA), color='yellow3') + 
      theme(axis.text.x=element_blank(), 
            axis.title.x=element_blank(),
            axis.ticks.x=element_blank()
      ) + theme_bw() 
    
    # GPvel
    graph.GPvel <- ggplot() + geom_line(data=subset(dataset, trialN==tr), aes(time, GPvel), color='brown', size=2, na.rm=T)  + 
      labs(x=NULL, y="Grip Position Velocity") + theme(axis.text.x=element_blank(), 
                                                       axis.title.x=element_blank(),
                                                       plot.title=element_blank(),
                                                       axis.ticks.x=element_blank()
      ) + theme_bw()
    
    # GPacc
    graph.GPacc <- ggplot() + geom_line(data=subset(dataset, trialN==tr), aes(time, GPacc), color='darkmagenta', size=2, na.rm=T)  + 
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
    graph.fingers <- ggplot() + geom_point(data=subset(dataset, trialN==tr), aes(time, indexZ), size=4.5, color='red', na.rm=T) + 
      geom_point(data=subset(dataset, trialN==tr & indexOccluded==1), aes(time, indexZ), size=4.5, color='palegoldenrod', na.rm=T) + 
      labs(title=sprintf("trial %d",tr), x=NULL, y="Fingers Z Position") + 
      geom_point(data=subset(dataset, trialN==tr), aes(time, thumbZ), color='blue', size=3.5, na.rm=T) + 
      geom_point(data=subset(dataset, trialN==tr & thumbOccluded==1), aes(time, thumbZ), color='paleturquoise', size=3.5, na.rm=T) + 
      geom_point(data=subset(dataset, trialN==tr), aes(time, GA), color='black', na.rm=T) + 
      geom_point(data=subset(dataset, trialN==tr & fingersOccluded==1), aes(time, GA), color='yellow3') + theme_bw()
    
    return(graph.fingers)
  }  
}