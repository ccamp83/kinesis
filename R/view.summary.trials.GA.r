#' @export
view.summary.trials.GA <- function(trials=NULL, datatraj, datakin, grid.size=20, frame=T, print=F, res=28, name="")
{
  datatraj$indexOccluded <- with(datatraj, ifelse(is.na(indexXraw), 1, 0))
  datatraj$thumbOccluded <- with(datatraj, ifelse(is.na(thumbXraw), 1, 0))
  
  if(is.null(trials))
  {
    trials <- unique(datatraj$trialN)
    num.trials <- length(trials)
  } else
  {
    num.trials <- length(trials)
  }
  
  if(print)
  {
    pdf(file=sprintf("%s_Trials_summary_GA.pdf", name), width=res, height=res)
  }
  
  for(i in 1:ceiling(num.trials/grid.size))
  {
    trials.temp <- trials[(1:grid.size)+(grid.size*(i-1))]
    datatraj.graph <- subset(datatraj, trialN %in% trials.temp[!is.na(trials.temp)])
    datakin.graph <- subset(datakin, trialN %in% trials.temp[!is.na(trials.temp)])
    
    if(!frame)
    {
      graph.fingers <- ggplot() + 
        geom_point(data=subset(datatraj.graph), aes(time, indexZ), size=4.5, color='red', na.rm=T) + 
        labs(title=sprintf("Trial summary %d/%d",i,ceiling(num.trials/grid.size))) + 
        geom_point(data=subset(datatraj.graph), aes(time, thumbZ), color='blue', size=3.5, na.rm=T) + 
        geom_point(data=subset(datatraj.graph), aes(time, GA), color='black', na.rm=T) + labs(x="time", y="fingers Z") +
        geom_point(data=subset(datatraj.graph, indexOccluded==1), aes(time, indexZ), size=4.5, color='palegoldenrod') + 
        geom_point(data=subset(datatraj.graph, thumbOccluded==1), aes(time, thumbZ), color='paleturquoise', size=3.5) + 
        geom_point(data=subset(datatraj.graph, fingersOccluded==1), aes(time, GA), color='yellow3') + 
        geom_point(data=subset(datakin.graph), aes(timeToMGA, MGA), color="red", size=9) +
        geom_point(data=subset(datakin.graph), aes(timeToFGA, FGA), color="green", size=6) +
        geom_point(data=subset(datakin.graph), aes(timeToTGA, TGA), color="cyan", size=6) +
        geom_point(data=subset(datakin.graph), aes(timeToMGA, IPz.MGA), color="red", size=10) +
        geom_point(data=subset(datakin.graph), aes(timeToMGA, TPz.MGA), color="blue", size=10) +
        geom_point(data=subset(datakin.graph), aes(timeToFGA, FIPz), color="green4", size=9) +
        geom_point(data=subset(datakin.graph), aes(timeToFGA, FTPz), color="yellow3", size=9) + 
        geom_point(data=subset(datakin.graph), aes(timeToTGA, IPz.TGA), color="cyan", size=9) +
        geom_point(data=subset(datakin.graph), aes(timeToTGA, TPz.TGA), color="cyan", size=9) + 
        facet_wrap(~trialN, scales="free") + theme_bw()
    } else
    {
      graph.fingers <- ggplot() + 
        geom_point(data=subset(datatraj.graph), aes(frameN, indexZ), size=4.5, color='red', na.rm=T) + 
        labs(title=sprintf("Trial summary %d/%d",i,ceiling(num.trials/grid.size))) + 
        geom_point(data=subset(datatraj.graph), aes(frameN, thumbZ), color='blue', size=3.5, na.rm=T) + 
        geom_point(data=subset(datatraj.graph), aes(frameN, GA), color='black', na.rm=T) + labs(x="frameN", y="fingers Z") +
        geom_point(data=subset(datatraj.graph, indexOccluded==1), aes(frameN, indexZ), size=4.5, color='palegoldenrod') + 
        geom_point(data=subset(datatraj.graph, thumbOccluded==1), aes(frameN, thumbZ), color='paleturquoise', size=3.5) + 
        geom_point(data=subset(datatraj.graph, fingersOccluded==1), aes(frameN, GA), color='yellow3') + 
        geom_point(data=subset(datakin.graph), aes(frameToMGA, MGA), color="red", size=9) +
        geom_point(data=subset(datakin.graph), aes(frameToFGA, FGA), color="green", size=6) +
        geom_point(data=subset(datakin.graph), aes(frameToTGA, TGA), color="cyan", size=6) +
        geom_point(data=subset(datakin.graph), aes(frameToMGA, IPz.MGA), color="red", size=10) +
        geom_point(data=subset(datakin.graph), aes(frameToMGA, TPz.MGA), color="blue", size=10) +
        geom_point(data=subset(datakin.graph), aes(frameToFGA, FIPz), color="green4", size=9) +
        geom_point(data=subset(datakin.graph), aes(frameToFGA, FTPz), color="yellow3", size=9) + 
        geom_point(data=subset(datakin.graph), aes(frameToTGA, IPz.TGA), color="cyan", size=9) +
        geom_point(data=subset(datakin.graph), aes(frameToTGA, TPz.TGA), color="cyan", size=9) + 
        facet_wrap(~trialN, scales="free") + theme_bw()      
    }
    
    print(graph.fingers)
  }
  
  if(print)
  {
    cat('Graphs saved in ', getwd(), '/', name, '_Trials_summary.pdf\n', sep='')
    dev.off()
  }
}