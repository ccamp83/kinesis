options(scipen = 1, digits = 3)

library(cowplot)

#### Prepare the dataset ####
# keep only useful columns
usefCols <- c("trialN","indexXraw","indexYraw","indexZraw","thumbXraw","thumbYraw","thumbZraw")
# check dataset
testData <- data.check(rtgData_bad[usefCols])
test

#### main dataset
trajData <- NULL
reach_paramData <- NULL
grasp_paramData <- NULL
timeinfoData <- NULL

#### Analysis loop ####
for(tN in unique(testData$trialN))
{
  # tN <- unique(testData$trialN)[1]
  cat("---- trial #", tN, ". ----\n\n")
  #### select trial
  testTrial <- subset(testData, trialN == tN)

  #### signals analysis
  # set start
  start <- c(267,-332,-11.5)
  # set end (depends on trial's conditions)
  kmData <- testTrial[c("thumbXraw","thumbYraw","thumbZraw","time")]
  km.res <- as.data.frame(kmeans(kmData, 2)$centers)
  km.res$moment <- with(km.res, ifelse(time == min(time), "start", "end"))
  end <- as.numeric(km.res[km.res$moment=="end",1:3])
  # refresh rate
  refreshRate <- 1/85
  # prepare signals datasets
  index.signal <- testTrial[,c("indexXraw","indexYraw","indexZraw")]
  thumb.signal <- testTrial[,c("thumbXraw","thumbYraw","thumbZraw")]
  # analysis: repair, filter, translate, rotate
  indexData <- kin.signal.analysis(index.signal, "index", start, end, deltaTime = refreshRate)
  thumbData <- kin.signal.analysis(thumb.signal, "thumb", start, end, deltaTime = refreshRate)

  #### merge back
  trialData <- cbind(testTrial[c("subjName","trialN","frameN","deltaTime","time")], indexData, thumbData)

  #### crop the inbound portion of trajectory
  # set velocity threshold
  returnVel_threshold <- -100
  # crop
  trialData <- subset(trialData, thumbZvel > returnVel_threshold & indexZvel > returnVel_threshold)

  #### onset time
  # set velocity threshold
  onsetVel_threshold <- 100
  # find onset frame
  onsetFrame <- kin.find.onsetTime(trialData$thumbVel, onsetVel_threshold)
  # crop trajectory before onset
  trialData <- subset(trialData, frameN >= onsetFrame)

  #### offset time
  # heuristic based on resultant vector of vel,acc,jerk
  # calculate jerk
  trialData$indexJerk <- kin.sgFilter(kin.sgFilter(trialData$indexVel, m = 1, ts = refreshRate), p = 12, ts = refreshRate)
  trialData$thumbJerk <- kin.sgFilter(kin.sgFilter(trialData$thumbVel, m = 1, ts = refreshRate), p = 12, ts = refreshRate)
  # resultant vectors
  trialData$thumbVelAccJerk.res <- with(trialData, sqrt(thumbVel^2 + thumbAcc^2 + thumbJerk^2))
  trialData$indexVelAccJerk.res <- with(trialData, sqrt(thumbVel^2 + thumbAcc^2 + thumbJerk^2))
  trialData$index_thumbVelAccJerk.res <- with(trialData, sqrt(thumbVelAccJerk.res^2 + indexVelAccJerk.res^2))
  # z distance after rotation
  ztheor <- round(kin.rotate.trajectory(as.data.frame(t(end-start)), end-start), 2)[3]
  # we look for the offset frame in the second half of the trajectory
  offsetFrame <- with(subset(trialData, thumbZ > ztheor/2),
                      frameN[match(kin.min(index_thumbVelAccJerk.res), index_thumbVelAccJerk.res)])
  # crop trajectory after offset
  trialData <- subset(trialData, frameN <= offsetFrame)

  #### space normalization
  # euclidean distance of thumb to its final position
  trialData$thuDist <- sqrt((trialData$thumbX - tail(trialData$thumbX, 1))^2 +
                              (trialData$thumbY - tail(trialData$thumbY, 1))^2 +
                              (trialData$thumbZ - tail(trialData$thumbZ, 1))^2
  )
  # bin thuDist
  binN <- 100
  trialData$thuDistB <- with(trialData, cut(thuDist, breaks = binN, labels = F))

  #### grasp analysis
  graspData <- kin.grasp.analysis(data = trialData, signals = c("index","thumb"), deltaTime = refreshRate)

  #### merge reaching and grasp analysis
  trialData <- cbind(trialData, graspData)

  #### other specs
  trialData$objectZ <- ifelse(end[3] > -300, 270, 350)

  #### append trajectory data to main trajectory dataset
  trajData <- rbind(trajData, trialData)

  #### extract parameters
  extractData <- kin.extract.parameters(trialData, c("index","thumb"), grasp = T)

  #### append reach parameters to main dataset
  trialParams.r <- extractData$reach_parameters
  trialParams.r$trialN <- tN
  reach_paramData <- rbind(reach_paramData, trialParams.r)

  #### append reach parameters to main dataset
  trialParams.g <- extractData$grasp_parameters
  trialParams.g$trialN <- tN
  grasp_paramData <- rbind(grasp_paramData, trialParams.g)

  #### append time info to main dataset
  timeinfoParams <- extractData$time_info
  timeinfoParams$trialN <- tN
  timeinfoData <- rbind(timeinfoData, timeinfoParams)
}

head(reach_paramData)
head(grasp_paramData)
head(timeinfoData)

#### Results ####
unique(trajData$trialN)

ggplot(data = trajData) +
  geom_point(aes(indexX, indexZ), color = "red") +
  geom_point(aes(thumbX, thumbZ), color = "blue") +
  facet_grid(. ~ objectZ) +
  coord_fixed()

ggplot(data = trajData) +
  geom_point(aes(-thuDist, indexZ), color = "red") +
  geom_point(aes(-thuDist, thumbZ), color = "blue") +
  facet_grid(. ~ objectZ)

ggplot(aes(trialN, movTime), data = timeinfoData) + geom_point()

ggplot(aes(trialN, MdeviationX, color = signal), data = reach_paramData) + geom_point() + geom_smooth(method = lm)
ggplot(aes(trialN, MdeviationY, color = signal), data = reach_paramData) + geom_point() + geom_smooth(method = lm)

ggplot(data=trajData) +
  geom_point(aes(time, GOF*180/pi, color = objectZ))

ggplot(data=trajData) +
  geom_point(aes(time, GOT*180/pi, color = objectZ))

ggplot(data=trajData) +
  geom_point(aes(time, GOS*180/pi, color = objectZ))
