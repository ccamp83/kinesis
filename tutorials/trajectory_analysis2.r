options(scipen = 1, digits = 3)

libraries()
library(cowplot)

# PROCEDURE FROM GALLIVAN and CHAPMAN 2014

#### Prepare the dataset ####
# keep only useful columns
usefCols <- c("trialN","indexXraw","indexYraw","indexZraw","thumbXraw","thumbYraw","thumbZraw")
# check dataset
testData <- data.check.test(rtgData_bad[usefCols])
test

graspData <- NULL
for(tN in unique(testData$trialN))
{
  # trial
  testTrial <- subset(testData, trialN == tN)
  # signals analysis
  start <- c(267,-332,-11.5)
  end <- c(34.8,-53.4,-283)
  refreshRate <- 1/85
  return_threshold <- -100

  index.signal <- testTrial[,c("indexXraw","indexYraw","indexZraw")]
  thumb.signal <- testTrial[,c("thumbXraw","thumbYraw","thumbZraw")]
  indexData <- kin.signal.analysis(index.signal, "index", start, end, deltaTime = refreshRate)
  thumbData <- kin.signal.analysis(thumb.signal, "thumb", start, end, deltaTime = refreshRate)

  # merge back
  trialData <- cbind(testTrial[c("subjName","trialN","frameN","frameT","time")], indexData, thumbData)

  # crop out the inbound portion of trajectory
  returnVel_threshold <- -100
  # crop return
  trialData <- subset(trialData, thumbZvel > returnVel_threshold & indexZvel > returnVel_threshold)

  # set onset time
  onsetVel_threshold <- 100
  onsetFrame <- kin.find.onsetTime(trialData$thumbVel, onsetVel_threshold)
  # crop out trajectory before onset
  trialData <- subset(trialData, frameN >= onsetFrame)

  # set offset time based on vel,acc,jerk
  # calculate jerk
  trialData$indexJerk <- kin.sgFilter(kin.sgFilter(trialData$indexVel, m = 1, ts = refreshRate), p = 12, ts = refreshRate)
  trialData$thumbJerk <- kin.sgFilter(kin.sgFilter(trialData$thumbVel, m = 1, ts = refreshRate), p = 12, ts = refreshRate)
  # resultant vectors
  trialData$thumbVelAccJerk.res <- with(trialData, sqrt(thumbVel^2 + thumbAcc^2 + thumbJerk^2))
  trialData$indexVelAccJerk.res <- with(trialData, sqrt(thumbVel^2 + thumbAcc^2 + thumbJerk^2))
  trialData$index_thumbVelAccJerk.res <- with(trialData, sqrt(thumbVelAccJerk.res^2 + indexVelAccJerk.res^2))
  # find offest time
  # theoretical z distance
  ztheor <- round(kin.rotate.trajectory(as.data.frame(t(end-start)), end-start), 2)[3]
  # we look for the offset in the second half of the trajectory
  offsetFrame <- with(subset(trialData, thumbZ > ztheor/2), frameN[match(kin.min(index_thumbVelAccJerk.res), index_thumbVelAccJerk.res)])
  # crop out trajectory after offset
  trialData <- subset(trialData, frameN <= offsetFrame)

  # space normalization
  trialData$thuDist <- sqrt((trialData$thumbX - tail(trialData$thumbX, 1))^2 +
                              (trialData$thumbY - tail(trialData$thumbY, 1))^2 +
                              (trialData$thumbZ - tail(trialData$thumbZ, 1))^2
  )
  # bin that distance
  binN <- 100
  trialData$thuDistB <- with(trialData, cut(thuDist, breaks = binN, labels = F))

  graspData <- rbind(graspData, trialData)
}

unique(graspData$trialN)

ggplot(data = graspData) +
  geom_point(aes(thuDist, indexZ), color = "red") +
  geom_point(aes(thuDist, thumbZ), color = "blue") +
  coord_fixed()


