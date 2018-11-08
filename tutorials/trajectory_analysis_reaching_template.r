library(kinesis)
library(cowplot)
options(scipen = 1, digits = 3)

#### Prepare the dataset ####
# five columns are expected
get("dataCols", kinesis_parameters)
# user can set them
kin.setDataCols(deltaTime = "refreshRate")
get("dataCols", kinesis_parameters)
# Fix dataset
testData <- data.check(reachData)

#### main dataset
trajData <- NULL
reach_paramData <- NULL
timeinfoData <- NULL

#### Analysis loop ####
trialsList <- unique(testData$trialN)
badTrials <- NULL
for(tN in trialsList)
{
  # tN <- trialsList[1]
  cat("---- trial #", tN, ". ----\n\n", sep = "")
  #### select trial ----
  testTrial.backup <- subset(testData, trialN == tN)

  columnsToExclude <- paste0("hand",c("X","Y","Z"),"raw")
  testTrial <- testTrial.backup[, !names(testTrial.backup)%in%columnsToExclude]
  testTrial$handXraw <- testTrial$handX
  testTrial$handZraw <- testTrial$handZ

  #### signals analysis ----
  # set start x y z coordinates ----
  start <- c(0,0,0)
  # set end (depends on trial's conditions) ----
  end <- c(0,0,.1)
  # refresh rate ----
  refreshRate <- 1/85
  # prepare signals datasets
  hand.signal <- testTrial[,c("handXraw","handZraw")]
  # analysis: repair, filter, translate, rotate ----
  handData <- kin.signal.analysis(hand.signal, "hand", start, end, deltaTime = refreshRate, f = F)

  #### merge back ----
  trialData <- cbind(testTrial[c("subjName","trialN","frameN","refreshRate","time","handXraw","handZraw")], handData)

  if(mean(trialData$handVel)>0.001)
  {
    #### onset time ----
    # set velocity threshold ----
    onsetVel_threshold <- .02
    # find onset frame ----
    onsetFramePos <- kin.find.traj.landmark("trialData$handZvel > onsetVel_threshold")
    # crop trajectory before onset ----
    trialData <- subset(trialData, frameN >= frameN[onsetFramePos])

    #### offset time ----
    # set velocity threshold ----
    returnVel_threshold <- -.02
    # find the frame where this happens
    offsetFramePos <- kin.find.traj.landmark("trialData$handZvel < returnVel_threshold")
    # crop
    trialData <- subset(trialData, frameN < frameN[offsetFramePos])

    # ggplot(aes(frameN, handZ), data = trialData) +
    #   geom_point()

    #### space normalization ----
    # euclidean distance of hand to its final position
    trialData$handDist <- sqrt((trialData$handX - tail(trialData$handX, 1))^2 +
                                (trialData$handY - tail(trialData$handY, 1))^2 +
                                (trialData$handZ - tail(trialData$handZ, 1))^2
    )
    # bin thuDist
    binN <- 100
    trialData$handDistB <- with(trialData, cut(handDist, breaks = binN, labels = F))

    #### append trajectory data to main trajectory dataset ----
    trajData <- rbind(trajData, trialData)

    #### extract parameters ----
    extractData <- kin.extract.parameters(trialData, c("hand"), grasp = F)

    #### append reach parameters to main dataset ----
    trialParams.r <- extractData$reach_parameters
    trialParams.r$trialN <- tN
    reach_paramData <- rbind(reach_paramData, trialParams.r)

    #### append time info to main dataset ----
    timeinfoParams <- extractData$time_info
    timeinfoParams$trialN <- tN
    timeinfoData <- rbind(timeinfoData, timeinfoParams)
  } else
  {
    badTrials <- c(badTrials, tN)
  }
}

head(reach_paramData)
head(timeinfoData)
badTrials

#### Results ####
ggplot(data = trajData) +
  geom_point(aes(handX, handZ)) +
  coord_fixed()

ggplot(data = trajData) +
  geom_point(aes(handXraw, handZraw)) +
  coord_fixed()

ggplot(data = trajData) +
  geom_point(aes(-handDist, handZ), color = "red")

ggplot(aes(trialN, movTime), data = timeinfoData) + geom_point()

ggplot(aes(trialN, MdeviationX, color = signal), data = reach_paramData) + geom_point() + geom_smooth(method = lm)
ggplot(aes(trialN, MdeviationY, color = signal), data = reach_paramData) + geom_point() + geom_smooth(method = lm)
