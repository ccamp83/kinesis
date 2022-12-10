library(kinesis)
mu::mu.library("cowplot")
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
  hand.signal <- testTrial[,c("handXraw","handZraw","time")] # requires 3D positional (raw) data and time column ONLY
  # analysis: repair, filter, translate, rotate ----
  handData <- kin.signal.analysis(hand.signal, "hand", start, end, f = F)
  if(!any(is.na(handData)))
  {

    #### merge back ----
    trialData <- cbind(testTrial[c("subjName","trialN","frameN","refreshRate","handXraw","handZraw")], handData)

    #### onset time ----
    # set velocity threshold ----
    onsetVel_threshold <- .02
    onset_condition <- "trialData$handZvel > onsetVel_threshold"
    # find onset frame ----
    onsetFramePos <- kin.find.traj.landmark(onset_condition)

    if(!is.na(onsetFramePos))
    {
      # crop trajectory before onset ----
      trialData <- subset(trialData, frameN >= frameN[onsetFramePos])

      #### offset time ----
      # set velocity threshold ----
      returnVel_threshold <- -.02
      offset_condition <- "trialData$handZvel < returnVel_threshold"
      # find the frame where this happens
      offsetFramePos <- kin.find.traj.landmark(offset_condition)

      if(!is.na(offsetFramePos))
      {
        # crop
        trialData <- subset(trialData, frameN < frameN[offsetFramePos])

        # ggplot(aes(frameN, handZ), data = trialData) +
        #   geom_point()

        #### append trajectory data to main trajectory dataset ----
        trajData <- rbind(trajData, trialData)

        #### extract parameters ----
        head(trialData)
        extractData <- kin.extract.parameters(trialData, c("hand"), grasp = F)
        if(!any(is.na(extractData)))
        {

          #### append reach parameters to main dataset ----
          trialParams.r <- extractData$reach_parameters
          trialParams.r$trialN <- tN
          reach_paramData <- rbind(reach_paramData, trialParams.r[1,])

          #### append time info to main dataset ----
          timeinfoParams <- extractData$time_info
          timeinfoParams$trialN <- tN
          timeinfoData <- rbind(timeinfoData, timeinfoParams)
        } else
          badTrials <- c(badTrials, tN)
      } else
        badTrials <- c(badTrials, tN)
    } else
      badTrials <- c(badTrials, tN)
  } else
    badTrials <- c(badTrials, tN)
}

head(reach_paramData)
head(timeinfoData)
badTrials

trajData.backup <- trajData

#### classification ####
head(trajData)
head(testTrial)

ddply(subset(trajData, trialN < 20), .(trialN), summarise,
      N = length(handZ))

### based on landmark data
head(reach_paramData)
names(reach_paramData)

head(timeinfoData)
names(timeinfoData)

clustData <- cbind(
  reach_paramData[,names(reach_paramData) %in%
                    c("FX","FZ",
                      "FXVel","FZVel",
                      "FVel","FAcc",
                      "MVel","MAcc","MDec",
                      "timeMAcc","timeMVel","timeMDec",
                      "pathLength",
                      "Zmax","timeToZmax")]
  ,
  timeinfoData[,names(timeinfoData) %in%
                 c("onset",
                   "offset"#,
                   # "movTime"
                   )]
)
head(clustData)

# determining optimal clusters
library(NbClust)
optClN <- NbClust(clustData,
                  distance = "euclidean",
                  method = "average",
                  min.nc=2, max.nc=8)
(NC <- max(optClN$Best.partition))
?NbClust
# number of optimal clusters

# add clusters indexes
reach_paramData$cluster <- factor(paste0("C", optClN$Best.partition))








### based on trajectory data
trajData <- merge(trajData.backup, reach_paramData[c("trialN","cluster")])

# in each trial, cut time in 100 breaks
trajData <- ddply(trajData, .(trialN), mutate,
                  timeB = cut(time, breaks = 100, labels = F))

ggplot(aes(time, handZ), data = subset(trajData, trialN==0)) +
  stat_summary(geom = "point")

cut(subset(trajData, trialN==0)$time, breaks = 100, labels = F)

# average x, y, z kinematics in each of the 100 time breaks in each trial
trajDataB <- ddply(trajData, .(trialN, timeB), summarise,
                   cluster = unique(cluster),
                   handX = mean(handX, na.rm = T),
                   handY = mean(handY, na.rm = T),
                   handZ = mean(handZ, na.rm = T),

                   handXvel = mean(handXvel, na.rm = T),
                   handYvel = mean(handYvel, na.rm = T),
                   handZvel = mean(handZvel, na.rm = T),

                   handVel = mean(handVel, na.rm = T),
                   handAcc = mean(handAcc, na.rm = T))
head(trajDataB)
ggplot(aes(timeB, handZ), data = trajDataB) +
  stat_summary(geom = "point") +
  facet_grid(. ~ cluster)

lengthunique(subset(trajDataB, cluster == "C3")$trialN)
head(trajDataB)

ggplot(aes(timeB, handZ),
       data = subset(trajDataB, cluster == "C3" & trialN < 60)) +
  stat_summary(geom = "point") +
  facet_grid(. ~ trialN)


# probability data
probData <- ddply(exp1Data, .(subjName, trajectoryID), summarise,
                  responseN = mean(responseN),
                  trajectoryIDF = factor(paste0("T", unique(trajectoryID))),
                  RT = mean(RT))

# only to get the wide version of the dataset
res <- ANOVA(probData,
             "responseN",
             subjName ~ trajectoryIDF)

edaData <- res$wdata
clustData <- edaData[,-1]

# determining optimal clusters
library(NbClust)
optClN <- NbClust(clustData,
                  distance = "euclidean",
                  method = "complete",
                  min.nc=2, max.nc=8)
(NC <- max(optClN$Best.partition))
# number of optimal clusters

# add clusters indexes
edaData$cluster <- factor(paste0("C", optClN$Best.partition))


mu::mu.library("traj")
# Setup data and time
data = example.data$data
time = example.data$time

# Run step1measures, step2factors and step3clusters
dataset <- data
for(i in 1:10)
{
  dataset[sample(1:130, 1), sample(2:7, 1)] <- NA
}

s1 = step1measures(dataset,time, ID=TRUE)
s2 = step2factors(s1)
s3 = step3clusters(s2)

# Print and plot "traj object"
s3
plot(s3)

# Run step3measures with predetermined number of colusters
s3.4clusters = step3clusters(s2, nclusters=4)

# Display "traj" object
s3.4clusters
summary(s3.4clusters)
plot(s3.4clusters)

s3$cluster[1:10,]
