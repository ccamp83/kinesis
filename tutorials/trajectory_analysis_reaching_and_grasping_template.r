# install.packages("remotes")
# remotes::install_github("vbaliga/pathviewR", force = T) # read motive-generated csv files
# devtools::install_github("ccamp83/mu")                  # stats analysis package
# devtools::install_github("ccamp83/kinesis", force = T)  # kinematic analysis package
library(mu)
library(kinesis)
library(pathviewr)
library(ggplot2)
library(cowplot)
theme_set(theme_cowplot())

### FETCH DATA ###
data(motiveData)

samplingHz <- 120

# rename cols
names(motiveData) <- c(
  "frameN",
  "time",
  paste0(rep(c("index","knuc","thumb","wrist"), each = 3), c("X","Y","Z"), "raw")
)

#### Prepare the dataset for preprocessing with kinesis package ####

# five columns are expected
get("dataCols", kinesis_parameters)
# user can set them
kin.setDataCols(deltaTime = "refreshRate")
get("dataCols", kinesis_parameters)
# Fix dataset
testData <- data.check(motiveData)
# enter any subjName on the console when prompted

# recap
head(testData)

### PREPROCESSING ####

options(scipen = 1, digits = 3)

#### TRAJECTORY DATA ####
trajData <- NULL
reach_paramData <- NULL
grasp_paramData <- NULL
timeinfoData <- NULL

# markers labels
signalsName <- c("index","thumb","knuc","wrist")

#### Analysis loop ####
trialsList <- unique(testData$trialN)
badTrials <- NULL

for(tN in trialsList)
{
  # tN <- trialsList[1]
  cat("---- trial #", tN, ". ----\n\n", sep = "")
  #### select trial ----
  testTrial.backup <- subset(testData, trialN == tN)

  columnsToExclude <- NULL # placeholder in case some columns can be excluded from traj analysis to make it run smoother
  testTrial <- testTrial.backup[, !names(testTrial.backup)%in%columnsToExclude]

  #### signals analysis ----

  # set start x y z coordinates ----
  start <- c(0,0,0)
  # set end (depends on trial's conditions) ----
  end <- c(0,0,.5) # 50 cm along the line of sight
  # refresh rate ----
  refreshRate <- 1/samplingHz

  # prepare signals datasets
  all.signal <- testTrial[,c(paste0(rep(signalsName, each = 3),c("X","Y","Z"),"raw"),"time")] # requires 3D positional (raw) data and time column ONLY
  # analysis: repair, filter, translate, rotate ----
  signalData <- NULL
  for(s in signalsName)
  {
    if(is.null(signalData))
    {
      signalData <- kin.signal.analysis(all.signal, s, start, end, f = F)
    } else
      signalData <- merge(signalData, kin.signal.analysis(all.signal, s, start, end, f = F))
  }

  head(signalData)

  if(any(is.na(signalData)))
  {
    badTrials <- c(badTrials, tN)
    next
  }

  #### merge back ----
  trialData <- cbind(testTrial[c("subjName","trialN","frameN","refreshRate",
                                 paste0(rep(signalsName, each = 3),c("X","Y","Z"),"raw"))], signalData)

  #### onset time ----
  # set velocity threshold ----
  onsetVel_threshold <- .02
  onset_condition <- "trialData$wristZvel > onsetVel_threshold" # velocity threshold based on wrist motion
  # find onset frame ----
  onsetFramePos <- kin.find.traj.landmark(onset_condition)

  if(is.na(onsetFramePos))
  {
    badTrials <- c(badTrials, tN)
    next
  }

  # crop trajectory before onset ----
  trialData <- subset(trialData, frameN >= frameN[onsetFramePos])

  #### offset time ----
  # set velocity threshold ----
  returnVel_threshold <- .01
  offset_condition <- "trialData$wristZvel < returnVel_threshold"
  # find the frame where this happens
  offsetFramePos <- kin.find.traj.landmark(offset_condition)

  if(is.na(offsetFramePos))
  {
    badTrials <- c(badTrials, tN)
    next
  }

  # crop
  trialData <- subset(trialData, frameN < frameN[offsetFramePos])

  # ggplot(data = trialData) +
  #   geom_point(aes(time, wristZ)) +
  #   geom_point(aes(time, knucZ), color = "darkgreen") +
  #   geom_point(aes(time, indexZ), color = "red") +
  #   geom_point(aes(time, thumbZ), color = "blue")

  #### append trajectory data to main trajectory dataset ----
  trajData <- rbind(trajData, trialData)

  #### extract parameters ----

  # calculate grip aperture kinematics
  trialData <- cbind(trialData,
                     kin.grasp.analysis(trialData, c("index","thumb"), refreshRate)
  )

  # extract reach and grasp kinematics
  extractData <- kin.extract.parameters(trialData, signalsName, grasp = T)

  if(any(is.na(extractData)))
  {
    badTrials <- c(badTrials, tN)
    next
  }

  #### append reach parameters to main dataset ----
  trialParams.r <- extractData$reach_parameters
  trialParams.r$trialN <- tN
  reach_paramData <- rbind(reach_paramData, trialParams.r)

  #### append grasp parameters to main dataset ----
  trialParams.g <- extractData$grasp_parameters
  trialParams.g$trialN <- tN
  grasp_paramData <- rbind(grasp_paramData, trialParams.g)

  #### append time info to main dataset ----
  timeinfoParams <- extractData$time_info
  timeinfoParams$trialN <- tN
  timeinfoData <- rbind(timeinfoData, timeinfoParams)
}

#### RESULTS ####

# results datasets
reach_paramData
grasp_paramData
timeinfoData
badTrials

#### check goodness of preprocessing & extraction ####

# index as f(time): raw (black) vs extracted (red)
ggplot() +
  geom_point(aes(time, indexZraw), data = testData) +
  geom_point(aes(time, indexZ), data = trialData, col = "red")

# thumb as f(time): raw (black) vs extracted (blue)
ggplot() +
  geom_point(aes(time, thumbZraw), data = testData) +
  geom_point(aes(time, thumbZ), data = trialData, col = "blue")

# all hand - birds' eye view: raw (black) vs extracted (turquoise)
ggplot() +

  geom_point(aes(indexXraw, indexZraw), data = testData) +
  geom_point(aes(thumbXraw, thumbZraw), data = testData) +
  geom_point(aes(knucXraw, knucZraw), data = testData) +
  geom_point(aes(wristXraw, wristZraw), data = testData) +

  geom_point(aes(indexX, indexZ), data = trialData, col = "turquoise") +
  geom_point(aes(thumbX, thumbZ), data = trialData, col = "turquoise") +
  geom_point(aes(knucX, knucZ), data = trialData, col = "turquoise") +
  geom_point(aes(wristX, wristZ), data = trialData, col = "turquoise") +

  coord_fixed()

# MGA, FGA
ggplot() +
  geom_point(aes(time, GA), data = trialData) +
  geom_point(aes(timeMGA, MGA), size = 3, col = "green", data = grasp_paramData) +
  geom_point(aes(timeinfoData$offset, FGA), size = 3, col = "pink", data = grasp_paramData)

# MVel
ggplot() +
  geom_point(aes(time, GPVel), data = trialData) +
  geom_point(aes(timeMVel, MVel), size = 3, col = "green", data = grasp_paramData)

# MAcc, MDec
ggplot() +
  geom_point(aes(time, GPAcc), data = trialData) +
  geom_point(aes(timeMAcc, MAcc), size = 3, col = "green", data = grasp_paramData) +
  geom_point(aes(timeMDec, MDec), size = 3, col = "pink", data = grasp_paramData)
