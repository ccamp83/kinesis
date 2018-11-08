options(scipen = 1, digits = 3)
library(kinesis)
library(cowplot)

# PROCEDURE FROM GALLIVAN and CHAPMAN 2014

#### Prepare the dataset ####
# five columns are expected, by default they are called as follows
(defaultCols <- get("dataCols", kinesis_parameters))
# check which default columns are missing
defaultCols[!defaultCols %in% names(reachData)]
# if the missing column is present in the dataset but called differently, set it
kin.setDataCols(deltaTime = "refreshRate")
# check that the renaming was successful
get("dataCols", kinesis_parameters)
# Check dataset
testData <- data.check(reachData)

#### select one trial as test ####
testTrial <- subset(testData, trialN == 50)

# make a backup
testTrial.backup <- testTrial

#### start location ####
start <- c(0,0,0)

#### end location ####
end <- c(0,0,.1)

#### refresh rate ####
deltaTime <- 1/85

#### signal analysis ####

# create a subset with only the columns that contain signal data
signalData <- testTrial[, c("handX","handZ")]

# what is the string that all the columns that contain signal data have in common?
signal.name <- "hand"

# check if there are x y and z columns for the signal
(signalDataCols <- names(signalData)[grepl(signal.name, names(signalData))])
# if one column is missing, set it to zero
if(length(signalDataCols) == 2)
{
  # expected columns
  expectedCols <- paste0(signal.name, c("X","Y","Z"))
  for(col in expectedCols)
  {
    isColFound <- length(names(signalData)[grepl(col, names(signalData))])>0
    if(!isColFound)
    {
      message(paste0("The following column is missing: ", col, ". Will be set to zero.\n"))
      signalData <- cbind(signalData, 0)
      names(signalData)[length(names(signalData))] <- col
    }
  }
}
# grab the columns with the signal name
signalData.nameCols <- names(signalData)[grepl(signal.name, names(signalData))]
# reorder
signalData.nameCols <- sort(signalData.nameCols)
# all other columns (if any)
otherCols <- names(signalData)[!grepl(signal.name, names(signalData))]

# retain only testTrial columns
signalData <- signalData[signalData.nameCols]

# append the suffix .raw to the signal columns
names(signalData) <- paste0(signal.name, c("X","Y","Z"), ".raw")

#### repair ####

# plot the data
(sigAnalysis.g <- ggplot() + geom_point(aes(handX.raw, handZ.raw), data = signalData) + coord_fixed())

# check for missing frames
signalRep <- as.data.frame(apply(signalData, 2, kin.signal.repair, maxFrames = maxFrames))
names(signalRep) <- paste0(signal.name, c("X","Y","Z"), "rep")

#### filter ####
signalSG <- as.data.frame(apply(signalRep, 2, kin.sgFilter, ts = deltaTime))
names(signalSG) <- paste0(signal.name, c("X","Y","Z"), "sg")

# plot the data
sigAnalysis.g +
  geom_point(aes(handXrep, handZrep), data = signalRep, color = "red") +
  geom_point(aes(handXsg, handZsg), data = signalSG, color = "blue")

# translate
M <- matrix(rep(start, nrow(signalSG)), ncol = 3, byrow = T) # replicate origin to create a dataset to subtract to the signal
signalTra <- as.data.frame(signalSG - M) # translate
names(signalTra) <- paste0(signal.name, c("X","Y","Z"), "tra")

sigAnalysis.g +
  geom_point(aes(handXrep, handZrep), data = signalRep, color = "red") +
  geom_point(aes(handXsg, handZsg), data = signalSG, color = "blue") +
  geom_point(aes(handXtra, handZtra), data = signalTra, color = "green")

# rotate

# the end location AFTER translation is
newEnd <- end - start

signalRot <- as.data.frame(kin.rotate.trajectory(signalTra, newEnd, f = F, t = T, s = T))
names(signalRot) <- paste0(signal.name, c("X","Y","Z"), "rot")

sigAnalysis.g +
  geom_point(aes(handXrep, handZrep), data = signalRep, color = "red") +
  geom_point(aes(handXsg, handZsg), data = signalSG, color = "blue") +
  geom_point(aes(handXtra, handZtra), data = signalTra, color = "green") +
  geom_point(aes(handXrot, handZrot), data = signalRot, color = "purple")

# vel, acc
signalVel <- as.data.frame(apply(signalRot, 2, kin.sgFilter, m=1, ts = deltaTime)) # 3D velocities
names(signalVel) <- paste0(signal.name, c("X","Y","Z"), "vel")
signalVel$vel_temp <- kin.sgFilter(sqrt(signalVel[,1]^2 + signalVel[,2]^2 + signalVel[,3]^2), ts = deltaTime)
signalVel$acc_temp <- kin.sgFilter(kin.sgFilter(signalVel$vel_temp, m = 1, ts = deltaTime), p = 12, ts = deltaTime)
names(signalVel)[4:5] <- paste(signal.name, c("Vel","Acc"), sep = "")

ggplot(mapping = aes(1:nrow(signalVel), signalVel$handVel)) + geom_point()
ggplot(mapping = aes(1:nrow(signalVel), signalVel$handZvel)) + geom_point()
ggplot(mapping = aes(1:nrow(signalVel), signalVel$handAcc)) + geom_point()

# merge
signal <- cbind(signalRot, signalVel)
names(signal)[1:3] <- paste0(signal.name, c("X","Y","Z"))

# merge all
trialData <- cbind(testTrial[c("subjName","trialN","frameN","refreshRate","time")], signal)

#### crop the inbound portion of trajectory ----
# set velocity threshold ----
returnVel_threshold <- -.05
# crop
trialData <- subset(trialData, handZvel > returnVel_threshold)

# review data
ggplot(mapping = aes(frameN, handVel), data = trialData) + geom_point()
ggplot() + geom_point(aes(handX.raw, handZ.raw), data = signalData) + coord_fixed()
ggplot() + geom_point(aes(handX, handZ), data = trialData) + coord_fixed()

#### onset time ----
# set velocity threshold ----
onsetVel_threshold <- .02
# find onset frame ----
(onsetFramePos <- kin.find.onsetTime(trialData$handVel, onsetVel_threshold))
# crop trajectory before onset ----
trialData <- subset(trialData, frameN >= frameN[onsetFramePos])

# review data
ggplot(mapping = aes(frameN, handVel), data = trialData) + geom_point()
ggplot() + geom_point(aes(handX, handZ), data = trialData) + coord_fixed()

#### offset time ----
# resultant vectors
trialData$handVelAcc.res <- with(trialData, sqrt(handVel^2 + handAcc^2))
# find offest time
offsetFrame <- with(trialData, frameN[match(kin.min(handVelAcc.res), handVelAcc.res)])
trialData$offsetTime <- trialData$time[trialData$frameN == offsetFrame]
# crop out trajectory after offset
trialData <- subset(trialData, frameN <= offsetFrame)

# review data
ggplot(mapping = aes(frameN, handVel), data = trialData) + geom_point()
ggplot() + geom_point(aes(handX, handZ), data = trialData) + coord_fixed()

#    1.1.4.5 package results ----
# columns to keep
testTrial.backup1 <- testTrial
keepCols <- c("trialN","indexX","indexY","indexZ","thumbX","thumbY","thumbZ",
              "subjName","frameN","deltaTime","time",
              "indexVel","indexAcc","thumbVel","thumbAcc")
testTrial <- testTrial.backup1[keepCols]

ggplot(data = testTrial) +
  geom_point(aes(time, thumbZ), color = "blue") +
  geom_point(aes(time, indexZ), color = "red")

#    1.1.5 curvature ---
testTrial$indexCurvature <- kin.signal.curvature(testTrial[c("indexX","indexY","indexZ")], deltaTime = 1/85)
testTrial$thumbCurvature <- kin.signal.curvature(testTrial[c("thumbX","thumbY","thumbZ")], deltaTime = 1/85)

#    1.1.5 total trajectory length ---
testTrial$indexCurveLength <- kin.signal.arclength(testTrial[c("indexX","indexY","indexZ")])
testTrial$thumbCurveLength <- kin.signal.arclength(testTrial[c("thumbX","thumbY","thumbZ")])

###  1.2 trajectory normalization ----
# space normalization

# first calculate euclidean distance of a reference trajectory to its final position
# in this example's case, it's the thumb
testTrial$thuDist <- sqrt((testTrial$thumbX - tail(testTrial$thumbX, 1))^2 +
                            (testTrial$thumbY - tail(testTrial$thumbY, 1))^2 +
                            (testTrial$thumbZ - tail(testTrial$thumbZ, 1))^2
)
# bin that distance
binN <- 100
testTrial$thuDistB <- with(testTrial, cut(thuDist, breaks = binN, labels = F))

##   1.2.1 space normalization through Functional Data Analysis (FDA) ----
# fit data with mathematical function
# for each trial, the discrete data in the extracted reach trajectory is fit using B-splines
# In our work, order six splines are fit to each of the three dimensions (x, y, z) of the motion data with
# a spline at every data point. The data are then smoothed using a roughness penalty on the fourth derivative (λ = 10−18, within
# 0.00001 of the generalized cross-validation estimate, Ramsay and Silverman, 2005), which allows for control of the smoothness of
# the second derivative. This process generates the mathematical definition of each dimension of data (x, y, or z) across time.

#### 2. single participant's average of many trials within a given experimental condition ####
#### 3. group average of that condition (average of all participants' mean trajectories) ####

#### extract kinematic parameters ####
# From melmoth & grant 2006

# --- general kinematics
onset <- min(testTrial$time)
offset <- max(testTrial$time)
movTime <- offset-onset

# ---- reach dynamics
# maximum acceleration
MAcc <- max(testTrial$indexAcc)
# maximum velocity
MVel <- max(testTrial$indexVel)
# maximum deceleration
MDec <- min(testTrial$indexAcc)

# time to maximum acceleration
timeMAcc <- testTrial$time[which.max(testTrial$indexAcc)]
# time to maximum velocity
timeMVel <- testTrial$time[which.max(testTrial$indexVel)]
# time to maximum deceleration
timeMDec <- testTrial$time[which.min(testTrial$indexAcc)]

# time from max acceleration to max velocity
timeMAccToMVel <- timeMVel - timeMAcc
# time from max velocity to max deceleration
timeMVelToMDec <- timeMDec - timeMVel
# time from max deceleration to movement offset
timeMDecToOffset <- offset - timeMDec

# ---- spatial reach kinematics
pathLength <- max(testTrial$indexCurveLength)
MdeviationX <- max(testTrial$indexX)
MdeviationY <- max(testTrial$indexY)

# ---- final 3D position
FX = testTrial$indexX[testTrial$time == offset]
FY = testTrial$indexY[testTrial$time == offset]
FZ = testTrial$indexZ[testTrial$time == offset]

#### check velocity ####
# check if there are anomalies in the calculation of velocity and acceleration that might be due to bad sampling
testTrial$indexVel.check <- mu::outliers.check(testTrial$indexVel, sd.thresh = 1.5, logical = T)
testTrial$thumbVel.check <- mu::outliers.check(testTrial$thumbVel, sd.thresh = 1.5, logical = T)

print(
  ggplot(data = testTrial) +
    geom_point(aes(time, scale(indexVel), alpha = indexVel.check), color = "red") +
    geom_point(aes(time, scale(thumbVel), alpha = thumbVel.check), color = "blue")
)

# turn bad samples into NAs
testTrial$indexX[testTrial$indexVel.check==1] <- NA
testTrial$indexY[testTrial$indexVel.check==1] <- NA
testTrial$indexZ[testTrial$indexVel.check==1] <- NA

testTrial$thumbX[testTrial$thumbVel.check==1] <- NA
testTrial$thumbY[testTrial$thumbVel.check==1] <- NA
testTrial$thumbZ[testTrial$thumbVel.check==1] <- NA
# repair again
testTrial$indexX <- with(testTrial, kin.signal.repair(indexX, maxFrames= 20))
testTrial$indexY <- with(testTrial, kin.signal.repair(indexY, maxFrames= 20))
testTrial$indexZ <- with(testTrial, kin.signal.repair(indexZ, maxFrames= 20))

testTrial$thumbX <- with(testTrial, kin.signal.repair(thumbX, maxFrames= 20))
testTrial$thumbY <- with(testTrial, kin.signal.repair(thumbY, maxFrames= 20))
testTrial$thumbZ <- with(testTrial, kin.signal.repair(thumbZ, maxFrames= 20))
