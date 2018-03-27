options(scipen = 1, digits = 3)

libraries()
library(cowplot)

# PROCEDURE FROM GALLIVAN and CHAPMAN 2014

#### Prepare the dataset ####
# keep only useful columns
usefCols <- c("trialN","indexXraw","indexYraw","indexZraw","thumbXraw","thumbYraw","thumbZraw")
testData <- rtgData_bad[usefCols]
# add frameN
testData <- kin.frameN(testData)
# is there missing data for each digit in this dataset?
testData <- kin.fingersOccluded(testData)
any(testData$fingersOccluded==1) # YES
# count frames with missing data
testData <- kin.framesOccluded(testData)
# create column for frame time (refresh rate)
testData$frameT <- 1/85
# create time column assuming constant 85Hz sampling
testData$time <- testData$frameN * testData$frameT

#### 1. individual trial analysis ####
###  1.1 extract ROI ----
##   1.1.1 fill in missing frames ----
# inpaint_nans function in matlab

# select one trial with missing frames
# flag trials with missing frames
testData <- ddply(testData, .(trialN), mutate,
                  missing.frames = any(framesOccluded > 0))
# extract these trials number
trial.na.frame <- unique(subset(testData, missing.frames)$trialN)
# pick a random bad trial
badTrialNum <- 40
testTrial <- subset(testData, trialN == badTrialNum)
# plot the data
ggplot(aes(frameN, thumbXraw, color = fingersOccluded), data = testTrial) + geom_point() # thumb data is bad
ggplot(aes(frameN, indexXraw, color = fingersOccluded), data = testTrial) + geom_point() # index data is OK

# how many bad frame are there?
max(testTrial$framesOccluded) # 15

# repair missing frames
testTrial$indexXrep <- with(testTrial, kin.smooth.repair(frameN,indexXraw, lam = 10e-18, maxFrames= 20, fingersOccluded=fingersOccluded, framesOccluded=framesOccluded))
testTrial$indexYrep <- with(testTrial, kin.smooth.repair(frameN,indexYraw, lam = 10e-18, maxFrames= 20, fingersOccluded=fingersOccluded, framesOccluded=framesOccluded))
testTrial$indexZrep <- with(testTrial, kin.smooth.repair(frameN,indexZraw, lam = 10e-18, maxFrames= 20, fingersOccluded=fingersOccluded, framesOccluded=framesOccluded))

testTrial$thumbXrep <- with(testTrial, kin.smooth.repair(frameN,thumbXraw, lam = 10e-18, maxFrames= 20, fingersOccluded=fingersOccluded, framesOccluded=framesOccluded))
testTrial$thumbYrep <- with(testTrial, kin.smooth.repair(frameN,thumbYraw, lam = 10e-18, maxFrames= 20, fingersOccluded=fingersOccluded, framesOccluded=framesOccluded))
testTrial$thumbZrep <- with(testTrial, kin.smooth.repair(frameN,thumbZraw, lam = 10e-18, maxFrames= 20, fingersOccluded=fingersOccluded, framesOccluded=framesOccluded))

ggplot(aes(frameN, thumbXrep, color = fingersOccluded), data = testTrial) + geom_point() # thumb data is bad

##   1.1.2 filter ----
#    1.1.2.1 butterworth filter ----
# dual pass, 8–12 Hz cutoff, 2nd order
testTrial$indexXbw <- with(testTrial, kin.bwFilter(indexXrep, cutoff_freq = 10, type = "pass"))
testTrial$indexYbw <- with(testTrial, kin.bwFilter(indexYrep, cutoff_freq = 10, type = "pass"))
testTrial$indexZbw <- with(testTrial, kin.bwFilter(indexZrep, cutoff_freq = 10, type = "pass"))

testTrial$thumbXbw <- with(testTrial, kin.bwFilter(thumbXrep, cutoff_freq = 10, type = "pass"))
testTrial$thumbYbw <- with(testTrial, kin.bwFilter(thumbYrep, cutoff_freq = 10, type = "pass"))
testTrial$thumbZbw <- with(testTrial, kin.bwFilter(thumbZrep, cutoff_freq = 10, type = "pass"))

ggplot(data = testTrial) +
  geom_point(aes(frameN, thumbXrep), color = "black") +
  geom_point(aes(frameN, thumbXbw), color = "red", alpha=.5)

#    1.1.2.2 Savitzky-Golay filter ----
# 3rd order
testTrial$indexXsg <- with(testTrial, kin.sgFilter(indexXrep, ts = 1/85))
testTrial$indexYsg <- with(testTrial, kin.sgFilter(indexYrep, ts = 1/85))
testTrial$indexZsg <- with(testTrial, kin.sgFilter(indexZrep, ts = 1/85))

testTrial$thumbXsg <- with(testTrial, kin.sgFilter(thumbXrep, ts = 1/85))
testTrial$thumbYsg <- with(testTrial, kin.sgFilter(thumbYrep, ts = 1/85))
testTrial$thumbZsg <- with(testTrial, kin.sgFilter(thumbZrep, ts = 1/85))

ggplot(data = testTrial) +
  geom_point(aes(frameN, thumbXrep), color = "black") +
  geom_point(aes(frameN, thumbXsg), color = "green", alpha=.5)

#    1.2.2.3 choose filter and apply ----
# savitzky-golay filter is less invasive than butterworth (less variable residuals)
qplot(indexXbw-indexXrep, indexXsg-indexXrep, data=testTrial, geom="point") + coord_fixed()
qplot(thumbXbw-thumbXrep, thumbXsg-thumbXrep, data=testTrial, geom="point") + coord_fixed()

testTrial$indexX <- testTrial$indexXsg
testTrial$indexY <- testTrial$indexYsg
testTrial$indexZ <- testTrial$indexZsg

testTrial$thumbX <- testTrial$thumbXsg
testTrial$thumbY <- testTrial$thumbYsg
testTrial$thumbZ <- testTrial$thumbZsg

##   1.1.3 translate and rotate to have all trajectories going in the same direction ----

# the trajectory is assumed to start at origin (0,0,0)
# and terminate at a point (0,0,z)
# with x axis: rightwards positive, leftwards negative
#      y axis: upwards positive, downwards negative
#      z axis: forwards positive, backwards negative

# this requires a translation followed by a rotation

# we know that in this dataset the z axis is reversed relative to the expected direction
# so we flip it
testTrial$indexZ <- testTrial$indexZ * -1
testTrial$thumbZ <- testTrial$thumbZ * -1

# starting dataset to apply translations and rotations on recursively
indData.backup <- testTrial[,c("indexX","indexY","indexZ")]
thuData.backup <- testTrial[,c("thumbX","thumbY","thumbZ")]
# dataset containing to-be-rotated fingers positions
rotData.backup <- rbind(indData.backup, setNames(thuData.backup, names(indData.backup)))

# define start and end of movement
# case unknown
# rationale: there will be many more samples around start and end of movement
# because of the low speed of motion
# hence the distribution of x, y and z positions should be highly bimodal
# we use cluster analysis to find the centroids of the two clusters (start & end)
# of each position of each finger

# CASE 1: whole grasp
kmData <- cbind(rotData.backup, time = testTrial[,"time"])
km.res <- as.data.frame(kmeans(kmData, 2)$centers)
km.res$moment <- with(km.res, ifelse(time == min(time), "start", "end"))
# translate the trajectory to origin (0,0,0)
transData <- km.res[km.res$moment=="start", !names(km.res)%in%c("time","moment")] # getting the centroids
# matrix subtraction
rotData <- rotData.backup - transData[rep(1, nrow(rotData.backup)),]
indData <- indData.backup - transData[rep(1, nrow(indData)),]
thuData <- thuData.backup - transData[rep(1, nrow(thuData)),]
# end coordinates of the whole grasp
end <- as.numeric(km.res[km.res$moment=="end",1:3] - km.res[km.res$moment=="start",1:3]) # centered end coordinates
# rotate trajectories
indData <- kin.rotate.trajectory(indData, end)
thuData <- kin.rotate.trajectory(thuData, end)
# polish rotated dataset
indData <- as.data.frame(indData)
names(indData) <- c("indexX","indexY","indexZ")
thuData <- as.data.frame(thuData)
names(thuData) <- c("thumbX","thumbY","thumbZ")

plot3d(rotData[c(1,3,2)])
points3d(indData[c(1,3,2)], col="red")
points3d(thuData[c(1,3,2)], col="blue")

# CASE 2: individual fingers
# --- INDEX
kmData <- cbind(indData.backup, time = testTrial[,"time"])
km.res <- as.data.frame(kmeans(kmData, 2)$centers)
km.res$moment <- with(km.res, ifelse(time == min(time), "start", "end"))
# translate the trajectory to origin (0,0,0)
transData <- km.res[km.res$moment=="start", !names(km.res)%in%c("time","moment")] # getting the centroids
# matrix subtraction
indData <- indData.backup - transData[rep(1, nrow(indData.backup)),]
# end coordinates of the whole grasp
end.ind <- as.numeric(km.res[km.res$moment=="end",1:3] - km.res[km.res$moment=="start",1:3]) # centered end coordinates
# --- THUMB
kmData <- cbind(thuData.backup, time = testTrial[,"time"])
km.res <- as.data.frame(kmeans(kmData, 2)$centers)
km.res$moment <- with(km.res, ifelse(time == min(time), "start", "end"))
# translate the trajectory to origin (0,0,0)
transData <- km.res[km.res$moment=="start", !names(km.res)%in%c("time","moment")] # getting the centroids
# matrix subtraction
thuData <- thuData.backup - transData[rep(1, nrow(thuData.backup)),]
# end coordinates of the whole grasp
end.thu <- as.numeric(km.res[km.res$moment=="end",1:3] - km.res[km.res$moment=="start",1:3]) # centered end coordinates
# rotate trajectories
indData.m <- kin.rotate.trajectory(indData, end.ind)
thuData.m <- kin.rotate.trajectory(thuData, end.thu)
# polish rotated dataset
indData <- as.data.frame(indData.m)
names(indData) <- c("indexX","indexY","indexZ")
thuData <- as.data.frame(thuData.m)
names(thuData) <- c("thumbX","thumbY","thumbZ")

plot3d(rotData[c(1,3,2)])
points3d(indData[c(1,3,2)], col="red")
points3d(thuData[c(1,3,2)], col="blue")

##   1.1.4 find movement onset ----
#    1.1.4.1 calculate velocity vector ----
# using Savitzy-Golay filter (remember the frame rate!!)
testTrial$thumbXvel <- with(testTrial, kin.sgFilter(thumbX,m=1, ts = 1/85))
testTrial$thumbYvel <- with(testTrial, kin.sgFilter(thumbY,m=1, ts = 1/85))
testTrial$thumbZvel <- with(testTrial, kin.sgFilter(thumbZ,m=1, ts = 1/85))
testTrial$thumbVel <- with(testTrial, sqrt(thumbXvel^2 + thumbYvel^2 + thumbZvel^2)) # in mm/s

#    1.1.4.2 Savitzky-Golay filter velocitiy and acceleration vectors ----
# 3rd order
# filter velocity
testTrial$thumbVel <- with(testTrial, kin.sgFilter(thumbVel, ts = 1/85))
# derive acceleration
testTrial$thumbAcc <- with(testTrial, kin.sgFilter(thumbVel, m=1, ts = 1/85))
# filter acceleration
testTrial$thumbAcc <- with(testTrial, kin.sgFilter(thumbAcc, ts = 1/85))

ggplot(data = testTrial) +
  geom_point(aes(time, thumbXvel), color = "red") +
  geom_point(aes(time, thumbYvel), color = "darkgreen") +
  geom_point(aes(time, thumbZvel), color = "blue") +
  geom_point(aes(time, thumbVel), color = "black")

#    1.1.4.3 set onset frame ----
# set the onset frame to be the first of four consecutive vector velocity readings of greater than a threshold
# in which

# crop trajectory where velocity is < 20 mm/s
onsetData <- testTrial[,c("frameN","thumbVel","thumbAcc")]
onsetData$travel <- with(onsetData, thumbVel > 20)
with(onsetData, split(thumbVel, travel))


##   1.1.5 find movement offset ----
# We set the offset frame to be whichever of two events occurs first: the frame containing the maximum position value in
# the direction of the reach (i.e., the max reach extent) or the first frame in which the velocity drops below 20 mm/s.

###  1.2 trajectory normalization ----
# time normalization is tricky
# space normalization can be better

##   1.2.1 space normalization through Functional Data Analysis (FDA) ----
# fit data with mathematical function
# for each trial, the discrete data in the extracted reach trajectory is fit using B-splines
# In our work, order six splines are fit to each of the three dimensions (x, y, z) of the motion data with
# a spline at every data point. The data are then smoothed using a roughness penalty on the fourth derivative (λ = 10−18, within
# 0.00001 of the generalized cross-validation estimate, Ramsay and Silverman, 2005), which allows for control of the smoothness of
# the second derivative. This process generates the mathematical definition of each dimension of data (x, y, or z) across time.

#### 2. single participant's average of many trials within a given experimental condition ####
#### 3. group average of that condition (average of all participants' mean trajectories) ####

