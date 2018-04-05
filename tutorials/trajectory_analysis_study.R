options(scipen = 1, digits = 3)

library(cowplot)

# PROCEDURE FROM GALLIVAN and CHAPMAN 2014

#### Prepare the dataset ####
# keep only useful columns
usefCols <- c("trialN","indexXraw","indexYraw","indexZraw","thumbXraw","thumbYraw","thumbZraw")
# Fix dataset
testData <- data.check(rtgData_bad[usefCols])
test

#### 1. individual trial analysis ####
###  1.1 extract ROI ----
##   1.1.1 fill in missing frames ----
# inpaint_nans function in matlab

# select one trial with missing frames
testTrial <- subset(testData, trialN == 50)
# testTrial <- subset(testData, trialN == badTrialNum)
# plot the data
# ggplot(aes(frameN, thumbXraw), data = testTrial) + geom_point() # thumb data is bad

# repair missing frames
testTrial$indexXrep <- with(testTrial, kin.signal.repair(indexXraw, maxFrames= 20))
testTrial$indexYrep <- with(testTrial, kin.signal.repair(indexYraw, maxFrames= 20))
testTrial$indexZrep <- with(testTrial, kin.signal.repair(indexZraw, maxFrames= 20))

testTrial$thumbXrep <- with(testTrial, kin.signal.repair(thumbXraw, maxFrames= 20))
testTrial$thumbYrep <- with(testTrial, kin.signal.repair(thumbYraw, maxFrames= 20))
testTrial$thumbZrep <- with(testTrial, kin.signal.repair(thumbZraw, maxFrames= 20))

# thumb data fixed
# ggplot(aes(frameN, thumbXrep, color = fingersOccluded), data = testTrial) + geom_point()

##   1.1.2 filter ----
#    1.1.2.1 butterworth filter ----
# dual pass, 8–12 Hz cutoff, 2nd order
# testTrial$indexXbw <- with(testTrial, kin.bwFilter(indexXrep, cutoff_freq = 10, type = "pass"))
# testTrial$indexYbw <- with(testTrial, kin.bwFilter(indexYrep, cutoff_freq = 10, type = "pass"))
# testTrial$indexZbw <- with(testTrial, kin.bwFilter(indexZrep, cutoff_freq = 10, type = "pass"))
#
# testTrial$thumbXbw <- with(testTrial, kin.bwFilter(thumbXrep, cutoff_freq = 10, type = "pass"))
# testTrial$thumbYbw <- with(testTrial, kin.bwFilter(thumbYrep, cutoff_freq = 10, type = "pass"))
# testTrial$thumbZbw <- with(testTrial, kin.bwFilter(thumbZrep, cutoff_freq = 10, type = "pass"))

# ggplot(data = testTrial) +
#   geom_point(aes(frameN, thumbXrep), color = "black") +
#   geom_point(aes(frameN, thumbXbw), color = "red", alpha=.5)

#    1.1.2.2 Savitzky-Golay filter ----
# 3rd order
testTrial$indexXsg <- with(testTrial, kin.sgFilter(indexXrep, ts = 1/85))
testTrial$indexYsg <- with(testTrial, kin.sgFilter(indexYrep, ts = 1/85))
testTrial$indexZsg <- with(testTrial, kin.sgFilter(indexZrep, ts = 1/85))

testTrial$thumbXsg <- with(testTrial, kin.sgFilter(thumbXrep, ts = 1/85))
testTrial$thumbYsg <- with(testTrial, kin.sgFilter(thumbYrep, ts = 1/85))
testTrial$thumbZsg <- with(testTrial, kin.sgFilter(thumbZrep, ts = 1/85))

# ggplot(data = testTrial) +
#   geom_point(aes(frameN, thumbXrep), color = "black") +
#   geom_point(aes(frameN, thumbXsg), color = "green", alpha=.5)

#    1.2.2.3 choose filter and apply ----
# savitzky-golay filter is less distorting than butterworth (less variable residuals)
# qplot(indexXbw-indexXrep, indexXsg-indexXrep, data=testTrial, geom="point") + coord_fixed()
# qplot(thumbXbw-thumbXrep, thumbXsg-thumbXrep, data=testTrial, geom="point") + coord_fixed()

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
indData <- indData.backup - transData[rep(1, nrow(indData.backup)),]
thuData <- thuData.backup - transData[rep(1, nrow(thuData.backup)),]
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

# plot3d(rotData[c(1,3,2)])
# points3d(indData[c(1,3,2)], col="red")
# points3d(thuData[c(1,3,2)], col="blue")

# ggplot() +
#   geom_point(aes(indexX, indexZ), data= rotData) +
#   geom_point(aes(indexX, indexZ), data= indData, color = "red") +
#   geom_point(aes(thumbX, thumbZ), data= thuData, color = "blue") +
#   coord_fixed()

# # CASE 2: individual fingers
# # --- INDEX
# kmData <- cbind(indData.backup, time = testTrial[,"time"])
# km.res <- as.data.frame(kmeans(kmData, 2)$centers)
# km.res$moment <- with(km.res, ifelse(time == min(time), "start", "end"))
# # translate the trajectory to origin (0,0,0)
# transData <- km.res[km.res$moment=="start", !names(km.res)%in%c("time","moment")] # getting the centroids
# # matrix subtraction
# indData <- indData.backup - transData[rep(1, nrow(indData.backup)),]
# # end coordinates of the whole grasp
# end.ind <- as.numeric(km.res[km.res$moment=="end",1:3] - km.res[km.res$moment=="start",1:3]) # centered end coordinates
# # --- THUMB
# kmData <- cbind(thuData.backup, time = testTrial[,"time"])
# km.res <- as.data.frame(kmeans(kmData, 2)$centers)
# km.res$moment <- with(km.res, ifelse(time == min(time), "start", "end"))
# # translate the trajectory to origin (0,0,0)
# transData <- km.res[km.res$moment=="start", !names(km.res)%in%c("time","moment")] # getting the centroids
# # matrix subtraction
# thuData <- thuData.backup - transData[rep(1, nrow(thuData.backup)),]
# # end coordinates of the whole grasp
# end.thu <- as.numeric(km.res[km.res$moment=="end",1:3] - km.res[km.res$moment=="start",1:3]) # centered end coordinates
# # rotate trajectories
# indData.m <- kin.rotate.trajectory(indData, end.ind)
# thuData.m <- kin.rotate.trajectory(thuData, end.thu)
# # polish rotated dataset
# indData <- as.data.frame(indData.m)
# names(indData) <- c("indexX","indexY","indexZ")
# thuData <- as.data.frame(thuData.m)
# names(thuData) <- c("thumbX","thumbY","thumbZ")

# plot3d(rotData[c(1,3,2)])
# points3d(indData[c(1,3,2)], col="red")
# points3d(thuData[c(1,3,2)], col="blue")

# merge rotated data and apply rotation
names(indData) <- paste(names(indData), "rot", sep="")
names(thuData) <- paste(names(thuData), "rot", sep="")
testTrial <- cbind(testTrial, indData, thuData)

testTrial$indexX <- testTrial$indexXrot
testTrial$indexY <- testTrial$indexYrot
testTrial$indexZ <- testTrial$indexZrot

testTrial$thumbX <- testTrial$thumbXrot
testTrial$thumbY <- testTrial$thumbYrot
testTrial$thumbZ <- testTrial$thumbZrot

##   1.1.4 find movement onset ----
#    1.1.4.1 calculate velocity vector ----
# using Savitzy-Golay filter (remember the frame rate!!)
testTrial$thumbXvel <- with(testTrial, kin.sgFilter(thumbX,m=1, ts = 1/85))
testTrial$thumbYvel <- with(testTrial, kin.sgFilter(thumbY,m=1, ts = 1/85))
testTrial$thumbZvel <- with(testTrial, kin.sgFilter(thumbZ,m=1, ts = 1/85))
testTrial$thumbVel <- with(testTrial, sqrt(thumbXvel^2 + thumbYvel^2 + thumbZvel^2)) # in mm/s

# using Savitzy-Golay filter (remember the frame rate!!)
testTrial$indexXvel <- with(testTrial, kin.sgFilter(indexX,m=1, ts = 1/85))
testTrial$indexYvel <- with(testTrial, kin.sgFilter(indexY,m=1, ts = 1/85))
testTrial$indexZvel <- with(testTrial, kin.sgFilter(indexZ,m=1, ts = 1/85))
testTrial$indexVel <- with(testTrial, sqrt(indexXvel^2 + indexYvel^2 + indexZvel^2)) # in mm/s

#    1.1.4.2 Savitzky-Golay filter velocitiy, acceleration and jerk vectors ----
# 3rd order
# filter velocity
testTrial$indexVel <- with(testTrial, kin.sgFilter(indexVel, ts = 1/85))
testTrial$thumbVel <- with(testTrial, kin.sgFilter(thumbVel, ts = 1/85))
# derive acceleration
testTrial$indexAcc <- with(testTrial, kin.sgFilter(indexVel, m=1, ts = 1/85))
testTrial$thumbAcc <- with(testTrial, kin.sgFilter(thumbVel, m=1, ts = 1/85))
# filter acceleration
testTrial$indexAcc <- with(testTrial, kin.sgFilter(indexAcc, p = 12, ts = 1/85))
testTrial$thumbAcc <- with(testTrial, kin.sgFilter(thumbAcc, p = 12, ts = 1/85))
# derive jerk
testTrial$indexJerk <- with(testTrial, kin.sgFilter(indexAcc, m=1, ts = 1/85))
testTrial$thumbJerk <- with(testTrial, kin.sgFilter(thumbAcc, m=1, ts = 1/85))
# filter jerk
testTrial$indexJerk <- with(testTrial, kin.sgFilter(indexJerk, p = 12, ts = 1/85))
testTrial$thumbJerk <- with(testTrial, kin.sgFilter(thumbJerk, p = 12, ts = 1/85))

#    1.1.4.3 find onset time ----
# crop out the inbound portion of trajectory
testTrial.backup <- testTrial
return_threshold <- -100
testTrial <- subset(testTrial.backup, thumbZvel > return_threshold & indexZvel > return_threshold)

# flag trajectory where z velocity is > threshold
# incrementally count frames where the condition is met
# the longest string is the winner, take its fist frame as zero
onset_threshold <- 100
onsetFrame <- kin.find.onsetTime(testTrial$thumbVel, onset_threshold)
# time at movement onset
testTrial$onsetTime <- testTrial$time[testTrial$frameN == onsetFrame]
# crop out trajectory before onset
testTrial <- subset(testTrial, frameN >= onsetFrame)

#    1.1.4.4 find offset time ----
# in case there is no exact offset time, offset time is defined differently depending on the movement
# in the case of grasping, it is defined as the time at which velocity, acceleration and jerk of index and thumb reach a minimum

# resultant vectors
testTrial$thumbVelAccJerk.res <- with(testTrial, sqrt(thumbVel^2 + thumbAcc^2 + thumbJerk^2))
testTrial$indexVelAccJerk.res <- with(testTrial, sqrt(thumbVel^2 + thumbAcc^2 + thumbJerk^2))
testTrial$index_thumbVelAccJerk.res <- with(testTrial, sqrt(thumbVelAccJerk.res^2 + indexVelAccJerk.res^2))
# find offest time
offsetFrame <- with(testTrial, frameN[match(kin.min(index_thumbVelAccJerk.res), index_thumbVelAccJerk.res)])
testTrial$offsetTime <- testTrial$time[testTrial$frameN == offsetFrame]
# crop out trajectory after offset
testTrial <- subset(testTrial, frameN <= offsetFrame)

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

###  1.2 trajectory normalization ----
# time normalization is tricky
# so opt for space normalization

# first calculate euclidean distance of a reference trajectory to its final position
# in this example's case, it's the thumb
testTrial$thuDist <- sqrt((testTrial$thumbX - tail(testTrial$thumbX, 1))^2 +
                          (testTrial$thumbY - tail(testTrial$thumbY, 1))^2 +
                          (testTrial$thumbZ - tail(testTrial$thumbZ, 1))^2
                          )
# bin that distance
binN <- 100
testTrial$thuDistB <- with(testTrial, cut(thuDist, breaks = binN, labels = F))

curvatureData.ind <- testTrial[c("indexX","indexY","indexZ")]
curvatureData.ind$indexXvel <- with(curvatureData.ind, kin.sgFilter(indexX,m=1, ts = 1/85))
curvatureData.ind$indexYvel <- with(curvatureData.ind, kin.sgFilter(indexY,m=1, ts = 1/85))
curvatureData.ind$indexZvel <- with(curvatureData.ind, kin.sgFilter(indexZ,m=1, ts = 1/85))
curvatureData.ind$indexXacc <- with(curvatureData.ind, kin.sgFilter(indexX,m=2, ts = 1/85))
curvatureData.ind$indexYacc <- with(curvatureData.ind, kin.sgFilter(indexY,m=2, ts = 1/85))
curvatureData.ind$indexZacc <- with(curvatureData.ind, kin.sgFilter(indexZ,m=2, ts = 1/85))

k = with(curvatureData.ind, abs())



##   1.2.1 space normalization through Functional Data Analysis (FDA) ----
# fit data with mathematical function
# for each trial, the discrete data in the extracted reach trajectory is fit using B-splines
# In our work, order six splines are fit to each of the three dimensions (x, y, z) of the motion data with
# a spline at every data point. The data are then smoothed using a roughness penalty on the fourth derivative (λ = 10−18, within
# 0.00001 of the generalized cross-validation estimate, Ramsay and Silverman, 2005), which allows for control of the smoothness of
# the second derivative. This process generates the mathematical definition of each dimension of data (x, y, or z) across time.

#### 2. single participant's average of many trials within a given experimental condition ####
#### 3. group average of that condition (average of all participants' mean trajectories) ####

# find core kinematics:
# velocity peak
# acceleration peak
# deceleration peak

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
