options(scipen = 1, digits = 3)

library(cowplot)

# PROCEDURE FROM GALLIVAN and CHAPMAN 2014

#### Prepare the dataset ####
# five columns are expected
get("dataCols", kinesis_parameters)
# user can set them
kin.setDataCols(deltaTime = "refreshRate")
get("dataCols", kinesis_parameters)
# Fix dataset
testData <- data.check(reachData)

#### 1. individual trial analysis ####
###  1.1 extract ROI ----
##   1.1.1 fill in missing frames ----
# inpaint_nans function in matlab

# select one trial with missing frames
testTrial <- subset(testData, trialN == 50)
testTrial$handY <- 0
# testTrial <- subset(testData, trialN == badTrialNum)
# plot the data
# ggplot(aes(frameN, handX), data = testTrial) + geom_point() # handX data is good

# check for missing frames
testTrial$handXrep <- with(testTrial, kin.signal.repair(handX, maxFrames= 20))
testTrial$handYrep <- with(testTrial, kin.signal.repair(handY, maxFrames= 20))
testTrial$handZrep <- with(testTrial, kin.signal.repair(handZ, maxFrames= 20))

##   1.1.2 filter ----
#    Savitzky-Golay filter ----
# 3rd order
testTrial$handXsg <- with(testTrial, kin.sgFilter(handXrep, ts = refreshRate))
testTrial$handYsg <- with(testTrial, kin.sgFilter(handYrep, ts = refreshRate))
testTrial$handZsg <- with(testTrial, kin.sgFilter(handZrep, ts = refreshRate))

# ggplot(data = testTrial) +
#   geom_point(aes(frameN, handXrep), color = "black") +
#   geom_point(aes(frameN, handXsg), color = "green", alpha=.5)

#    1.2.2.1 apply filter ----
testTrial$handX <- testTrial$handXsg
testTrial$handY <- testTrial$handYsg
testTrial$handZ <- testTrial$handZsg

##   1.1.3 translate and rotate to have all trajectories going in the same direction ----

# the trajectory is assumed to start at origin (0,0,0)
# and terminate at a point (0,0,z)
# with x axis: rightwards positive, leftwards negative
#      y axis: upwards positive, downwards negative
#      z axis: forwards positive, backwards negative

# this requires a translation followed by a rotation

# starting dataset to apply translations and rotations on recursively
handData.backup <- testTrial[,c("handX","handY","handZ")]
# if any of the column is missing, it is set to zero
# dataset containing to-be-rotated fingers positions
rotData.backup <- handData.backup

# define start and end of movement
# case unknown
# rationale: there will be many more samples around start and end of movement
# because of the low speed of motion
# hence the distribution of x, y and z positions should be highly bimodal
# we use cluster analysis to find the centroids of the two clusters (start & end)
# of each position of each finger

# CASE 1: whole grasp
# translate the trajectory to origin (0,0,0)
transData <- data.frame("handX" = 0, "handY" = 0, "handZ" = .1)
# matrix subtraction
rotData <- rotData.backup - transData[rep(1, nrow(rotData.backup)),]
# end coordinates of the whole grasp
end <- transData
# rotate trajectories
handData <- kin.rotate.trajectory(handData, end)
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
