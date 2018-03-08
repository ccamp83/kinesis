libraries()

# PROCEDURE FROM GALLIVAN  and CHAPMAN 2014

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
max(testTrial$framesOccluded) # 18

# repair missing frames
testTrial$thumbX <- with(testTrial, kin.smooth.repair(frameN,thumbXraw, lam = 10e-18, maxFrames= 20, fingersOccluded=fingersOccluded, framesOccluded=framesOccluded))
ggplot(aes(frameN, thumbX, color = fingersOccluded), data = testTrial) + geom_point() # thumb data is bad

##   1.1.2 butterworth filter ----
# dual pass, 8–12 Hz cutoff, 2nd order
testTrial$thumbXbw <- with(testTrial, kin.bwFilter(thumbX, cutoff_freq = 10, type = "pass"))

ggplot(data = testTrial) +
  geom_point(aes(frameN, thumbX), color = "black") +
  geom_point(aes(frameN, thumbXbw), color = "red")

##   1.1.3 translate and rotate to have all trajectories going in the same direction ----
##   1.1.4 find movement onset ----
#    1.1.4.1 calculate velocity vector ----
#    1.1.4.2 butterworth filter velocitiy vector ----
# dual pass, 8–12 Hz cutoff, 2nd order
#    1.1.4.3 set onset frame ----
# set the onset frame to be the first of four consecutive vector velocity readings of greater than 20 mm/s
# in which there was a total acceleration of 20 mm/s2 across the four points.
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

