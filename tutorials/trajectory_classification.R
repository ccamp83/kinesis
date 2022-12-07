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
