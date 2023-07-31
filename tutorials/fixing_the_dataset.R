#### BASIC FIXES TO DATASET ####
library(kinesis)

### setting dataset columns ----
# five columns are expected
get("dataCols", kinesis_parameters)
# user can set them
kin.setDataCols(subjName = "subjID", frameN = "frame", time = "t", deltaTime = "refreshTime")
get("dataCols", kinesis_parameters)
# they can be reset by calling the setter function empty
kin.setDataCols()
get("dataCols", kinesis_parameters)

### simple dataset check ----
data(rtgData_bad) # dataset provided in this package
data.check(rtgData_bad, check.only = T)

### restoring missing columns ----
head(rtgData_bad) # dataset provided in this package
rtgChecked <- data.check(rtgData_bad) # subjName is missing. When asked to type the subject name, run the next line as is (no quotes)
test_subject
head(rtgChecked)

### time.unit ----

# calculation of deltaTime borrows measurement unit from time, if time exists
# example 1
data(rtgData) # dataset provided in this package
head(rtgData) # notice time in milliseconds
rtgData <- data.check(rtgData)
head(rtgData) # deltaTime is calculated also in milliseconds
# example 2
data(rtgData) # reload dataset
rtgData$time <- rtgData$time / 1000 # change time to seconds
rtgData <- data.check(rtgData)
head(rtgData) # deltaTime is calculated also in seconds

# create time and deltaTime with a custom unit
# example 1
data(rtgData) # reload dataset
rtgData <- rtgData[names(rtgData)!="time"] # remove time
rtgData <- data.check(rtgData, time.unit = 1)
head(rtgData) # time and deltaTime are in seconds
# example 2
data(rtgData) # reload dataset
rtgData <- rtgData[names(rtgData)!="time"] # remove time
rtgData <- data.check(rtgData, time.unit = 1000)
head(rtgData) # time and deltaTime are in milliseconds
