# load required libraries ----
libraries()

#### BASIC FIXES TO DATASET ####
### restoring missing columns ----
head(rtgData_bad) # dataset provided by this package
rtgChecked <- data.check(rtgData_bad) # subjName is missing. When asked to type the subject name, run the next line as is (no quotes)
test_subject
head(rtgChecked)

### time.unit # TODO - check this!!! doesn't work ----
rtgData <- data.check(rtgData) # dataset provided by this package
# time column in rtgData is in milliseconds. Note that data.check allows to specify different time units as well
head(rtgData)

# instead, should the dataset have time in seconds
# the function will return frameT as a vector of NAs
data(rtgData) # reload dataset
rtgData$time <- rtgData$time / 1000 # change time to seconds
rtgData <- data.check(rtgData)
rtgData$frameT # always check that frameT looks good

# use time.unit to fix it
data(rtgData) # reload dataset
rtgData$time <- rtgData$time / 1000 # change time to seconds
rtgData <- data.check(rtgData, time.unit = 1)
rtgData$frameT
