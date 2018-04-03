# required columns
reqColumns <- c("trialN",
                paste("signal",c("X","Y","Z"),"raw", sep = ""))
# optional columns
optColumns <- c("deltaTime","frameN","time")

testData3 <-
eval(substitute(
  ddply(testData2, .(trialCol), summarise,
        max(thumbXraw)),
  list(trialCol = as.name(reqColumns[1]))
))

