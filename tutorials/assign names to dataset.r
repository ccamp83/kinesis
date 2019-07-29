data("rtgData")

head(rtgData)

# required columns
reqColumns <- c("trialN",
                paste("signal",c("X","Y","Z"),"raw", sep = ""))
# optional columns
optColumns <- c("deltaTime","frameN","time")

rtgData2 <-
eval(substitute(
  ddply(rtgData, .(trialCol), summarise,
        testVar = max(thumbXraw)),
  list(trialCol = as.name(reqColumns[1]))
))

head(rtgData2)
