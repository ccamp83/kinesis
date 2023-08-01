library(kinesis)

# original 3D signal
rtgDataFixed <- data.check(rtgData_bad)
signal <- rtgDataFixed[rtgDataFixed$trialN==50, c("thumbXraw","thumbYraw","thumbZraw","time")]
# parameters
signal.name <- "thumb"
start <- c(267,-332,-11.5)
end <- c(34.8,-53.4,-283)
refreshRate <- 1/85

head(signal)
head(kin.signal.analysis(rtgDataFixed, "thumb", start, end, deltaTime = refreshRate))
