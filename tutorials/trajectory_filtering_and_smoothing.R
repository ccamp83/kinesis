library(kinesis)
mu::mu.library("cowplot")

# Fix dataset
testData <- data.check(rtgData)
testData$trialN <- testData$trialN+1

trajData <- NULL
for(i in unique(testData$trialN))
{
  # i <- 1
  temp <- subset(testData, trialN == i)
  frameT0 <- min(temp$frameN[temp$time == min(temp$time)])
  temp$time[temp$frameN < frameT0] <- temp$time[temp$frameN < frameT0] - max(temp$time[temp$frameN < frameT0])
  trajData <- rbind(trajData, temp)
}

t1 <- subset(testData, trialN == 2)
head(t1)
ggplot(aes(time, indexZraw), data = t1) +
  geom_point()

4/85

t1$indexZBw <- kin.bwFilter(t1$indexZraw,
                            n = 4, cutoff_freq = 4, type = "low")
ggplot(data = t1) +
  geom_line(aes(time, indexZraw)) +
  geom_line(aes(time, indexZBw), color = "red") +
  coord_cartesian(xlim = c(500, 2500), ylim = c(-390, -360))

freqz(t1$indexZraw)
freqz(t1$indexZBw)

1000 / median(t1$deltaTime, na.rm = T)
1000 / (median(t1$deltaTime, na.rm = T) * 2)


