devtools::install_github("ccamp83/kinesis")
devtools::install_github("ccamp83/mu")
library(kinesis)
library(mu)
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

# Generate a noisy signal
x <- sin(seq(0, 10 * pi, length.out = 1000)) +
     sin(seq(0,  2 * pi, length.out = 1000))

# Filter the noisy signal using a Butterworth filter of order 6 and cutoff frequency 0.1
filtered_x <- butterworth_filter(x, 4, 0.6)
filtered_x <- kin.bwFilter(x,
                           n = 4,
                           cutoff_freq = 250,
                           type = "low")
ggplot() +
  geom_line(aes(1:1000, x)) +
  geom_line(aes(1:1000, c(filtered_x)), color = "red")

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


