library(kinesis)
library(ggplot2)
theme_set(theme_bw())

data(motiveData)
names(motiveData)
ggplot(motiveData, aes(time_sec, LFinger_position_z)) +
  geom_point()

# smoothing spline
motiveData$indexZ.ss <-
with(motiveData,
     kin.ssFilter(
       x = time_sec,
       y = LFinger_position_z,
       spar = 5e-2,
       deriv = 0
     )
     )

# butterworth 5Hz
motiveData$indexZ.bw5 <-
  with(motiveData,
       kin.bwFilter(
         s = LFinger_position_z,
         n = 2,
         cutoff_freq = 5,
         type = "low"
       )
  )

# butterworth 10Hz
motiveData$indexZ.bw10 <-
  with(motiveData,
       kin.bwFilter(
         s = LFinger_position_z,
         n = 2,
         cutoff_freq = 10,
         type = "low"
       )
  )

# savitzky-golay
motiveData$indexZ.sg <-
  with(motiveData,
       kin.sgFilter(
         x = LFinger_position_z,
         p = 4,
         m = 0,
         ts = time_sec
       )
  )

# whole trajectory comparisons
results.g <- ggplot(motiveData) +
  geom_line(aes(time_sec, LFinger_position_z), linewidth = 1) +
  geom_line(aes(time_sec, indexZ.ss), color = "turquoise", linewidth = 1) +
  geom_line(aes(time_sec, indexZ.bw5), color = "red", linewidth = 1) +
  geom_line(aes(time_sec, indexZ.bw10), color = "darkred", linewidth = 1) +
  geom_line(aes(time_sec, indexZ.sg), color = "grey", linewidth = 1)

results.g

# zooming in on start of forward movement
results.g +
  coord_cartesian(xlim = c(2.3,2.6), ylim = c(-.0975,-.082))

# zooming in on when maximum distance is reached
results.g +
  coord_cartesian(xlim = c(4.1,4.6), ylim = c(.319,.323))

# zooming in on end of return movement
results.g +
  coord_cartesian(xlim = c(8.15,8.5), ylim = c(-.107,-.0975))
