library(kinesis)
library(ggplot2)
theme_set(theme_bw())
data("motiveData")
names(motiveData)
ggplot(motiveData, aes(time_sec, LFinger_position_z)) +
  geom_point()

motiveData$indexZ.ss <-
with(motiveData,
     kin.ssFilter(
       x = time_sec,
       y = LFinger_position_z,
       spar = .5,
       deriv = 0
     )
     )

motiveData$indexZ.bw <-
  with(motiveData,
       kin.bwFilter(
         s = LFinger_position_z,
         n = 2,
         cutoff_freq = 10,
         type = "low"
       )
  )

motiveData$indexZ.sg <-
  with(motiveData,
       kin.sgFilter(
         x = LFinger_position_z,
         p = 4,
         m = 0,
         ts = 1/120
       )
  )

ggplot(motiveData) +
  geom_line(aes(time_sec, LFinger_position_z), linewidth = 1) +
  geom_line(aes(time_sec, indexZ.ss), color = "turquoise", linewidth = 1) +
  geom_line(aes(time_sec, indexZ.bw), color = "darkred", linewidth = 1) +
  geom_line(aes(time_sec, indexZ.sg), color = "grey", linewidth = 1)

ggplot(motiveData) +
  geom_line(aes(time_sec, LFinger_position_z), linewidth = 1) +
  geom_line(aes(time_sec, indexZ.ss), color = "turquoise", linewidth = 1) +
  geom_line(aes(time_sec, indexZ.bw), color = "darkred", linewidth = 1) +
  geom_line(aes(time_sec, indexZ.sg), color = "grey", linewidth = 1) +
  coord_cartesian(xlim = c(2.3,2.65), ylim = c(-.1,-.075))

ggplot(motiveData) +
  geom_line(aes(time_sec, LFinger_position_z), linewidth = 1) +
  geom_line(aes(time_sec, indexZ.ss), color = "turquoise", linewidth = 1) +
  geom_line(aes(time_sec, indexZ.bw), color = "darkred", linewidth = 1) +
  geom_line(aes(time_sec, indexZ.sg), color = "grey", linewidth = 1) +
  coord_cartesian(xlim = c(8,8.5), ylim = c(-.105,-.05))
