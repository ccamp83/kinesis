library(ggplot2)
library(cowplot)

############## From Taylor & Ivry, 2011

#### Standard model of sensorimotor adaptation ####

## ----------- Initial states ----------- ##

# Error definitions
# error based on distance from feedback to target (target error)
e.target <- function(rot_n, rot.est_n) { rot_n - rot.est_n }
# error based on distance from feedback to aimed position
e.aiming <- function(rot_n, rot.est_n, s.actual_n, s.desired_n) { (rot_n - rot.est_n) + (s.actual_n - s.desired_n) }

# feed-forward model update rule
ffmod.update <- function(A, B, rot.est_n, e.targ_n) { A*rot.est_n + B*e.targ_n }

# memory term (retention)
A = .38
# learning rate / sensitivity to error
B = .012

## ----------- Simulate data ----------- ##

# baseline
baseline <- expand.grid(trial = 1:120,
                        rot_n = 0)
# adaptation
rotation_theta <- -30
adaptation <- expand.grid(trial = 120+(1:320),
                          rot_n = rotation_theta)
# washout
washout <- expand.grid(trial = 120+320+(1:120),
                       rot_n = 0)
# complete dataset
simData <- rbind(baseline, adaptation, washout)
# initial internal state is set to zero
simData$rot.est <- c(0, rep(NA, nrow(simData)-1))
# initialize error
simData$error <- rep(NA, nrow(simData))
# compute trial-by-trial update
for(tr in 1:nrow(simData))
{
  if(tr < nrow(simData))
  {
    # current error
    simData[simData$trial==tr,"error"] <- with(subset(simData, trial==tr), e.target(rot_n, rot.est))
    # model update
    simData[simData$trial==tr+1,"rot.est"] <- with(subset(simData, trial==tr), ffmod.update(A, B, rot.est, error))
  }
}
head(simData)
ggplot(aes(trial, rot.est), data = simData) + geom_line() + geom_point() + coord_cartesian(ylim = c(-rotation_theta, rotation_theta))
ggplot(aes(trial, error), data = simData) + geom_line() + geom_point() + coord_cartesian(ylim = c(-rotation_theta, rotation_theta))

## ----------- Fit data ----------- ##
simData$prev_rot.est <- c(NA, simData$rot.est[1:(nrow(simData)-1)])
# head(simData[simData$rot_n!=0,])
nls(formula = rot.est ~ A*prev_rot.est + B*error, data = simData, start = c(A = 0, B = 0))

