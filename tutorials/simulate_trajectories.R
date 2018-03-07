libraries()

# generate index and thumb trajectories
dig <- generate.trajectory(start=c(200,-300,0), end=c(0,0,400), phi=pi/4, ap=1000, MT=1500, r=40)
ggplot() + geom_point(aes(indexXraw, indexZraw), data=dig, color='red') +
  geom_point(aes(thumbXraw, thumbZraw), data=dig, color='blue') + coord_fixed()
plot3d(dig[,c(1,3,2)], col='red', radius=4, type='s')
spheres3d(dig[,c(4,6,5)], col="blue", radius=4)

# generate only index trajectory
dig <- generate.trajectory(start=c(0,0,0), end=c(8,8,0), phi=pi/4, ap=50, MT=1500, r=0, index.only = T)
ggplot() + geom_point(aes(indexXraw, indexYraw), data=dig, color='red') + coord_fixed()

# can be used with the other function of the kinesis package too:
dig <- generate.trajectory(start=c(200,-300,0), end=c(0,0,-400), phi=pi/4, ap=1000, MT=1500/11, r=40)
# note the negative sign of the final z coord since kin.SmoothAndDerive will multiply z coord by -1
# also note that MT was scaled by the refresh time (11ms in this case) to simulate optotrak sampling
dig$trialN <- 1
dig <- data.check(dig) # next line is a bogus subjName (testName) working as input to data.check |=> testName is not a function!!!
testName
dig <- kin.SmoothAndDerive(dig)
head(dig)
kinDig <- kin.extract(dig)
view.single.trial(1, dig)
view.single.trial.GA(1, dig, kinDig)
view.single.trial.grasp3d.anim(1, dig, kinDig, type='s')

