# kinesis
R package to analyse kinematic trajectories of reaching and grasping movements
=======
This package provides a set of functions to preprocess 3D human kinematic data, such as motion capture recordings of reaching and grasping movements. The package has been specifically developed using positional data from readouts of Optotrak Certus, Optitrack (Motive), HTC Vive.  

## Installation
```{r}
# install from github
devtools::install_github("ccamp83/kinesis")

# load
library(kinesis)
```

## Preparing the dataset
The kinesis package expects a dataset in long format, containing at minimum the following five columns: 

- subjName: participant / agent identifier

- trialN: counter of the reach/grasp movements within the dataset

- frameN: counter of the samples within each reach/grasp movement

- deltaTime: each frame's duration (in sec / millisec et cetera) - ie the refresh rate

- time: elapsed time (in sec / millisec et cetera) - ie the by-trial cumulative sum of deltaTime

### kin.getDataCols & kin.setDataCols
The names of these five columns can be handled via two functions

```{r setup, include=T, echo=T}
# retrieve the names of the mandatory five columns expected in the dataset
kin.getDataCols()

# user can customize their names using kin.setDataCols()
kin.setDataCols(deltaTime = "refreshRate")

# checking that the column names have been updated
kin.getDataCols()
```

### data.check
The function data.check scans the dataset for the above five columns and can create them automatically if they don't exist:

- if subjName is missing, the user is asked to type an identifier on the console

- trialN is set to 1

- frameN is set to the row number

- deltaTime is set to 0.01176471 (85 Hz)

- time is calculated from deltaTime

### Dataset prepping tutorial

The following [tutorial](tutorials/fixing_the_dataset.R) illustrates these functionalities.

## Trajectory data preprocessing

Preprocessing of trajectory data should be done on a trial-by-trial basis.
Once the overall dataset has been correctly prepped (as per the above section), data from a single trial should be subset and then analyzed through the following steps:

- set start and end positions of the reach/grasp: this information allows the package to accurately estimate x, y and z deviations of the movement relative to the start-end straight line. Both are specified as 3D vectors (x, y, z coordinates of start position and end position)

```{r}
start <- c(0,0,.2)     # when at start position, the hand is horizontally aligned with the center of the body (x = 0), resting on the tabletop (y = 0) and 20 cm away from the body (z = .2 - in metres)
end <- c(0, 0, .4)     # the target is located along the line of sight and resting on the tabletop (x and y = 0) and 40 cm away from the body
```
- set refreshRate: this is one over the sampling frequency in Hz (eg. if the sampling frequency is 120 Hz, refreshRate is 1/120). This information is used to accurately derive positional data with respect to time to calculate velocity and acceleration profiles.

- run kin.signal.analysis using the above information. This function performs the following steps in this order: 
1) Repair (check for missing frames up to a custom threshold and replace with spline prediction) 
2) Smooth (using one of three possible filters: spline, Butterworth - default, Savitzky-Golay)
3) Translate (subtract start position from positional data)
4) Rotate (align the start-end straight line with the line of sight)
5) Derive - in three steps:
  * calculates x, y, z components through derivation (uses spline prediction)
  * calculates velocity resultant through trigonometry
  * calculates acceleration resultant through derivation (uses spline prediction)
  
## Kinematic features extraction

After preprocessing, the kinesis package enables extracting specific features from the kinematics of reaching and grasping data that allow descriptive and inferential statistical analysis of these movements.
The procedure for features extraction follows these steps:

- 
