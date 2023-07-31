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

## Dataset format
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

The following [tutorial](tutorials/fixing_the_dataset.R) illustrates these steps.
