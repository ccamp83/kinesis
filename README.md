# kinesis
R package to analyse kinematic trajectories of reaching and grasping movements
=======
This package provides a set of functions to preprocess 3D human kinematic data, such as motion capture recordings of reaching and grasping movements. The package has been specifically developed using positional data from readouts of an Optotrak Certus and a HTC Vive.  

## Dataset format
The kinesis package expects a dataset in long format, containing at minimum the following five columns: 

- subjName: participant / agent identifier

- trialN: counter of the reach/grasp movements within the dataset

- frameN: counter of the samples within each reach/grasp movement

- deltaTime: each frame's duration (in sec / millisec et cetera) - ie the refresh rate

- time: elapsed time (in sec / millisec et cetera) - ie the by-trial cumulative sum of deltaTime



```{r setup, include=FALSE, echo=FALSE}
ssss
```
