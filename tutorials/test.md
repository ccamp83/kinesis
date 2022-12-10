# Dataset format
The kinesis package expects a dataset in long format, containing at least the following five columns: 

- subjName: participant / agent identifier

- trialN: counter of the reach/grasp movements within the dataset

- frameN: counter of the samples within each reach/grasp movement

- deltaTime: each frame's duration (in sec / millisec et cetera) - ie the refresh rate

- time: elapsed time (in sec / millisec et cetera) - ie the cumulative sum of deltaTime

## this is a test file
### nothing else to see here
