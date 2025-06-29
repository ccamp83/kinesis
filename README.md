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

## Package Structure
The package is organized into several functional groups:

1. **Data Preparation**
   - `kin.getDataCols()` and `kin.setDataCols()`: Manage column names
   - `data.check()`: Validate and prepare datasets

2. **Signal Processing**
   - `kin.signal.analysis()`: Main preprocessing pipeline
   - Filter functions: `kin.bwFilter()`, `kin.sgFilter()`, `kin.ssFilter()`
   - `kin.signal.curvature()` and `kin.signal.arclength()`: Geometric analysis

3. **Grasp Analysis**
   - `kin.grasp.analysis()`: Analyze grip aperture and orientation
   - `kin.generate.trajectory()`: Generate synthetic trajectories

4. **Feature Extraction**
   - `kin.find.traj.landmark()`: Identify movement landmarks
   - `kin.extract.parameters()`: Extract kinematic features

5. **Visualization** (New!)
   - `kin.plot.trajectory()`: Visualize 3D trajectories
   - `kin.plot.velocity()`: Plot velocity profiles
   - `kin.plot.grasp()`: Visualize grip aperture

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
    * this [tutorial](tutorials/filters_comparison.R) compares these three filters
3) Translate (subtract start position from positional data)
4) Rotate (align the start-end straight line with the line of sight)
5) Derive - in three steps:
    1. calculates x, y, z velocity components through derivation (uses spline prediction)
    2. calculates velocity resultant through trigonometry
    3. calculates acceleration resultant through derivation (uses spline prediction)

- run kin.grasp.analysis (if applicable). This function takes two markers (index and thumb) and calculates 1) their Euclidean distance in 3D space ("Grip Aperture" or GA); 2) their middle distance in 3D space ("Grip Position" or GP); 3) the orientation of the segment connecting index to thumb in 3D space ("Grip Orientation" or GO). It also derives the GP to calculate its velocity and acceleration components as in point 5 of kin.signal.analysis.

## Kinematic features extraction

After preprocessing, the kinesis package enables extracting specific features from the kinematics of reaching and grasping data allowing descriptive and inferential statistical analysis of these movements.
The procedure for kinematic features extraction follows these steps:

- set a criterion for the movement onset then crop the data prior to that event. This is done by using kin.find.traj.landmark, which identifies the position of the first frame of the longest sequence of consecutive observations adhering to any criterion. For example, if the movement onset criterion is when the wrist velocity becomes greater than 2 cm/s, kin.find.traj.landmark will locate the longest sequence of consecutive frames where that criterion is met (there could be false starts, but the actual movement is usually the longest period of time spent moving) and returns the position of the first frame of that sequence (ie the movement onset time).

- set a criterion for the movement offset then crop the data after that event. This is done by using kin.find.traj.landmark in the same way as for the movement onset.

- run kin.extract.parameters. This functions returns an array of kinematic features or landmarks for each specified reaching signal (eg, index, thumb, wrist) and grasping signal (GP, GA, GO, if applicable) suitable for statistical analysis. The complete list follows here:

| Reaching | Grasping |
|---|---|
| FX (final X position) | FX (final grip position in X) |
| FY | FY |
| FZ | FZ |
| FXVel (final velocity component in X) | FXVel (final grip velocity component in X) |
| FYVel | FYVel |
| FZVel | FZVel |
| FVel (final velocity resultant) | FVel (final grip velocity resultant) |
| FAcc (final acceleration resultant) | FAcc (final grip acceleration resultant) |
| MVel (maximum velocity) | MVel (maximum grip velocity) |
| MAcc (maximum acceleration) | MAcc |
| MDec (maximum deceleration) | MDec |
| timeMVel (time to MVel) | timeMVel |
| timeMAcc | timeMAcc |
| timeMDec | timeMDec |
| pathLength (total length of the trajectory) | pathLength |
| Xmax (maximum deviation from straight path on X axis) | Xmax |
| Ymax | Ymax |
| Zmax | Zmax |
| timeToXmax | timeToXmax |
| timeToYmax | timeToYmax |
| timeToZmax | timeToZmax |
| XlocMinN (total local minima of the X position. diagnostic, ie how smooth vs jerky in X) | XlocMinN |
| YlocMinN | YlocMinN |
| ZlocMinN | ZlocMinN |
| XlocMaxN (total local maxima of the X position - as above) | XlocMaxN |
| YlocMaxN | YlocMaxN |
| ZlocMaxN | ZlocMaxN |
| timeMAccToMVel (time from max acceleration to max velocity) | timeMAccToMVel |
| timeMVelToMDec (time from max velocity to max deceleration) | timeMVelToMDec |
| timeMDecToOffset (time from max deceleration to movement offset) | timeMDecToOffset |
|  | FGA (final grip aperture) |
|  | MGA (maximum grip aperture) |
|  | timeMGA |
|  | timeMVelToMGA |
|  | timeMGAToMDec |
|  | timeMGAToOffset |
|  | MGAVel (grip velocity resultant at MGA time) |
|  | MGAAcc (grip acceleration resultant at MGA time) |
|  | FGOf (final grip orientation on the frontoparallel plane, in rad) |
|  | FGOt (final grip orientation on the transverse plane, in rad) |
|  | FGOs (final grip orientation on the sagittal plane, in rad) |

## Batch Processing
For analyzing multiple trials or participants, use the batch processing functions:

```{r}
# Process multiple trials in parallel
kin.batch.process(data, 
                 start_positions,
                 end_positions,
                 refresh_rate = 1/120,
                 n_cores = parallel::detectCores() - 1)

# Export results to various formats
kin.export.results(results, format = "csv")  # or "json", "excel"
```

## Contributing
Contributions are welcome! Please feel free to submit a Pull Request.

## Citation
If you use this package in your research, please cite:
```
@software{kinesis2024,
  author = {Camp, Carlo},
  title = {kinesis: R package for kinematic trajectory analysis},
  year = {2024},
  url = {https://github.com/ccamp83/kinesis}
}
```

