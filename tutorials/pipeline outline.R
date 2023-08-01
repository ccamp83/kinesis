# import dataset

# prepping the dataset
#   identify base columns, specs columns and markers columns
#   ensure that the names of the columns with the markers' data follows the structure markernameXraw
#   review/add base and specs columns (subjName, condition, time et cetera)

# kin.getDataCols & kin.setDataCols
# use these to prep data.check

# data.check

# create baseCols, specsCols, markersCols
# reorder dataset

# trial loop
# set start, end, refreshrate

# kin.signal.analysis
#   check that time column is present
#   check that x, y, z columns are present
#     if one is not found, create it with all values set to zero
#   kin.signal.repair
#     check is NAs are present
#       if no NAs found
#         kin.signal.missing (check if there are frames where the position is static and turn them to NAs)
#       if NAs are found
#         find all segments containing NAs
#         repair segments whose length is < maxFrames (uses sreg to predict)
#   kin.ssFilter
#     filter signal using spline
#     TODO: choose between spline, savitkzy-golay, butterworth
#   translate (subtract start position)
#   kin.rotate.trajectory
#   calculate velocity and acceleration
#     calculate x, y, z components through derivation (uses kin.ssFilter)
#     calculate velocity resultant
#     calculate acceleration resultant through derivation (uses kin.ssFilter)
#   returns the rotated vectors, velocities and acceleration vectors, time

# set onset_condition
# crop

# set offset_condition
# crop

# if grasp data as well
#   kin.grasp.analysis -> cbind output to trial data

# rbind trial data to all trials dataset (eg trajData)

# kin.extract.parameters

# tidy up reach data, grasp data, time info data
#   add base and specs cols

# add reach, grasp and time data to their main datasets



