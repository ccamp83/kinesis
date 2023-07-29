# data.check

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

