#Question 2
precipitation_day <- (5 * 24) / (100 * 100) # m/d

catchment <- 185000 * (1000^2) # m^2

runoff_day <- 2900 * 60 * 60 * 24 # m^3/d

avg_runoff_change <- precipitation * catchment # m^3/d

#Question 3

avg_runoff <- 2900 # m^3/s

rhine_length <- 1230 * 1000 # m

rhine_volume <- rhine_length * catchment # m^3

time_to_sea <- avg_runoff / rhine_volume # s
