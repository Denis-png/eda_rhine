library(data.table)
library(ggplot2)

#Question 2

runoff_stations <- readRDS('./data/runoff_stations.rds')
runoff_day <- readRDS('./data/runoff_day.rds')

average_catchment <- mean(runoff_stations[,area])
average_catchment
average_runoff <- mean(runoff_day[,value])
average_runoff

#Question 3

runoff_station_avg <- data.table(aggregate(runoff_day[,4],list(runoff_day$sname), mean))

colnames(runoff_station_avg) <- c('Name','Value')

runoff_station_avg

ggplot(data = runoff_station_avg, aes(x = Value, y = Name, color = Name))+
  geom_point()

