library(data.table)
library(ggplot2)

runoff_stations <- fread('./data/raw/runoff_stations.csv')

runoff_stations[, sname := factor(abbreviate(station))]
runoff_stations[, id := factor(id)]
runoff_stations[, altitude := round(altitude, 0)]

runoff_stations_new <- runoff_stations[,.(sname, area, altitude)]
runoff_stations_new$size <- runoff_stations_new[,.(area/altitude)]

ggplot(data = runoff_stations_new, aes(x = area, y = altitude, color = size)) +
  geom_point() +
  geom_text(aes(label=sname),hjust=0, vjust=0)

runoff_stations[, lat := round(lat, 3)]
runoff_stations[, lon := round(lon, 3)]

runoff_stations_very_new <- runoff_stations[,.(sname, lon, lat)]
runoff_stations_very_new$altitude <- runoff_stations_new[,altitude]

ggplot(data = runoff_stations_very_new, aes(x = lon, y = lat, color = altitude)) +
  geom_point()+
  geom_text(aes(label=sname),hjust=0, vjust=0)
