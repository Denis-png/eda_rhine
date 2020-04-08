library(data.table)
library(ggplot2)
library(mapview) 
library(sf)

#Question 1

runoff_stats <- readRDS('./data/runoff_stats.rds')
runoff_day <- readRDS('./data/runoff_day.rds')

runoff_median <- runoff_day[,.(round(median(value),0)), by = sname]
runoff_stats$median <- runoff_median$V1

colset_4 <-  c("MEAN", "MEDIAN", "MIN", "MAX")
shapeset_4 <- c(3, 8, 11, 14)
ggplot(runoff_stats) + 
  geom_point(aes(x= sname, y= mean_day, col=colset_4[1]), shape = shapeset_4[1]) +
  geom_point(aes(x= sname, y= median, col=colset_4[2]), shape = shapeset_4[2]) +
  geom_point(aes(x= sname, y= min_day, col=colset_4[3]), shape = shapeset_4[3]) +
  geom_point(aes(x= sname, y= max_day, col=colset_4[4]), shape = shapeset_4[4]) +
  labs(y="Runoff", x = "Station name", color = 'Runoff value type')

#Question 2

runoff_stats$skewness <- (runoff_stats$mean_day - runoff_stats$median) / runoff_stats$sd_day
runoff_stats$cv <- runoff_stats$sd_day / runoff_stats$mean_day
runoff_stats

runoff_stats_new <- data.table(skewness = (runoff_stats$mean_day - runoff_stats$median) / runoff_stats$sd_day, cv = runoff_stats$sd_day / runoff_stats$mean_day)
runoff_stats_new

#Question 3

#Can't solve this one. Didn't found the way how to assign runoff_class to runoff_month table to a new column.
runoff_summary <- readRDS('./data/runoff_summary.rds')
runoff_month <- readRDS('./data/runoff_month.rds')

runoff_month$runoff_class[sname == runoff_summary$sname] <- runoff_summary$runoff_class

ggplot(runoff_month, aes(x = factor(month), y = value, fill= runoff_class)) +
  facet_wrap(~sname, scales = 'free') +
  geom_boxplot()

#Question 4

ggplot(runoff_day, aes(x = sname, y = value)) +
  geom_boxplot() 

#We see that outliers for the stations with the highest altitude are the biggest.
#This happens because of the different amount of precipitation and snow on different altitudes

#Question 5

runoff_stations <- readRDS('./data/runoff_stations.rds')
runoff_stations[, area_class := factor('small')]
runoff_stations[area >= 10000 & area < 100000, area_class := factor('medium')]
runoff_stations[area >= 100000, area_class := factor('large')]

runoff_stations[, alt_class := factor('low')]
runoff_stations[altitude >= 10 & altitude < 100, alt_class := factor('medium')]
runoff_stations[altitude >= 100, alt_class := factor('high')]

runoff_stations$mean_day <- runoff_stats$mean_day

ggplot(runoff_stations, aes(x = mean_day, y = area, col = area_class, cex = alt_class)) +
  geom_point()



