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

