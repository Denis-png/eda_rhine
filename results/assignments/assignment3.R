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

runoff_summary <- readRDS('./data/runoff_summary.rds')
runoff_month <- readRDS('./data/runoff_month.rds')
to_merge <- runoff_summary[,.(sname,runoff_class)]
runoff_month_new <- merge(runoff_month, to_merge, by = 'sname')
ggplot(runoff_month_new, aes(x = factor(month), y = value, fill = runoff_class)) +
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

#Explorer question 4


runoff_winter <- readRDS('./data/runoff_winter.rds')
runoff_summer <- readRDS('./data/runoff_summer.rds')
runoff_month <- readRDS('./data/runoff_month.rds')
runoff_year <- readRDS('./data/runoff_year.rds')

runoff_summer_TM1 <- runoff_summer[,min(value),by = sname]
runoff_summer_TM2 <- runoff_summer[,max(value),by = sname]
runoff_summer_to_merge <- merge(runoff_summer_TM1, runoff_summer_TM2, by = 'sname')
colnames(runoff_summer_to_merge) <- c('sname','min', 'max')
runoff_summer_to_merge
runoff_summer
runoff_summer_minmax <- merge(runoff_summer, runoff_summer_to_merge, by = 'sname')
runoff_summer_minmax
runoff_summer_max_final <- runoff_summer_minmax[runoff_summer_minmax$value == runoff_summer_minmax$max] 
runoff_summer_min_final <- runoff_summer_minmax[runoff_summer_minmax$value == runoff_summer_minmax$min] 

ggplot(data = runoff_summer_max_final, aes(x = sname, y = max, label = year)) +
  geom_point() +
  geom_text(aes(label=year),hjust=0, vjust=0) 

ggplot(data = runoff_summer_min_final, aes(x = sname, y = min, label = year)) +
  geom_point() +
  geom_text(aes(label=year),hjust=0, vjust=0) 

runoff_w_TM1 <- runoff_winter[,min(value),by = sname]
runoff_w_TM2 <- runoff_winter[,max(value),by = sname]
runoff_w_to_merge <- merge(runoff_w_TM1, runoff_w_TM2, by = 'sname')
colnames(runoff_w_to_merge) <- c('sname','min', 'max')

runoff_w_minmax <- merge(runoff_winter, runoff_w_to_merge, by = 'sname')

runoff_w_max_final <- runoff_w_minmax[runoff_w_minmax$value == runoff_w_minmax$max] 
runoff_w_min_final <- runoff_w_minmax[runoff_w_minmax$value == runoff_w_minmax$min] 

ggplot(data = runoff_w_max_final, aes(x = sname, y = max, label = year)) +
  geom_point() +
  geom_text(aes(label=year),hjust=0, vjust=0) 

ggplot(data = runoff_w_min_final, aes(x = sname, y = min, label = year)) +
  geom_point() +
  geom_text(aes(label=year),hjust=0, vjust=0) 

runoff_m_TM1 <- runoff_month[,min(value),by = sname]
runoff_m_TM2 <- runoff_month[,max(value),by = sname]
runoff_m_to_merge <- merge(runoff_m_TM1, runoff_m_TM2, by = 'sname')
colnames(runoff_m_to_merge) <- c('sname','min', 'max')

runoff_m_minmax <- merge(runoff_month, runoff_m_to_merge, by = 'sname')

runoff_m_max_final <- runoff_m_minmax[runoff_m_minmax$value == runoff_m_minmax$max] 
runoff_m_min_final <- runoff_m_minmax[runoff_m_minmax$value == runoff_m_minmax$min] 

ggplot(data = runoff_m_max_final, aes(x = sname, y = max, label = year)) +
  geom_point() +
  geom_text(aes(label=year),hjust=0, vjust=0) 

ggplot(data = runoff_m_min_final, aes(x = sname, y = min, label = year)) +
  geom_point() +
  geom_text(aes(label=year),hjust=0, vjust=0) 

runoff_y_TM1 <- runoff_year[,min(value),by = sname]
runoff_y_TM2 <- runoff_year[,max(value),by = sname]
runoff_y_to_merge <- merge(runoff_y_TM1, runoff_y_TM2, by = 'sname')
colnames(runoff_y_to_merge) <- c('sname','min', 'max')

runoff_y_minmax <- merge(runoff_year, runoff_y_to_merge, by = 'sname')

runoff_y_max_final <- runoff_y_minmax[runoff_y_minmax$value == runoff_y_minmax$max] 
runoff_y_min_final <- runoff_y_minmax[runoff_y_minmax$value == runoff_y_minmax$min] 
ggplot(data = runoff_y_max_final, aes(x = sname, y = max, label = year)) +
  geom_point() +
  geom_text(aes(label=year),hjust=0, vjust=0) 

ggplot(data = runoff_y_min_final, aes(x = sname, y = min, label = year)) +
  geom_point() +
  geom_text(aes(label=year),hjust=0, vjust=0) 


