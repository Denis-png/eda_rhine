source('./code/sourcecode.r')

#Question 2

precip_day$year <- precip_day[,year(date)]
precip_stats <- data.table(year = unique(precip_day$year), mean_year = precip_day[,mean(value), by = year])
precip_stats[,2] <- NULL
colnames(precip_stats) <- c('year','mean_year')
precip_stats # a new dataset we're gonna plot in regression plot

#Regression
ggplot(precip_stats, aes(x = year, y = mean_year)) +
  geom_point() +
  geom_smooth(method = 'loess', formula = y ~ x, se = 0, col = colset_4[1]) +
  scale_color_manual(values = colset_4[c(2, 3, 4)]) +
  xlab(label = "Year") +
  ylab(label = "Precipitacion") 

#Formatting date to seasons
precip_day$month <- precip_day[,month(date)]
precip_day[month >11 | month <3, season := factor('winter')]
precip_day[month >2 & month < 6, season := factor('spring')]
precip_day[month >5 & month < 9, season := factor('summer')]
precip_day[month >8 & month < 12, season := factor('autmn')]
precip_day
#Boxplot
ggplot(precip_day, aes(season, value, fill = season)) +
  geom_boxplot() +
  xlab(label = "Season") +
  ylab(label = "Precipitation")
