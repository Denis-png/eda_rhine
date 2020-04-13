source('./code/sourcecode.r')

#Question 1
year_thres <- 2000
runoff_year_key
runoff_year_key[year < year_thres, period := factor('pre_2000')]
runoff_year_key[year >= year_thres, period := factor('aft_2000')]
runoff_month_key[year < year_thres, period := factor('pre_2000')]
runoff_month_key[year >= year_thres, period := factor('aft_2000')]

to_plot <- rbind(cbind(runoff_year_key, season = factor('year')), 
                 cbind(runoff_month_key, season = factor('month')),fill=TRUE) 

ggplot(to_plot, aes(season, value, fill = period)) +
  geom_boxplot() +
  facet_wrap(~sname, scales = 'free_y') +
  scale_fill_manual(values = colset_4[c(4, 1)]) +
  xlab(label = "Season") +
  ylab(label = "Runoff (m3/s)") +
  theme_bw()
#Annual runoff for BASR and KOEL stations increased after 2000, but decreased for DOMA

#Question 2
numberOfDays <- function(date) {
  m <- format(date, format="%m")
  
  while (format(date, format="%m") == m) {
    date <- date + 1
  }
  
  return(as.integer(format(date - 1, format="%d")))
}
quantiles <- runoff_day[,.(quantile(value, c(0.1)),quantile(value, c(0.9))), by = sname] # counting higl and low quantiles

runoff_day_new <- merge(runoff_day, quantiles, by = 'sname')
runoff_day_new[value > V2, quantile:= factor('high')]
runoff_day_new[value <= V2 & value >= V1, quantile:= factor('normal')]
runoff_day_new[value < V1, quantile:= factor('low')]
runoff_day_new$V1 <- NULL
runoff_day_new$V2 <- NULL
runoff_day_new
ggplot(runoff_day_new, aes(x = season, y = date, fill = season)) +
ggplot(runoff_day_new, aes(x = season, y = numberOfDays(date))) +
  geom_boxplot() +
  facet_wrap(~quantile)

#Question 3
dt <- runoff_summary[, .(sname, area, category)]
runoff_stats_short <- runoff_day[year <= 2010, .(sname, mean_day = round(mean(value)), 0), by = sname] # counting new means
to_plot <- runoff_stats_short[dt, on = 'sname']

ggplot(to_plot, aes(x = mean_day, y = area)) +
  geom_point(aes(col = category), cex = 3) +
  geom_smooth(method = 'loess', formula = y ~ x, se = 0, col = colset_4[1]) +
  scale_color_manual(values = colset_4[c(2, 3, 4)]) +
  xlab(label = "Area (km3)") +
  ylab(label = "Runoff (m3/s)") +
  theme_bw()
#We can notice that nothing has changed, although mean value in new
ggplot(to_plot, aes(x = mean_day, y = area)) +
  geom_point(aes(col = category), cex = 3) +
  geom_smooth(method = 'lm', formula = y ~ x, se = 0, col = colset_4[1]) +
  scale_color_manual(values = colset_4[c(2, 3, 4)]) +
  xlab(label = "Area (km3)") +
  ylab(label = "Runoff (m3/s)") +
  theme_bw()
#Linear model became less accurate