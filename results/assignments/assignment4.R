source('./code/sourcecode.r')

#Question 1
year_thres <- 2000

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


