#Packages
library(data.table)
library(ggplot2)



#Datasets
runoff_summary <- readRDS('data/runoff_summary.rds')
runoff_stats <- readRDS('data/runoff_stats.rds')
runoff_month <- readRDS('data/runoff_month.rds')
runoff_summer <- readRDS('data/runoff_summer.rds')
runoff_winter <- readRDS('data/runoff_winter.rds')
runoff_year <- readRDS('data/runoff_year.rds')
precip_day <- readRDS('data/precip_day.rds')
runoff_year_key <- readRDS('data/runoff_year_key.rds')
runoff_month_key <- readRDS('data/runoff_month_key.rds')
runoff_day <- readRDS('data/runoff_day.rds')
#Variables
colset_4 <-  c("#D35C37", "#BF9A77", "#D6C6B9", "#97B8C2")
theme_set(theme_bw())


