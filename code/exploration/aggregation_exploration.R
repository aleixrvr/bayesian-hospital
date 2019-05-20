# min date: 2100-06-07
# max date: 2209-08-07
source("code/utils/stats_funciton.R")
require(ggplot2)

## How much staff is present /treatments given each day over the years on average
dist_day <- calc_stats(window_start="2100-06-07",window_end="2209-08-07", 
                           time_aggr_1 = "ISOYEAR", time_aggr_2 = "DAYOFYEAR", unit_aggr = T)
dist_day[, .(avg_staff = mean(staff)), by=CURR_CAREUNIT]
ggplot(dist_day, aes(CURR_CAREUNIT,staff)) +  geom_boxplot() + theme_bw() +
    labs(x="Care Unit", y= "Number of Staff", title="Distribution of Staff")
ggplot(dist_day, aes(CURR_CAREUNIT,treatments)) +  geom_boxplot() + theme_bw() +
    labs(x="Care Unit", y= "Number of Treatments", title="Distribution of Treatments")

## How much staff is present on average
dist_mwd <- calc_stats(window_start="2101-01-01",window_end="2208-12-31", 
                       time_aggr_1 = "MONTH", time_aggr_2 = "DAYOFWEEK", unit_aggr = T)
dist_mwd[, .(avg_staff = mean(staff)), by=CURR_CAREUNIT]
ggplot(dist_mwd, aes(CURR_CAREUNIT,staff)) +  geom_boxplot() + theme_bw() +
    labs(x="Care Unit", y= "Number of Staff", title="Distribution of Staff on each Day")
