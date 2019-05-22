source("code/utils/stats_function.R") 
require("tidyverse")
min_date <- "2100-06-07"
max_date <- "2209-08-07"

## How much staff is present /treatments given each day over the years on average
dist_day <- calc_stats(min_date, max_date, time_aggregation = c("DATE"))
dist_day[, .(avg_staff = mean(staff)), by=CURR_CAREUNIT]
ggplot(dist_day, aes(CURR_CAREUNIT,staff)) +  geom_boxplot() + theme_bw() + labs(x="Care Unit", y= "Number of Staff", title="Distribution of Staff")
ggplot(dist_day, aes(CURR_CAREUNIT,treatments)) +  geom_boxplot() + theme_bw() + labs(x="Care Unit", y= "Number of Treatments", title="Distribution of Treatments")

## How much staff is present on average
dist_mwd <- calc_stats(min_date, max_date, time_aggregation = c("ISOYEAR","MONTH"))
dist_mwd[, newdate := paste0(ISOYEAR,MONTH)] %>% 
    ggplot(aes(newdate, avg_los)) + geom_line() + facet_grid(CURR_CAREUNIT~.)




