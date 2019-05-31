source("code/utils/stats_function.R") 
require("tidyverse")
min_date <- "2100-06-07"
max_date <- "2209-08-07"

## How much staff is present /treatments given each day over the years on average
#dist_day <- calc_stats(min_date, max_date, time_aggregation = c("DATE"))
#dist_day[, .(avg_staff = mean(staff)), by=CURR_CAREUNIT]
#ggplot(dist_day, aes(CURR_CAREUNIT,staff)) +  geom_boxplot() + theme_bw() + labs(x="Care Unit", y= "Number of Staff", title="Distribution of Staff")
#ggplot(dist_day, aes(CURR_CAREUNIT,patients)) +  geom_boxplot() + theme_bw() + labs(x="Care Unit", y= "Number of Patients", title="Distribution of Patients")
#ggplot(dist_day, aes(CURR_CAREUNIT,treatments)) +  geom_boxplot() + theme_bw() + labs(x="Care Unit", y= "Number of Treatments", title="Distribution of Treatments")

## Is the length of stay dependent on the admission month
#dist_mwd <- calc_stats(min_date, max_date, time_aggregation = c("ISOYEAR","MONTH"))
#dist_mwd[, newdate := paste0(ISOYEAR,MONTH)] %>%  ggplot(aes(newdate, avg_los)) + geom_line() + facet_grid(CURR_CAREUNIT~.)

## Does time-shifting have an influence on distribution of staff/ patients over time
queries_list <- list()
times <- c("CHARTTIME", "CHARTTIME_SHIFT_1", "CHARTTIME_SHIFT_2", "CHARTTIME_SHIFT_3", "CHARTTIME_SHIFT_4", "CHARTTIME_SHIFT_5")
for(i in times){
    dist_shift <- calc_stats(min_date, max_date, time_aggregation = c("ISOYEAR","QUARTER","DATE"),time_shift = i)
    dist_shift[, newdate := paste0(ISOYEAR,QUARTER)]
    queries_list[[i]] <- dist_shift[,.(avg_staff = mean(staff), avg_treat = mean(treatments), avg_pat = mean(patients)), by=list(CURR_CAREUNIT,newdate)]
}

times <- c("CHARTTIME", "CHARTTIME_SHIFT_1", "CHARTTIME_SHIFT_2", "CHARTTIME_SHIFT_3", "CHARTTIME_SHIFT_4", "CHARTTIME_SHIFT_5")
measures <- c("avg_staff", "avg_pat", "avg_treat","avg_los")
for(i in times){
    ggplot(queries_list[[i]], aes(newdate,avg_staff, group = CURR_CAREUNIT)) +  geom_line() + 
        labs(x="Quarterly Aggregation", y= "Number of Staff", title=paste("Distribution of Staff over time -", tolower(i))) + 
        facet_grid(CURR_CAREUNIT~.)  + theme(axis.text.x = element_blank(),axis.ticks = element_blank())
    ggsave(paste0("code/exploration/Plots/Staff_", i,".png"), plot = last_plot(), device = png(),width = 6, height = 4, units = 'in')
    
    ggplot(queries_list[[i]], aes(newdate,avg_pat, group = CURR_CAREUNIT)) +  geom_line() + 
        labs(x="Quarterly Aggregation", y= "Number of Patients", title=paste("Distribution of Patients over time -", tolower(i))) + 
        facet_grid(CURR_CAREUNIT~.)  + theme(axis.text.x = element_blank(),axis.ticks = element_blank())
    ggsave(paste0("code/exploration/Plots/Patients_", i,".png"), plot = last_plot(), device = png(),width = 6, height = 4, units = 'in')
    
    ggplot(queries_list[[i]], aes(newdate,avg_treat, group = CURR_CAREUNIT)) +  geom_line() + 
        labs(x="Quarterly Aggregation", y= "Number of Treatments", title=paste("Distribution of Treatments over time -", tolower(i))) + 
        facet_grid(CURR_CAREUNIT~.)  + theme(axis.text.x = element_blank(),axis.ticks = element_blank())
    ggsave(paste0("code/exploration/Plots/Treatments_", i,".png"), plot = last_plot(), device = png(),width = 6, height = 4, units = 'in')
}
