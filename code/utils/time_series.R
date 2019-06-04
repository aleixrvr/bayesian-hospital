#source("code/utils/stats_function.R") 
source("../utils/stats_function.R") 
require(tidyverse, quietly = TRUE)
require(lubridate, quietly = TRUE)

#min_date <- as.Date("2100-06-07")
#max_date <- as.Date("2209-08-07")
#objective <- "avg_los"
#shift <- "CHARTTIME"
#time_window <- c(min_date,max_date)
#aggregation <- "QUARTER"

plot_timeseries <- function(objective, shift, time_window, aggregation){
    dist_shift <- calc_stats(time_window[1], time_window[2], time_aggregation = c("ISOYEAR",aggregation,"DATE"),time_shift = shift)
    dist_shift[, newdate := paste0(ISOYEAR,get(aggregation))]
    dist_shift <- dist_shift[,.(avg_staff = mean(staff), avg_treat = mean(treatments), avg_pat = mean(patients), avg_los = mean(avg_los)), by=list(CURR_CAREUNIT,newdate)]
    
    durations <- (time_window[2] - time_window[1]) / 6 
    x_labels <- c(time_window[1] + durations, time_window[1] + 2 * durations, time_window[1] + 3 * durations, time_window[1] + 4 * durations, time_window[1] + 5 * durations, time_window[1] + 6 * durations)
    x_breaks <- unname(round(quantile(as.numeric(dist_shift$newdate), probs = seq(0, 1, 0.2))))
    y_breaks <- unname(round(quantile(dist_shift[,get(objective)], probs = c(.25,.95))))
    
    plot <- ggplot(dist_shift, aes(newdate, get(objective), group = CURR_CAREUNIT)) + 
        geom_line(color=I("#9ebcda"),size=.4) + 
        facet_grid(CURR_CAREUNIT~.) + 
        labs(x=element_blank(), y=element_blank()) +
        theme(#axis.text.x = element_blank(), 
            #axis.ticks.y = element_blank(),
            strip.text.y = element_text(size=10, face="bold"),
            strip.text.x = element_text(size=10, face="bold"),
            strip.background = element_rect(colour="white", fill=I("#9ebcda")),
            panel.background = element_rect(fill = 'white', colour = 'white')) +
        scale_x_discrete(breaks = x_breaks, labels = x_labels) + 
        scale_y_continuous(breaks = y_breaks)
    list(plot)
}

#plot_timeseries("avg_pat", "CHARTTIME", c(min_date,max_date),"QUARTER")

