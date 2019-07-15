# Library ====
library(ggplot2)
library(magrittr)
library(stargazer)
library(lubridate)

source('release/prepare_data.R')
source('release/outflow_curves.R')

# Model data ====
load("release/data/clean_dataset.RData")

# Settings ====
unit_flow <- split(flow_data,by="CURR_UNIT")
shift_types <- unique(flow_data$CHART_SHIFT)
w_max <- 2
staff_lags <- 2
days_of_week <- (today()+0:6) %>% lubridate::wday(label=TRUE)


results_table <- data.frame(
  resources=integer(), 
  outflow=numeric(), 
  unit_name=character(), 
  do_unit=character(), 
  shift_num=integer(),
  week_day=character()
  )


# Build and merge relevant features for each dataset ====
median_stats <- list()
unit_flow <- prepare_data(unit_flow, w_max)
outflow_models <- build_outflow_models(unit_flow, staff_lags)

median_stats <- list()
for( shift_num in shift_types ){
  for( week_day in days_of_week ){
    median_stats[[as.character(shift_num)]] <- list()
    shift_unit_flow_day <- lapply(unit_flow, function(X){
      X %>% 
        .[ CHART_SHIFT == shift_num] %>% 
        .[lubridate::wday(CHART_DATE, label=TRUE) == week_day]
    })
    
    for(unit_name in names(unit_flow)){
      print(paste('Calculating...:', unit_name, ', shift:', shift_num))
      do_resources <- outflow_do(unit_name, shift_unit_flow_day, outflow_models, staff_lags)
      do_resources$shift_num <- shift_num
      do_resources$week_day <- as.character(week_day)
      results_table <- rbind(results_table, do_resources)
      median_stats[[as.character(shift_num)]][[unit_name]] <- max(na.omit(shift_unit_flow_day[[unit_name]]$STAFF))
    }
  }
}
  

# Visual Checks
results_table <- data.table(results_table)
plots <- list()
for( shift_num_iter in shift_types ){
  results_table[shift_num==shift_num_iter] %>%
    ggplot(aes(resources, outflow, group=unit_name, color=unit_name)) +
    geom_line() +
    facet_grid(week_day~do_unit)+
    ggtitle(paste0('Shift:', shift_num_iter)) ->
    plots[[shift_num_iter]]
}
sapply(plots, plot)
# 
# stargazer(outflow_models,
#           title="Outflow Models - Results",
#           column.labels = paste("Outflow",names(outflow_models)),
#           align=TRUE,
#           type = "text")
# 
# 
# saveRDS(results_table, 'release/model/do_unit.RDS')
# saveRDS(median_stats, 'release/model/median_stats.RDS')
