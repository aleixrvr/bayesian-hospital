# Library ====
library(ggplot2)
library(magrittr)
library(stargazer)
library(data.table)

source('model-building/prepare_data.R')
source('model-building/outflow_curves.R')

# Model data ====
load("model-building/data/clean_dataset.RData")

# Settings ====
unit_flow <- split(flow_data,by="CURR_UNIT")
shift_types <- unique(flow_data$CHART_SHIFT)
w_max <- 2
staff_lags <- 2

results_table <- data.frame(
  resources=integer(), 
  outflow=numeric(), 
  unit_name=character(), 
  do_unit=character(), 
  shift_num=integer()
  )


# Build and merge relevant features for each dataset ====
median_stats <- list()
unit_flow <- prepare_data(unit_flow, w_max)
outflow_models <- build_outflow_models(unit_flow, staff_lags)

median_stats <- list()
for( shift_num in shift_types ){
  median_stats[[as.character(shift_num)]] <- list()
  shift_unit_flow <- lapply(unit_flow, function(X){
    X[ CHART_SHIFT == shift_num ]
  })
  
  for(unit_name in names(unit_flow)){
    print(paste('Calculating...:', unit_name, ', shift:', shift_num))
    do_resources <- outflow_do(unit_name, shift_unit_flow, outflow_models, staff_lags)
    do_resources$shift_num <- shift_num
    results_table <- rbind(results_table, do_resources)
    median_stats[[as.character(shift_num)]][[unit_name]] <- max(na.omit(shift_unit_flow[[unit_name]]$STAFF))
  }
}
  

# Visual Checks
# results_table <- data.table(results_table)
# plots <- list()
# for( shift_num_iter in shift_types ){
#   results_table[shift_num==shift_num_iter] %>%
#     ggplot(aes(resources, outflow, group=unit_name, color=unit_name)) +
#     geom_line() +
#     facet_grid(.~do_unit)+
#     ggtitle(paste0('Shift:', shift_num_iter)) ->
#     plots[[shift_num_iter]]
# }
# sapply(plots, plot)
# 
# stargazer(outflow_models,
#           title="Outflow Models - Results",
#           column.labels = paste("Outflow",names(outflow_models)),
#           align=TRUE,
#           type = "latex")


saveRDS(results_table, 'model-building/model/do_unit.RDS')
saveRDS(median_stats, 'model-building/model/median_stats.RDS')
