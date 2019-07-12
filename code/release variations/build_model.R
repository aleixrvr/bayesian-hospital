# Library ====
library(ggplot2)
library(magrittr)
library(stargazer)

source('release/prepare_data.R')
source('release/outflow_curves.R')

# Model data ====
load("release/data/clean_dataset_label.RData")
# load("release/data/clean_dataset.RData")


# Settings ====
unit_flow <- split(flow_data,by="CURR_UNIT")
# staff_types <- c("Nurse", "Respiratory", "IMD", "PCT_NA",
#                  "Resident", "Others", "Unknown")

# staff_types <- c("RN", "IMD", "RT", "MD", "MS", "PCA", "CO", "Unknown")
staff_types <- c("RN", "RT", "PCA", "CO")


shift_types <- unique(flow_data$CHART_SHIFT)
w_max <- 2
staff_lags <- 2

results_table <- data.frame(
  resources=integer(), 
  outflow=numeric(), 
  unit_name=character(), 
  do_unit=character(), 
  staff=character(),
  shift_num=integer()
  )

# staff_types <- 'TOTAL'

# Build and merge relevant features for each dataset ====
median_stats <- list()
unit_flow0 <- unit_flow
for( staff_type in staff_types){
  unit_flow_shift0 <- prepare_data(unit_flow0, staff_type, w_max)
  median_stats[[staff_type]] <- list()
  outflow_models <- build_outflow_models(unit_flow_shift0, staff_lags)
  
  for( shift_num in shift_types ){
    median_stats[[staff_type]][[shift_num]] <- list()
    shift_unit_flow <- lapply(unit_flow_shift0, function(X){
      X[ CHART_SHIFT == shift_num ]
    })
    
    # stargazer(outflow_models,
    #           title="Outflow Models - Results",
    #           column.labels = paste("Outflow",names(outflow_models)),
    #           align=TRUE,
    #           type = "text")
    
    for(unit_name in names(unit_flow)){
      print(paste('Calculating...:', staff_type, ', ', unit_name, ', shift:', shift_num))
      do_resources <- outflow_do(unit_name, shift_unit_flow, outflow_models, staff_lags)
      do_resources$staff <- staff_type
      do_resources$shift_num <- shift_num
      results_table <- rbind(results_table, do_resources)
      median_stats[[staff_type]][[as.character(shift_num)]][[unit_name]] <- max(na.omit(shift_unit_flow[[unit_name]]$STAFF))
    }
  }
  
}
# 
results_table <- data.table(results_table)
plots <- list()
for( shift_num_iter in shift_types ){
  results_table[shift_num==shift_num_iter] %>%
    .[resources < 10] %>% 
    .[staff %chin% staff_types] %>% 
    ggplot(aes(resources, outflow, group=unit_name, color=unit_name)) +
    geom_line() +
    # facet_grid(.~do_unit)+
    facet_grid(staff~do_unit) +
    ggtitle(paste0('Shift:', shift_num_iter)) ->
    plots[[shift_num_iter]]
}
sapply(plots, plot)
# 
# # saveRDS(results_table, 'release/model/do_unit.RDS')
# # saveRDS(median_stats, 'release/model/median_stats.RDS')
# 
