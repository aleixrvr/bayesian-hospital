# Library ====
library(ggplot2)
library(magrittr)
library(stargazer)

source('release/prepare_data.R')
source('release/outflow_curves.R')

# Model data ====
load("release/data/clean_dataset.RData")


# Settings ====
unit_flow <- split(flow_data,by="CURR_UNIT")
staff_types <- c("Nurse", "Respiratory", "IMD", "PCT_NA", 
                 "Resident", "Others")
shift_types <- unique(flow_data$CHART_SHIFT)
w_max <- 2
staff_lags <- 2

results_table <- data.frame(
  resources=integer(), 
  outflow=numeric(), 
  unit_name=character(), 
  do_unit=character(), 
  staff=character())

staff_types <- 'TOTAL'

# Build and merge relevant features for each dataset ====
unit_flow0 <- unit_flow
for( staff_type in staff_types){
  unit_flow <- prepare_data(unit_flow0, staff_type, w_max)
  outflow_models <- build_outflow_models(unit_flow, staff_lags)
  
  stargazer(outflow_models,
            title="Outflow Models - Results",
            column.labels = paste("Outflow",names(outflow_models)),
            align=TRUE,
            type = "text")
  
  for(unit_name in names(unit_flow)){
    do_resources <- outflow_do(unit_name, unit_flow, outflow_models, staff_lags)
    do_resources$staff <- staff_type
    results_table <- rbind(results_table, do_resources)
  }
  
  # median(unit_flow[[unit_name]]$STAFF)

}

ggplot(results_table, aes(resources, outflow, group=unit_name, color=unit_name)) +
  geom_line() +
  facet_grid(.~do_unit, scales = 'free_x')


