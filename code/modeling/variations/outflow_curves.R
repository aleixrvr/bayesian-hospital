# library, download model data, functions ====
source("code/modeling/variations/data_prep_other_aggregation_patients.R")
library(ggplot2)
library(magrittr)
# ls()

# unit_flow <- unit_flow_hour
unit_flow <- unit_flow_shift
# unit_flow <- unit_flow_day
# Modelling ====


## Removing NICU
unit_flow[['NICU']] = NULL
unit_names <- names(unit_flow)
unit_flow <- lapply(unit_flow, function(X){
  X %>% select(-contains('NICU')) 
})
names(unit_flow) <- unit_names

staff_lags_name = paste0('l_STAFF_', 1:staff_lags)
staff_lags_name_main <- staff_lags_name[1:length(staff_lags_name) -1 ]
staff_lags_lagged <- staff_lags_name[1:length(staff_lags_name)]

units <- names(unit_flow)
inflow_models <- list()
outflow_models <- list()
for(i in units){
  to_model_inflow <- unit_flow[[i]] %>% select(INFLOW, starts_with("from_"))
  to_model_inflow[,paste0("from_",i):=NULL]
  inflow_models[[i]] <- lm(INFLOW ~ ., to_model_inflow)
  unit_flow[[i]] %>% 
    select(
      OUTFLOW, 
      STAFF, 
      PATIENTS, 
      starts_with("l1_waiting_time"),
      staff_lags_name_main) -> 
    to_model_outflow
  
  to_model_outflow[,l1_waiting_time:=NULL]
  outflow_models[[i]] <- lm(OUTFLOW ~ ., to_model_outflow)
}


# stargazer(inflow_models, 
#           title="Inflow Models - Results", 
#           column.labels = paste("Inflow",names(outflow_models)), 
#           align=TRUE, 
#           type = "latex")
stargazer(outflow_models,
          title="Outflow Models - Results",
          column.labels = paste("Outflow",names(outflow_models)),
          align=TRUE,
          type = "text")
# rm(list=c("i", "to_model_outflow", "to_model_inflow","units"))

# Waiting times curves ====

add_unit_name <- function(variables, unit_name){
  return( paste0(variables, '_', unit_name) )
}

change_col_names_lags <- function(other_pred_data, unit_name, other_unit){
  setnames(other_pred_data, add_unit_name('l_STAFF_1', other_unit), 'STAFF')
  for( pos in 2:staff_lags ){
    prev_lag <- staff_lags_lagged[pos] %>% add_unit_name(other_unit)
    new_lag <- staff_lags_name_main[pos - 1] 
    setnames(other_pred_data, prev_lag, new_lag)
  }
  setnames(other_pred_data, 'l_PATIENTS', 'PATIENTS')
  
  other_pred_data %>% 
    select(starts_with("l2_waiting_time_")) %>% 
    colnames() ->
    waiting_times_col_names
  for( col_name in waiting_times_col_names ){
    new_col_name <- gsub('2', '1', col_name)
    setnames(other_pred_data, col_name, new_col_name)
  }
  setnames(other_pred_data, 'l2_waiting_time', add_unit_name('l1_waiting_time', unit_name))
  
  return(other_pred_data)
}


pred_downstream_waiting <- function(pred_data, unit_name, other_unit, resources=NULL){
  staff_lags_lagged_other <- paste0(staff_lags_lagged, '_', other_unit)
  l_patients_other <- paste0('l_PATIENTS_', other_unit)
  
  pred_data %>% 
    select(
      staff_lags_lagged_other, 
      l_PATIENTS,
      starts_with("l2_waiting_time"),
      starts_with("l_INFLOW_")
    ) ->
    other_pred_data
  
  other_pred_data <- change_col_names_lags(other_pred_data, unit_name, other_unit)
  
  if(!is.null(resources)){
    staff_cols <- other_pred_data %>% select(contains('STAFF')) %>% colnames()
    for(col in staff_cols){
      other_pred_data[[col]] <- resources
    }
  }
  
  other_pred_data %<>% select(-contains("INFLOW"))
  pred_outflow <- try(predict(outflow_models[[other_unit]],  other_pred_data))
  if(class(pred_outflow) == 'try-error') browser()
  return( 1 / theta_thres(as.data.frame(pred_outflow - pred_data$INFLOW), w_max) )
}


predict_outflow_main <- function(unit_name, pred_data, pred_waiting, resources=NULL){
  
  pred_waiting_all <- data.frame(matrix(unlist(pred_waiting), ncol=length(pred_waiting), byrow=FALSE))
  colnames(pred_waiting_all) <- paste0("l1_waiting_time_", names(pred_waiting))
  
  pred_data %>% 
    select(c('PATIENTS', 'STAFF', staff_lags_name_main)) %>% 
    cbind(pred_waiting_all) ->
    df2predict
  
  if(!is.null(resources)){
    staff_cols <- df2predict %>% select(contains('STAFF')) %>% colnames()
    for(col in staff_cols){
      df2predict[[col]] <- resources
    }
  }
  
  pred_outflow <- try(predict(outflow_models[[unit_name]], df2predict))
  if(class(pred_outflow) == 'try-error') browser()
  return( mean(pred_outflow) )
}

outflow_do <- function(unit_name, unit_flow_data, outflow_models, r = c(-20,20), w_max = w_max){
  # set units in the ICU to be examined
  do_unit <- unit_name
  all_units <- names(unit_flow_data)
  units <- all_units[all_units != unit_name]
  
  sel_resources = unique(pmax(median(unit_flow[[unit_name]]$STAFF,na.rm = TRUE) + r, 0))
  sel_resources = sel_resources[1]:sel_resources[2]
  
  # prepare data for the selected unit_name
  unit_flow_data[[unit_name]] %>% 
    as.data.table() -> pred_data
  pred_data <- pred_data[complete.cases(pred_data)]
  
  
  pred_waiting <- list()
  for( other_unit in units ){
    pred_waiting[[other_unit]] <- pred_downstream_waiting(pred_data, unit_name, other_unit)
  }
  
  # Estimate effect on the main unit_name
  to_plot <- data.frame(resources=integer(), outflow=numeric(), unit_name=character(), 
                        do_unit=character())
  for(resources in sel_resources){
    outflow <- predict_outflow_main(unit_name, pred_data, pred_waiting, resources)
    to_plot <- rbind(to_plot, data.frame(resources, outflow, unit_name, do_unit))
  }
  
  # Estimate waiting time curves by varying staff in the downstream units
  pred_waiting_base <- pred_waiting
  for(other_unit in units){
    pred_waiting_old <- pred_waiting[[other_unit]]
    
    for(resources in sel_resources){
      pred_waiting <- pred_waiting_base
      pred_waiting[[other_unit]] <- pred_downstream_waiting(pred_data, unit_name, other_unit, resources)
      
      outflow <- predict_outflow_main(unit_name, pred_data, pred_waiting)
      to_plot <- rbind(to_plot, data.frame(resources, outflow, unit_name=other_unit, do_unit))
    }
  }
  
  return(to_plot)
}


to_plot <- data.frame(resources=integer(), outflow=numeric(), unit_name=character(), do_unit=character())
for(unit_name in names(unit_flow)){
  do_waiting <- outflow_do(unit_name, unit_flow, outflow_models)
  to_plot <- rbind(to_plot, do_waiting)
}

ggplot(to_plot, aes(resources, outflow, group=unit_name, color=unit_name)) +
  geom_line() +
  facet_grid(.~do_unit, scales = 'free_x')


