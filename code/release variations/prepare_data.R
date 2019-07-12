library(data.table)
library(dplyr)

# Functions ====
lagpad <- function(x, k = 1) {
  c(rep(NA, k), x)[1 : length(x)]
}
theta_thres <- function(net_flow, w_max){
  apply(net_flow, 1, function(x) max(x,(1/w_max)))
}

prepare_data <- function(unit_flow, staff_type = 'TOTAL', w_max){
  unit_flow <- lapply(unit_flow, function(X){
    X <- as.data.frame(X)
    staff_column <- paste('STAFF', staff_type, sep='_')
    
    X$STAFF <- X[,staff_column]
    X$STAFF_TOTAL <- NULL
    X[[staff_column]] <- NULL
    X$waiting_time <- 1 / theta_thres(data.frame(X$OUTFLOW - X$INFLOW), w_max)
    X$l1_waiting_time <- lagpad(X$waiting_time)
    X$l2_waiting_time <- lagpad(X$waiting_time, 2)
    X$l_STAFF_1 <- lagpad(X$STAFF)
    X$l_STAFF_2 <- lagpad(X$STAFF, 2)
    X$l_OUTFLOW = lagpad(X$OUTFLOW)
    X$l_INFLOW = lagpad(X$INFLOW)
    X$l_PATIENTS = lagpad(X$PATIENTS)
    as.data.table(X)
  })
  
  unit_flow0 <- unit_flow
  units <- names(unit_flow)
  for(i in units){
    to_merge <- units[units != i]
    for(m in to_merge){
      other_unit_flow <- unit_flow0[[m]] %>% 
        select(
          CHART_DATE, 
          CHART_SHIFT, 
          l1_waiting_time, 
          l2_waiting_time, 
          starts_with("l_STAFF"), 
          l_INFLOW,
          l_PATIENTS)
      unit_flow[[i]] <- merge(unit_flow[[i]], other_unit_flow, 
                              by = c("CHART_DATE", "CHART_SHIFT"), 
                              suffixes = c("", paste0("_", m)))
    }
  }
  unit_flow[['NICU']] = NULL
  unit_names <- names(unit_flow)
  unit_flow <- lapply(unit_flow, function(X){
    X %>% select(-contains('NICU')) 
  })
  names(unit_flow) <- unit_names
  
  return(unit_flow)
}

build_outflow_models <- function(unit_flow, staff_lags){
  
  staff_lags_name = paste0('l_STAFF_', 1:staff_lags)
  staff_lags_name_main <- staff_lags_name[1:length(staff_lags_name) -1 ]
  
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
  
  return(outflow_models)
}
