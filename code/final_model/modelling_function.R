# Library ====
library(ggplot2)
library(magrittr)

# Model data ====
load("data/clean_dataset.RData")

# Functions ====
lagpad <- function(x, k = 1) {
    c(rep(NA, k), x)[1 : length(x)]
}
theta_thres <- function(net_flow, w_max){
    apply(net_flow, 1, function(x) max(x,(1/w_max)))
}


# Settings ====
unit_flow <- split(flow_data,by="CURR_UNIT")
staff_types <- c("STAFF_Nurse", "STAFF_Respiratory", "STAFF_IMD", "STAFF_PCT_NA", 
                 "STAFF_Resident", "STAFF_Others")
shift_types <- unique(flow_data$CHART_SHIFT)
w_max <- 2
i <- 1

# Build and merge relevant features for each dataset ====
model_flow <- lapply(unit_flow, function(X){
    X <- as.data.frame(X)
    X$STAFF <- X[,staff_types[i]]
    X$STAFF_TOTAL <- NULL
    X$STAFF_Nurse <- NULL
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

unit_flow0 <- model_flow
units <- names(model_flow)
for(i in units){
    to_merge <- units[units != i]
    for(m in to_merge){
        other_model_flow <- unit_flow0[[m]] %>% 
            select(
                CHART_DATE, 
                CHART_SHIFT, 
                l1_waiting_time, 
                l2_waiting_time, 
                starts_with("l_STAFF"),
                starts_with("STAFF"),
                l_INFLOW,
                l_PATIENTS)
        model_flow[[i]] <- merge(model_flow[[i]], other_unit_flow, 
                                by = c("CHART_DATE", "CHART_SHIFT"), 
                                suffixes = c("", paste0("_", m)))
    }
}
# Fit models for later predictions ====
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


model_flow


# Re-build original dataset for next loop iteration ====
X[,staff_types[i]] <- X$STAFF
X$STAFF <- NULL

