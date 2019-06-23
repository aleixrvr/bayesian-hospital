# download model data ====
library(stargazer)
source("../modeling/data_prep_hour.R")
rm(list=c("billing", "con", "detailed_inflow", "detailed_outflow","in_cols", 
          "inflow", "inflow_sql", "out_cols", "outflow", "outflow_sql",
          "relabel_cols", "skeleton", "skeleton_sql"))
theta_thres <- function(net_flow, w_max){
  apply(net_flow, 1, function(x) max(x,(1/w_max)))
}

# create relevant features for each dataset ====
flow_data[,AVG_LOS:=NULL]
unit_flow <- split(flow_data,by="CURR_UNIT")
unit_flow <- lapply(unit_flow, function(X){
  X %>% mutate(net_flow = OUTFLOW - INFLOW, 
               max_wait = 1/4, 
               waiting_time = 1/pmax(net_flow, max_wait),
               l1_waiting_time = lagpad(waiting_time),
               l2_waiting_time = lagpad(waiting_time, 2),
               l_OUTFLOW = lagpad(OUTFLOW), 
               l_STAFF = lagpad(STAFF),
               l_INFLOW = lagpad(INFLOW)) %>%
    select(-c(net_flow,max_wait)) %>% 
    as.data.table() -> X
  X <- X[complete.cases(X)]
})

# merge  relevant features for each dataset ====
units <- names(unit_flow)
for(i in units){
  to_merge <- units[units != i]
  for(m in to_merge){
    unit_flow[[i]] <- merge(unit_flow[[i]], unit_flow[[m]][,c("CHART_DATE", 
                                                              "CHART_HOUR", 
                                                              "l1_waiting_time", 
                                                              "l2_waiting_time", 
                                                              "l_STAFF", 
                                                              "l_INFLOW")], 
                            by = c("CHART_DATE", "CHART_HOUR"), 
                            suffixes = c("", paste0("_", m)))
  }
  
}
rm(list=c("to_merge", "i", "m", "lagpad"))

# Modelling ====
inflow_models <- list()
outflow_models <- list()
for(i in units){
  to_model_inflow <- unit_flow[[i]] %>% select(INFLOW, starts_with("from_"))
  to_model_inflow[,paste0("from_",i):=NULL]
  inflow_models[[i]] <- lm(INFLOW ~ ., to_model_inflow)
  to_model_outflow <- unit_flow[[i]] %>% select(OUTFLOW, STAFF, starts_with("l1_waiting_time"))
  to_model_outflow[,l1_waiting_time:=NULL]
  outflow_models[[i]] <- lm(OUTFLOW ~ ., to_model_outflow)
}

stargazer(inflow_models, title="Results", align=TRUE, type = "latex")
stargazer(outflow_models, title="Results", align=TRUE, type = "latex")
rm(list=c("i", "to_model_outflow", "to_model_inflow","units"))

# Waiting times curves ====
waiting_do <- function(unit, unit_flow_data, r, outflow_models, w_max = 3){
  units <- names(unit_flow)
  units <- units[units != unit]
  unit_flow[[unit]] %>% 
    select(starts_with("l_STAFF_"), starts_with("l_INFLOW_"), 
           starts_with("l2_waiting_time_"), 
           l2_waiting_time, INFLOW) %>% as.data.table() -> pred_data
  colnames(pred_data)[colnames(pred_data) == "l2_waiting_time"] <- paste0("l2_waiting_time_",unit)
  colnames(pred_data) <- gsub("2","1",colnames(pred_data))
  pred_outflows <- list()
  for(i in units){
    orig_cols <- colnames(pred_data)
    colnames(pred_data)[colnames(pred_data) == paste0("l_STAFF_",i)] <- "STAFF"
    inflow <- as.data.frame(pred_data)[paste0("l_INFLOW_",i)]
    pred_outflows[[i]] <- 1 / theta_thres(predict(outflow_models[[i]],  pred_data) - inflow, w_max)
    colnames(pred_data) <- orig_cols
  }
  
  df <- data.frame(matrix(unlist(pred_outflows), ncol=length(pred_outflows), byrow=FALSE))
  df <- cbind(r, df) #cbind(unit_flow[[unit]]$STAFF, df)
  colnames(df) <- c("STAFF", paste0("l1_waiting_time_",names(pred_outflows)))
  mean(1 / theta_thres(data.frame(predict(outflow_models[[unit]], df) - pred_data$INFLOW), w_max))
}
to_plot <- data.frame(ressources=integer(),
                      waiting_time=integer())
for(i in 1:100){
  to_plot[i, "ressources"] <- i
  to_plot[i, "waiting_time"] <- waiting_do(unit = "CCU", unit_flow_data = unit_flow, 
                                           r = i, outflow_models = outflow_models)
}
plot(to_plot$ressources, to_plot$waiting_time,type = "l", col="red")
