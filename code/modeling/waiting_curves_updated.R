# library, download model data, functions ====
source("code/modeling/data_prep_updated_hour.R")
# source("code/modeling/data_prep_updated_other_aggregation.R")
# source("code/modeling/data_prep_updated_day.R")
library(ggplot2)
# ls()

unit_flow <- unit_flow_hour
# unit_flow <- unit_flow_shift
# unit_flow <- unit_flow_day
# Modelling ====
units <- names(unit_flow)
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
outflow_models["CCU"]

# Waiting times curves ====
waiting_do <- function(unit, unit_flow_data, outflow_models, r = c(1,100), w_max = 3){
    # set units in the ICU to be examined
    all_units <- names(unit_flow_data)
    units <- all_units[all_units != unit]
    # prepare data for the selected unit
    unit_flow_data[[unit]] %>% 
        select(starts_with("l_STAFF_"), starts_with("l_INFLOW_"), 
               starts_with("l2_waiting_time_"), 
               l2_waiting_time, INFLOW, STAFF) %>% as.data.table() -> pred_data
    pred_data <- pred_data[complete.cases(pred_data)]
    staff_data <- pred_data$STAFF
    pred_data <- select(pred_data, -STAFF)
    colnames(pred_data)[colnames(pred_data) == "l2_waiting_time"] <- paste0("l2_waiting_time_",unit)
    colnames(pred_data) <- gsub("2","1",colnames(pred_data))
    # estimate outflows for downstream units
    pred_outflows <- list()
    for(i in units){
        orig_cols <- colnames(pred_data)
        colnames(pred_data)[colnames(pred_data) == paste0("l_STAFF_",i)] <- "STAFF"
        inflow <- as.data.frame(pred_data)[paste0("l_INFLOW_",i)]
        pred_outflows[[i]] <- 1 / theta_thres(predict(outflow_models[[i]],  pred_data) - inflow, w_max)
        colnames(pred_data) <- orig_cols
    }
    # estimate waiting time curves by varying staff in the unit of interest 
    to_plot <- data.frame(ressources=integer(), waiting_time=integer(), unit=character())#, do_unit=character())
    for(ressources in r[1]:r[2]){
        df <- data.frame(matrix(unlist(pred_outflows), ncol=length(pred_outflows), byrow=FALSE))
        df <- cbind(ressources, df)
        colnames(df) <- c("STAFF", paste0("l1_waiting_time_",names(pred_outflows)))
        waiting_time <- mean(1 / theta_thres(data.frame(predict(outflow_models[[unit]], df) - pred_data$INFLOW), w_max))
        to_plot <- rbind(to_plot, data.frame(ressources, waiting_time, unit))
    }
    
    # estimate waiting time curves by varying staff in the downstream units
    for(downstream_unit in units){
      pred_outflows_old <- pred_outflows[[downstream_unit]]
      
      for(ressources in r[1]:r[2]){
      
        orig_cols <- colnames(pred_data)
        colnames(pred_data)[colnames(pred_data) == paste0("l_STAFF_", downstream_unit)] <- "STAFF"
        pred_data[, "STAFF"] <- ressources
        inflow <- as.data.frame(pred_data)[paste0("l_INFLOW_", downstream_unit)]
        pred_outflows[[downstream_unit]] <- 1 / theta_thres(predict(outflow_models[[downstream_unit]],  pred_data) - inflow, w_max)
        colnames(pred_data) <- orig_cols
        
        df <- data.frame(matrix(unlist(pred_outflows), ncol=length(pred_outflows), byrow=FALSE))
        df <- cbind(staff_data, df)
        colnames(df) <- c("STAFF", paste0("l1_waiting_time_",names(pred_outflows)))
        waiting_time <- mean(1 / theta_thres(data.frame(predict(outflow_models[[unit]], df) - pred_data$INFLOW), w_max))
        to_plot <- rbind(to_plot, data.frame(ressources, waiting_time, unit=downstream_unit))
      }
      
      pred_outflows[[downstream_unit]] <- pred_outflows_old
    }

    return(to_plot)
}
see <- waiting_do("CCU", unit_flow, outflow_models)
ggplot(see, aes(ressources, waiting_time, group=unit, color=unit)) +
  geom_line()

# plot(see$ressources, see$waiting_time,type = "l", col="tomato", lwd=2)


