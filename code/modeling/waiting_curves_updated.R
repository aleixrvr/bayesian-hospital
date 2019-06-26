# library, download model data, functions ====
source("code/modeling/data_prep_updated_hour.R")
source("code/modeling/data_prep_updated_shift.R")
source("code/modeling/data_prep_updated_day.R")
ls()

unit_flow <- unit_flow_shift
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
waiting_do <- function(unit, unit_flow_data, outflow_models, r = c(0,100), w_max = 3){
    # set units in the ICU to be examined
    all_units <- names(unit_flow_data)
    units <- all_units[all_units != unit]
    # prepare data for the selected unit
    unit_flow_data[[unit]] %>% 
        select(starts_with("l_STAFF_"), starts_with("l_INFLOW_"), 
               starts_with("l2_waiting_time_"), 
               l2_waiting_time, INFLOW) %>% as.data.table() -> pred_data
    pred_data <- pred_data[complete.cases(pred_data)]
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
    to_plot <- data.frame(ressources=integer(), waiting_time=integer())#, do_unit=character())
    for(ressources in r[1]:r[2]){
        df <- data.frame(matrix(unlist(pred_outflows), ncol=length(pred_outflows), byrow=FALSE))
        df <- cbind(ressources, df)
        colnames(df) <- c("STAFF", paste0("l1_waiting_time_",names(pred_outflows)))
        #to_plot[ressources, "do_unit"] <- unit
        to_plot[ressources, "ressources"] <- ressources
        to_plot[ressources, "waiting_time"] <- 
            mean(1 / theta_thres(data.frame(predict(outflow_models[[unit]], df) - pred_data$INFLOW), w_max))
    }
    # estimate waiting time curves by varying staff in the downstream units
    # for(i in units){
    #   orig_cols <- colnames(pred_data)
    #   colnames(pred_data)[colnames(pred_data) == paste0("l_STAFF_",i)] <- "STAFF"
    #   inflow <- as.data.frame(pred_data)[paste0("l_INFLOW_",i)]
    #   pred_outflows[[i]] <- 1 / theta_thres(predict(outflow_models[[i]],  pred_data) - inflow, w_max)
    #   colnames(pred_data) <- orig_cols
    # }
    # for(ressources in r[1]:r[2]){
    #   df <- data.frame(matrix(unlist(pred_outflows), ncol=length(pred_outflows), byrow=FALSE))
    #   df <- cbind(unit_flow[[unit]]$STAFF, df)
    #   colnames(df) <- c("STAFF", paste0("l1_waiting_time_",names(pred_outflows)))
    #   to_plot[ressources, "do_unit"] <- unit
    #   to_plot[ressources, "ressources"] <- ressources
    #   to_plot[ressources, "waiting_time"] <-
    #     mean(1 / theta_thres(data.frame(predict(outflow_models[[unit]], df) - pred_data$INFLOW), w_max))
    # }
    # return all estimated waiting time curves
    return(to_plot[r[1]:r[2],])
}
see <- waiting_do("CCU", unit_flow, outflow_models)
plot(see$ressources, see$waiting_time,type = "l", col="tomato", lwd=2)


