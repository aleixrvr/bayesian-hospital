# library, download model data, functions ====
#source("code/modeling/data_prep_updated_hour.R")
source("code/modeling/data_prep_updated_other_aggregation.R")
#source("code/modeling/data_prep_updated_day.R")
library(ggplot2)
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


stargazer(inflow_models, 
          title="Inflow Models - Results", 
          column.labels = paste("Inflow",names(outflow_models)), 
          align=TRUE, 
          type = "latex")
stargazer(outflow_models, 
          title="Outflow Models - Results", 
          column.labels = paste("Outflow",names(outflow_models)), 
          align=TRUE, 
          type = "latex")
rm(list=c("i", "to_model_outflow", "to_model_inflow","units"))
outflow_models

# Waiting times curves ====
waiting_do <- function(unit, unit_flow_data, outflow_models, r = c(1,100), w_max = 2){
    # set units in the ICU to be examined
    do_unit <- unit
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
    to_plot <- data.frame(ressources=integer(), waiting_time=integer(), unit=character(), 
                          do_unit=character())
    for(ressources in r[1]:r[2]){
        df <- data.frame(matrix(unlist(pred_outflows), ncol=length(pred_outflows), byrow=FALSE))
        df <- cbind(ressources, df)
        colnames(df) <- c("STAFF", paste0("l1_waiting_time_",names(pred_outflows)))
        waiting_time <- mean(1 / theta_thres(data.frame(predict(outflow_models[[unit]], df) - 
                                                            pred_data$INFLOW), w_max))
        to_plot <- rbind(to_plot, data.frame(ressources, waiting_time, unit, do_unit))
    }
    
    # estimate waiting time curves by varying staff in the downstream units
    for(downstream_unit in units){
      pred_outflows_old <- pred_outflows[[downstream_unit]]
      
      for(ressources in r[1]:r[2]){
      
        orig_cols <- colnames(pred_data)
        colnames(pred_data)[colnames(pred_data) == paste0("l_STAFF_", downstream_unit)] <- "STAFF"
        pred_data[, "STAFF"] <- ressources
        inflow <- as.data.frame(pred_data)[paste0("l_INFLOW_", downstream_unit)]
        pred_outflows[[downstream_unit]] <- 1 / theta_thres(predict(outflow_models[[downstream_unit]],  
                                                                    pred_data) - inflow, w_max)
        colnames(pred_data) <- orig_cols
        
        df <- data.frame(matrix(unlist(pred_outflows), ncol=length(pred_outflows), byrow=FALSE))
        df <- cbind(staff_data, df)
        colnames(df) <- c("STAFF", paste0("l1_waiting_time_",names(pred_outflows)))
        waiting_time <- mean(1 / theta_thres(data.frame(predict(outflow_models[[unit]], df) - 
                                                            pred_data$INFLOW), w_max))
        to_plot <- rbind(to_plot, data.frame(ressources, waiting_time, unit=downstream_unit, do_unit))
      }
      
      pred_outflows[[downstream_unit]] <- pred_outflows_old
    }

    return(to_plot)
}

# par(mfrow=c(3,2))
# for(i in names(unit_flow)){
#     see <- waiting_do(i, unit_flow, outflow_models)
#     plot(see$ressources, see$waiting_time,type = "l", col="tomato", lwd=2, 
#          main=paste("Waiting Curve", i), ylim=c(0,3))
#     
# }
# par(mfrow=c(1,1))
# plot(see$ressources, see$waiting_time,type = "l", col="tomato", lwd=2)

to_plot <- data.frame(ressources=integer(), waiting_time=integer(), unit=character(), do_unit=character())
for(unit_name in names(unit_flow)){
  do_waiting <- waiting_do(unit_name, unit_flow, outflow_models)
  to_plot <- rbind(to_plot, do_waiting)
}

ggplot(to_plot, aes(ressources, waiting_time, group=unit, color=unit)) +
    geom_line() +
    facet_grid(.~do_unit)



head(to_plot$waiting_time)
if(max(to_plot$waiting_time) == 2){
  to_plot$waiting_time <- to_plot$waiting_time * 4
}
units <- as.character(unique(to_plot$unit))
# colors <- c(I("#9ebcda"), "gray79", "gray54", "gray46", "gray25")
colors <- c(I("#042B51"), I("#40639E"), I("#9EBCDA"), I("#73B780"), I("#9CD8A7"))
names(colors) <- units
par(mfrow=c(3,2), cex.axis=.8, cex.lab=.9, cex.main=1.2)

for(u in units){
    see <- to_plot[to_plot$unit == u,]
    unit_detail <- as.character(unique(see$unit))
    other_units <- as.character(unique(see$do_unit))
    other_units <- other_units[other_units != unit_detail]
    
    
    plot(see[see$do_unit == unit_detail,"ressources"], 
         see[see$do_unit == unit_detail,"waiting_time"], 
         col=colors[unit_detail], type = "l", lwd=2, 
         ylab="Waiting time in hours", las=2, ylim=c(3,9),
         xlab= "Ressources", frame.plot=FALSE, xaxt="n",
         #main=paste("Change in waiting times in the", unit_detail, "dependent", 
         #           "\n on staff and downstream units")) 
         main=paste(unit_detail))
    axis(1, at=c(0,25,50,75,100))
    unit_median_staff <- median(unit_flow[[unit_detail]]$STAFF,na.rm = TRUE)
    abline(v= unit_median_staff, lty=3, lwd=2, col="grey")
    text(x=unit_median_staff,y=4,"Median",col="grey",cex=.8,pos=2)
    for(i in 1:length(other_units)){
        lines(see[see$do_unit == other_units[i],"ressources"],
              see[see$do_unit == other_units[i],"waiting_time"], 
              lwd=1, col=colors[i])
    }
    lines(see[see$do_unit == unit_detail,"ressources"], 
          see[see$do_unit == unit_detail,"waiting_time"], 
          col=colors[unit_detail], type = "l", lwd=3)
}
plot.new()
legend("center", names(colors), col = colors, lwd = 4, cex = 1, bty="n")
par(mfrow=c(1,1),cex.axis=1, cex.lab=1, cex.main=1)




