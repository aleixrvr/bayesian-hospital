CCU_flow_clean <- CCU_flow %>% select(CHART_DATE, CHART_HOUR, STAFF, INFLOW, l2_waiting_time, starts_with("l_STAFF_"), 
                                      starts_with("l_INFLOW_"), starts_with("l2_waiting_time_"))
CCU_flow_clean <- CCU_flow_clean[complete.cases(CCU_flow_clean),]
CCU_flow_clean <- as.data.frame(CCU_flow_clean)
theta_thres <- function(net_flow, w_max){
  max(c(net_flow),(1/w_max))
}
waiting_do <- function(unit_flow_data, r){#}, reg_list){
  #1/ theta_thres(
  outflow_ccu_mod$coefficients["(Intercept)"] + 
    outflow_ccu_mod$coefficients["l1_waiting_time_csru"] * 1/ theta_thres(
      outflow_csru_mod$coefficients["(Intercept)"] +
        outflow_csru_mod$coefficients[c("l1_waiting_time_ccu", "l1_waiting_time_micu", 
                                        "l1_waiting_time_sicu", "l1_waiting_time_tsicu")] %*%
        t(unit_flow_data[c("l2_waiting_time", "l2_waiting_time_micu", 
                           "l2_waiting_time_sicu", "l2_waiting_time_tsicu")]) +
        outflow_csru_mod$coefficients["STAFF"] * unit_flow_data$l_STAFF_csru +
        unit_flow_data$l_INFLOW_csru
      , 1/2) + 
    outflow_ccu_mod$coefficients["l1_waiting_time_micu"] * 1/ theta_thres(
      outflow_micu_mod$coefficients["(Intercept)"] +
        outflow_micu_mod$coefficients[c("l1_waiting_time_ccu", "l1_waiting_time_csru", 
                                        "l1_waiting_time_sicu", "l1_waiting_time_tsicu")] %*%
        t(unit_flow_data[c("l2_waiting_time", "l2_waiting_time_micu", 
                           "l2_waiting_time_sicu", "l2_waiting_time_tsicu")]) +
        outflow_micu_mod$coefficients["STAFF"] * unit_flow_data$l_STAFF_micu +
        unit_flow_data$l_INFLOW_micu
      , 1/2) + 
    outflow_ccu_mod$coefficients["l1_waiting_time_sicu"] * 1/ theta_thres(
      outflow_sicu_mod$coefficients["(Intercept)"] +
        outflow_sicu_mod$coefficients[c("l1_waiting_time_ccu", "l1_waiting_time_csru", 
                                        "l1_waiting_time_micu", "l1_waiting_time_tsicu")] %*%
        t(unit_flow_data[c("l2_waiting_time", "l2_waiting_time_csru", 
                           "l2_waiting_time_micu", "l2_waiting_time_tsicu")]) +
        outflow_sicu_mod$coefficients["STAFF"] * unit_flow_data$l_STAFF_sicu +
        unit_flow_data$l_INFLOW_sicu
      , 1/2) + 
    outflow_ccu_mod$coefficients["l1_waiting_time_tsicu"] * 1/ theta_thres(
      outflow_tsicu_mod$coefficients["(Intercept)"] +
        outflow_tsicu_mod$coefficients[c("l1_waiting_time_ccu", "l1_waiting_time_crsu", 
                                         "l1_waiting_time_sicu", "l1_waiting_time_micu")] %*%
        t(unit_flow_data[c("l2_waiting_time", "l2_waiting_time_csru", 
                           "l2_waiting_time_sicu", "l2_waiting_time_micu")]) +
        outflow_tsicu_mod$coefficients["STAFF"] * unit_flow_data$l_STAFF_tsicu +
        unit_flow_data$l_INFLOW_tsicu
      , 1/2) + 
    outflow_ccu_mod$coefficients["STAFF"] * r - 
    unit_flow_data$INFLOW#, 1/2)
}

# apply(CCU_flow_clean, 1, waiting_do, r=1) # does not work, why? 

set.seed(123)
test <- sample(1:nrow(CCU_flow_clean),2000,replace = FALSE)
to_plot <- data.frame(ressources=integer(),
                      waiting_time=integer(),
                      nas=integer())
results <- vector()
for(r in 1:200){
  for(i in test){
    results[i] <- 1/ waiting_do(CCU_flow_clean[i,], r)
  }
  to_plot[r, "ressources"] <- r
  to_plot[r, "nas"] <- sum(is.na(results)) / nrow(CCU_flow_clean)
  to_plot[r, "waiting_time"] <- mean(results, na.rm = TRUE)
}
plot(to_plot$ressources, to_plot$waiting_time,type = "l", col="red",xlim = c(0,130),ylim=c(0,4))
#plot(to_plot$ressources, to_plot$nas,type = "l", col="red")


