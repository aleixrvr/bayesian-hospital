# data ====
CCU_flow_clean <- CCU_flow %>% select(CHART_DATE, CHART_HOUR, STAFF, INFLOW, l2_waiting_time, 
                                      starts_with("l_STAFF_"), 
                                      starts_with("l_INFLOW_"), starts_with("l2_waiting_time_"))
CCU_flow_clean <- CCU_flow_clean[complete.cases(CCU_flow_clean),]
CCU_flow_clean <- as.data.frame(CCU_flow_clean)
theta_thres <- function(net_flow, w_max){
  max(c(net_flow),(1/w_max))
}

# first ====
waiting_do <- function(unit_flow_data, r){#}, reg_list){
  #1/ theta_thres(
  
  # beta_0
  outflow_ccu_mod$coefficients["(Intercept)"] + 
    # beta_w_crsu - fraction
    outflow_ccu_mod$coefficients["l1_waiting_time_csru"] * 1/ theta_thres(
      outflow_csru_mod$coefficients["(Intercept)"] +
        # beta_w_downstream
        # outflow_csru_mod$coefficients[c("l1_waiting_time_ccu", "l1_waiting_time_micu", 
        #                                 "l1_waiting_time_sicu", "l1_waiting_time_tsicu")] %*%
        # t(unit_flow_data[c("l2_waiting_time", "l2_waiting_time_micu", 
        #                    "l2_waiting_time_sicu", "l2_waiting_time_tsicu")]) +
        # predict for downstream units the outflow by using down down stream units values
        outflow_csru_mod$coefficients["STAFF"] * unit_flow_data$l_STAFF_csru +
        unit_flow_data$l_INFLOW_csru
      , 5) + 
    # beta_w_micu - fraction
    outflow_ccu_mod$coefficients["l1_waiting_time_micu"] * 1/ theta_thres(
      outflow_micu_mod$coefficients["(Intercept)"] +
        # beta_w_downstream
        outflow_micu_mod$coefficients[c("l1_waiting_time_ccu", "l1_waiting_time_csru", 
                                        "l1_waiting_time_sicu", "l1_waiting_time_tsicu")] %*%
        t(unit_flow_data[c("l2_waiting_time", "l2_waiting_time_micu", 
                           "l2_waiting_time_sicu", "l2_waiting_time_tsicu")]) +
        outflow_micu_mod$coefficients["STAFF"] * unit_flow_data$l_STAFF_micu +
        unit_flow_data$l_INFLOW_micu
      , 1/2) + 
    # beta_w_sicu - fraction
    outflow_ccu_mod$coefficients["l1_waiting_time_sicu"] * 1/ theta_thres(
      outflow_sicu_mod$coefficients["(Intercept)"] +
        # beta_w_downstream
        outflow_sicu_mod$coefficients[c("l1_waiting_time_ccu", "l1_waiting_time_csru", 
                                        "l1_waiting_time_micu", "l1_waiting_time_tsicu")] %*%
        t(unit_flow_data[c("l2_waiting_time", "l2_waiting_time_csru", 
                           "l2_waiting_time_micu", "l2_waiting_time_tsicu")]) +
        outflow_sicu_mod$coefficients["STAFF"] * unit_flow_data$l_STAFF_sicu +
        unit_flow_data$l_INFLOW_sicu
      , 1/2) + 
    # beta_w_tsicu - fraction
    outflow_ccu_mod$coefficients["l1_waiting_time_tsicu"] * 1/ theta_thres(
      outflow_tsicu_mod$coefficients["(Intercept)"] +
        # beta_w_downstream
        outflow_tsicu_mod$coefficients[c("l1_waiting_time_ccu", "l1_waiting_time_crsu", 
                                         "l1_waiting_time_sicu", "l1_waiting_time_micu")] %*%
        t(unit_flow_data[c("l2_waiting_time", "l2_waiting_time_csru", 
                           "l2_waiting_time_sicu", "l2_waiting_time_micu")]) +
        outflow_tsicu_mod$coefficients["STAFF"] * unit_flow_data$l_STAFF_tsicu +
        unit_flow_data$l_INFLOW_tsicu
      , 1/2) + 
    # do_r
    outflow_ccu_mod$coefficients["STAFF"] * r - 
    # inflow
    unit_flow_data$INFLOW#, 1/2)
}

# apply(CCU_flow_clean, 1, waiting_do, r=1) # does not work, why? 

set.seed(123)
test <- sample(1:nrow(CCU_flow_clean),2000,replace = FALSE)
to_plot <- data.frame(ressources=integer(),
                      waiting_time=integer(),
                      nas=integer())
# figure out where na's actually happen
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

glimpse(CCU_flow_clean)
CCU_flow_clean <- CCU_flow_clean %>% rename(l2_waiting_time_ccu = l2_waiting_time)

colnames(CCU_flow_clean[55555,])  %>% rename(l2_waiting_time_ccu = l2_waiting_time)




# second ====
waiting_do2 <- function(unit_flow_data, r, w_max = 1/2){ #}, reg_list){
  unit_flow_data %>% 
    select(STAFF = l_STAFF_csru, l1_waiting_time_ccu = l2_waiting_time,
           l1_waiting_time_micu = l2_waiting_time_micu, 
           l1_waiting_time_sicu = l2_waiting_time_sicu, 
           l1_waiting_time_tsicu = l2_waiting_time_tsicu) -> data_csru
  csru_out_down <- predict(outflow_csru_mod, data_csru)
  csru_wait_down <- 1 / theta_thres(csru_out_down - unit_flow_data$l_INFLOW_csru, w_max)
  
  unit_flow_data %>% 
    select(STAFF = l_STAFF_micu, l1_waiting_time_ccu = l2_waiting_time,
           l1_waiting_time_csru = l2_waiting_time_csru, 
           l1_waiting_time_sicu = l2_waiting_time_sicu, 
           l1_waiting_time_tsicu = l2_waiting_time_tsicu) -> data_micu
  micu_out_down <- predict(outflow_micu_mod, data_micu)
  micu_wait_down <- 1 / theta_thres(micu_out_down - unit_flow_data$l_INFLOW_micu, w_max)
  
  unit_flow_data %>% 
    select(STAFF = l_STAFF_sicu, l1_waiting_time_ccu = l2_waiting_time,
           l1_waiting_time_csru = l2_waiting_time_csru, 
           l1_waiting_time_micu= l2_waiting_time_micu, 
           l1_waiting_time_tsicu = l2_waiting_time_tsicu) -> data_sicu
  sicu_out_down <- predict(outflow_sicu_mod, data_sicu)
  sicu_wait_down <- 1 / theta_thres(sicu_out_down - unit_flow_data$l_INFLOW_sicu, w_max)
  
  unit_flow_data %>% 
    select(STAFF = l_STAFF_tsicu, l1_waiting_time_ccu = l2_waiting_time,
           l1_waiting_time_crsu = l2_waiting_time_csru, 
           l1_waiting_time_micu= l2_waiting_time_micu, 
           l1_waiting_time_sicu = l2_waiting_time_sicu) -> data_tsicu
  tsicu_out_down <- predict(outflow_tsicu_mod, data_tsicu)
  tsicu_wait_down <- 1 / theta_thres(tsicu_out_down - unit_flow_data$l_INFLOW_tsicu, w_max)
  
  
  waiting_down <- c(csru_wait_down, micu_wait_down, sicu_wait_down, tsicu_wait_down)
  names(waiting_down) <- c("l1_waiting_time_csru", "l1_waiting_time_micu", "l1_waiting_time_sicu", 
                           "l1_waiting_time_tsicu")
  model <- as.data.frame(c(unit_flow_data, waiting_down))
  model$STAFF <- r
  1 / theta_thres(predict(outflow_ccu_mod, model) - unit_flow_data$INFLOW, w_max)
}

waiting_do2(CCU_flow_clean[32323,], r=1)

apply(CCU_flow_clean[1:100,], 1, waiting_do2, r=1) # does not work, why? 

set.seed(123)
test <- sample(1:nrow(CCU_flow_clean),2000,replace = FALSE)
to_plot <- data.frame(ressources=integer(),
                      waiting_time=integer(),
                      nas=integer())
results <- vector()
for(r in 1:50){
  for(i in test){
    results[i] <- waiting_do2(CCU_flow_clean[i,], r)
  }
  to_plot[r, "ressources"] <- r
  to_plot[r, "nas"] <- sum(is.na(results)) / nrow(CCU_flow_clean)
  to_plot[r, "waiting_time"] <- mean(results, na.rm = TRUE)
}
plot(to_plot$ressources, to_plot$waiting_time,type = "l", col="red")
#plot(to_plot$ressources, to_plot$nas,type = "l", col="red")

# playground ====
CCU_flow_clean[51663,] %>% 
    select(STAFF = l_STAFF_csru, l1_waiting_time_ccu = l2_waiting_time_ccu,
           l1_waiting_time_micu = l2_waiting_time_micu, 
           l1_waiting_time_sicu = l2_waiting_time_sicu, 
           l1_waiting_time_tsicu = l2_waiting_time_tsicu) -> data_csru
csru_out_down <- predict(outflow_csru_mod, data_csru)
csru_wait_down <- 1 / theta_thres(csru_out_down - CCU_flow_clean[51663,]$l_INFLOW_csru, w_max)

CCU_flow_clean[51663,] %>% 
    select(STAFF = l_STAFF_micu, l1_waiting_time_ccu = l2_waiting_time_ccu,
           l1_waiting_time_csru = l2_waiting_time_csru, 
           l1_waiting_time_sicu = l2_waiting_time_sicu, 
           l1_waiting_time_tsicu = l2_waiting_time_tsicu) -> data_micu
micu_out_down <- predict(outflow_micu_mod, data_micu)
micu_wait_down <- 1 / theta_thres(micu_out_down - CCU_flow_clean[51663,]$l_INFLOW_micu, w_max)

CCU_flow_clean[51663,] %>% 
    select(STAFF = l_STAFF_sicu, l1_waiting_time_ccu = l2_waiting_time_ccu,
           l1_waiting_time_csru = l2_waiting_time_csru, 
           l1_waiting_time_micu= l2_waiting_time_micu, 
           l1_waiting_time_tsicu = l2_waiting_time_tsicu) -> data_sicu
sicu_out_down <- predict(outflow_sicu_mod, data_sicu)
sicu_wait_down <- 1 / theta_thres(sicu_out_down - CCU_flow_clean[51663,]$l_INFLOW_sicu, w_max)

CCU_flow_clean[51663,] %>% 
    select(STAFF = l_STAFF_tsicu, l1_waiting_time_ccu = l2_waiting_time_ccu,
           l1_waiting_time_crsu = l2_waiting_time_csru, 
           l1_waiting_time_micu= l2_waiting_time_micu, 
           l1_waiting_time_sicu = l2_waiting_time_sicu) -> data_tsicu
tsicu_out_down <- predict(outflow_tsicu_mod, data_tsicu)
tsicu_wait_down <- 1 / theta_thres(tsicu_out_down - CCU_flow_clean[51663,]$l_INFLOW_tsicu, w_max)


waiting_down <- c(csru_wait_down, micu_wait_down, sicu_wait_down, tsicu_wait_down)
names(waiting_down) <- c("l1_waiting_time_csru", "l1_waiting_time_micu", "l1_waiting_time_sicu",
                         "l1_waiting_time_tsicu")
model <- as.data.frame(c(CCU_flow_clean[51663,], waiting_down))
model$STAFF <- 1
1 / theta_thres(predict(outflow_ccu_mod, model) - CCU_flow_clean[51663,]$INFLOW, w_max)
