library(stargazer)
#source("code/modeling/data_prep_hour.R")
#source("../modeling/data_prep_hour.R")

model_data <- flow_data %>% 
  group_by(CURR_UNIT) %>% 
  mutate_at(c("from_OUT" ,"from_NWARD" ,"from_NICU", "from_MICU", "from_TSICU" ,"from_CSRU" ,"from_SICU" ,"from_CCU" ), lagpad) %>% 
  mutate(net_flow = OUTFLOW - INFLOW,
         max_wait = 1/2,
         waiting_time = 1/pmax(net_flow, max_wait),
         l1_waiting_time = lagpad(waiting_time),
         l2_waiting_time = lagpad(waiting_time, 2),
         l_OUTFLOW = lagpad(OUTFLOW),
         l_STAFF = lagpad(STAFF),
         l_INFLOW = lagpad(INFLOW)) %>% 
  select(-c(AVG_LOS)) %>% 
  ungroup() 

glimpse(model_data)

CCU_flow <- model_data %>% 
  filter(CURR_UNIT == "CCU")

CSRU_flow <- model_data %>% 
  filter(CURR_UNIT == "CSRU") 

MICU_flow <- model_data %>% 
  filter(CURR_UNIT == "MICU") 

SICU_flow <- model_data %>% 
  filter(CURR_UNIT == "SICU") 

TSICU_flow <- model_data %>% 
  filter(CURR_UNIT == "TSICU") 

CCU_flow <- CCU_flow  %>% 
  left_join(y = CSRU_flow %>% select(CHART_DATE, CHART_HOUR, l1_waiting_time, l2_waiting_time, l_STAFF, l_INFLOW), 
            by =  c("CHART_DATE", "CHART_HOUR")) %>% 
  rename(l1_waiting_time_csru = l1_waiting_time.y,
         l2_waiting_time_csru = l2_waiting_time.y,
         l_STAFF_csru = l_STAFF.y,
         l_INFLOW_csru = l_INFLOW.y) %>% 
  left_join(y = MICU_flow %>% select(CHART_DATE, CHART_HOUR, l1_waiting_time, l2_waiting_time, l_STAFF, l_INFLOW), 
            by =  c("CHART_DATE", "CHART_HOUR")) %>% 
  rename(l1_waiting_time_micu = l1_waiting_time,
         l2_waiting_time_micu = l2_waiting_time,
         l_STAFF_micu = l_STAFF,
         l_INFLOW_micu = l_INFLOW) %>% 
  left_join(y = SICU_flow %>% select(CHART_DATE, CHART_HOUR, l1_waiting_time, l2_waiting_time, l_STAFF, l_INFLOW), 
            by =  c("CHART_DATE","CHART_HOUR")) %>% 
  rename(l1_waiting_time_sicu = l1_waiting_time,
         l2_waiting_time_sicu = l2_waiting_time,
         l_STAFF_sicu = l_STAFF,
         l_INFLOW_sicu = l_INFLOW) %>% 
  left_join(y = TSICU_flow %>% select(CHART_DATE, CHART_HOUR, l1_waiting_time, l2_waiting_time, l_STAFF, l_INFLOW), 
            by =  c("CHART_DATE", "CHART_HOUR")) %>% 
  rename(l1_waiting_time_tsicu = l1_waiting_time,
         l2_waiting_time_tsicu = l2_waiting_time,
         l_STAFF_tsicu = l_STAFF,
         l_INFLOW_tsicu = l_INFLOW,
         l_STAFF = l_STAFF.x,
         l_INFLOW = l_INFLOW.x,
         l1_waiting_time = l1_waiting_time.x,
         l2_waiting_time = l2_waiting_time.x)

CSRU_flow <- CSRU_flow  %>% 
  left_join(y = CCU_flow %>% select(CHART_DATE, l1_waiting_time, CHART_HOUR), by =  c("CHART_DATE", "CHART_HOUR")) %>% 
  rename(l1_waiting_time_ccu = l1_waiting_time.y) %>% 
  left_join(y = MICU_flow %>% select(CHART_DATE, l1_waiting_time, CHART_HOUR), by =  c("CHART_DATE", "CHART_HOUR")) %>% 
  rename(l1_waiting_time_micu = l1_waiting_time) %>% 
  left_join(y = SICU_flow %>% select(CHART_DATE, l1_waiting_time, CHART_HOUR), by =  c("CHART_DATE", "CHART_HOUR")) %>% 
  rename(l1_waiting_time_sicu = l1_waiting_time) %>% 
  left_join(y = TSICU_flow %>% select(CHART_DATE, l1_waiting_time, CHART_HOUR), by =  c("CHART_DATE", "CHART_HOUR")) %>% 
  rename(l1_waiting_time_tsicu = l1_waiting_time,
         l1_waiting_time = l1_waiting_time.x)

glimpse(CSRU_flow)
MICU_flow <- MICU_flow  %>% 
  left_join(y = CCU_flow %>% select(CHART_DATE, l1_waiting_time, CHART_HOUR), by =  c("CHART_DATE", "CHART_HOUR")) %>% 
  rename(l1_waiting_time_ccu = l1_waiting_time.y) %>% 
  left_join(y = CSRU_flow %>% select(CHART_DATE, l1_waiting_time, CHART_HOUR), by =  c("CHART_DATE", "CHART_HOUR")) %>% 
  rename(l1_waiting_time_csru = l1_waiting_time) %>% 
  left_join(y = SICU_flow %>% select(CHART_DATE, l1_waiting_time, CHART_HOUR), by =  c("CHART_DATE", "CHART_HOUR")) %>%  
  rename(l1_waiting_time_sicu = l1_waiting_time) %>% 
  left_join(y = TSICU_flow %>% select(CHART_DATE, l1_waiting_time, CHART_HOUR), by =  c("CHART_DATE", "CHART_HOUR")) %>%  
  rename(l1_waiting_time_tsicu = l1_waiting_time,
         l1_waiting_time = l1_waiting_time.x)

SICU_flow <- SICU_flow  %>% 
  left_join(y = CCU_flow %>% select(CHART_DATE, l1_waiting_time, CHART_HOUR), by =  c("CHART_DATE", "CHART_HOUR")) %>% 
  rename(l1_waiting_time_ccu = l1_waiting_time.y) %>% 
  left_join(y = CSRU_flow %>% select(CHART_DATE, l1_waiting_time, CHART_HOUR), by =  c("CHART_DATE", "CHART_HOUR")) %>% 
  rename(l1_waiting_time_csru = l1_waiting_time) %>% 
  left_join(y = MICU_flow %>% select(CHART_DATE, l1_waiting_time, CHART_HOUR), by =  c("CHART_DATE", "CHART_HOUR")) %>% 
  rename(l1_waiting_time_micu = l1_waiting_time) %>% 
  left_join(y = TSICU_flow %>% select(CHART_DATE, l1_waiting_time, CHART_HOUR), by =  c("CHART_DATE", "CHART_HOUR")) %>% 
  rename(l1_waiting_time_tsicu = l1_waiting_time,
         l1_waiting_time = l1_waiting_time.x)

TSICU_flow <- TSICU_flow  %>% 
  left_join(y = CCU_flow %>% select(CHART_DATE, l1_waiting_time, CHART_HOUR), by =  c("CHART_DATE", "CHART_HOUR")) %>% 
  rename(l1_waiting_time_ccu = l1_waiting_time.y) %>% 
  left_join(y = CSRU_flow %>% select(CHART_DATE, l1_waiting_time, CHART_HOUR), by =  c("CHART_DATE", "CHART_HOUR")) %>% 
  rename(l1_waiting_time_crsu = l1_waiting_time) %>% 
  left_join(y = MICU_flow %>% select(CHART_DATE, l1_waiting_time, CHART_HOUR), by =  c("CHART_DATE", "CHART_HOUR")) %>% 
  rename(l1_waiting_time_micu = l1_waiting_time) %>% 
  left_join(y = SICU_flow %>% select(CHART_DATE, l1_waiting_time, CHART_HOUR), by =  c("CHART_DATE", "CHART_HOUR")) %>% 
  rename(l1_waiting_time_sicu = l1_waiting_time,
         l1_waiting_time = l1_waiting_time.x)


## OLS models

inflow_ccu_mod <- lm(INFLOW ~ ., CCU_flow %>% select(INFLOW, starts_with("from_"), -from_CCU))
outflow_ccu_mod <- lm(OUTFLOW ~ ., CCU_flow %>% select(OUTFLOW, STAFF, starts_with("l1_waiting_time_")))

inflow_csru_mod <- lm(INFLOW ~ ., CSRU_flow %>% select(INFLOW, starts_with("from_"), -from_CSRU))
outflow_csru_mod <- lm(OUTFLOW ~ ., CSRU_flow %>% select(OUTFLOW, STAFF, starts_with("l1_waiting_time_")))

inflow_micu_mod <- lm(INFLOW ~ ., MICU_flow %>% select(INFLOW, starts_with("from_"), -from_MICU))
outflow_micu_mod <-lm(OUTFLOW ~ ., MICU_flow %>% select(OUTFLOW, STAFF, starts_with("l1_waiting_time_")))

inflow_sicu_mod <- lm(INFLOW ~ ., SICU_flow %>% select(INFLOW, starts_with("from_"), -from_SICU))
outflow_sicu_mod <-lm(OUTFLOW ~ ., SICU_flow %>% select(OUTFLOW, STAFF, starts_with("l1_waiting_time_")))

inflow_tsicu_mod <- lm(INFLOW ~ ., TSICU_flow %>% select(INFLOW, starts_with("from_"), -from_TSICU))
outflow_tsicu_mod <-lm(OUTFLOW ~ ., TSICU_flow %>% select(OUTFLOW, STAFF, starts_with("l1_waiting_time_")))


summary(outflow_tsiu_mod)

stargazer(inflow_ccu_mod, inflow_csru_mod, inflow_micu_mod, inflow_sicu_mod, inflow_tsicu_mod, title="Results", align=TRUE, type = "latex")
stargazer(outflow_ccu_mod, outflow_csru_mod, outflow_micu_mod, outflow_sicu_mod, inflow_tsicu_mod, title="Results", align=TRUE, type = "latex")


ggplotRegression <- function (fit) {
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}


ggplotRegression(outflow_micu_mod)


