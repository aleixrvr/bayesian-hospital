source("code/modeling/data_prep.R")

model_data <- flow_data %>% 
  group_by(CURR_UNIT) %>% 
  mutate_at(c("from_OUT" ,"from_NWARD" ,"from_NICU", "from_MICU", "from_TSICU" ,"from_CSRU" ,"from_SICU" ,"from_CCU" ), lagpad) %>% 
  mutate(net_flow = OUTFLOW - INFLOW,
         max_wait = 1/5,
         waiting_time = 1/pmax(net_flow, max_wait),
         l_waiting_time = lagpad(waiting_time),
         l_OUTFLOW = lagpad(OUTFLOW)) %>% 
  select(-c(AVG_LOS)) %>% 
  ungroup() 

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
  left_join(y = CSRU_flow %>% select(CHART_DATE, l_waiting_time), by =  c("CHART_DATE" = "CHART_DATE")) %>% 
  rename(l_waiting_time_csru = l_waiting_time.y) %>% 
  left_join(y = MICU_flow %>% select(CHART_DATE, l_waiting_time), by =  c("CHART_DATE" = "CHART_DATE")) %>% 
  rename(l_waiting_time_micu = l_waiting_time) %>% 
  left_join(y = SICU_flow %>% select(CHART_DATE, l_waiting_time), by =  c("CHART_DATE" = "CHART_DATE")) %>% 
  rename(l_waiting_time_sicu = l_waiting_time) %>% 
  left_join(y = TSICU_flow %>% select(CHART_DATE, l_waiting_time), by =  c("CHART_DATE" = "CHART_DATE")) %>% 
  rename(l_waiting_time_tsicu = l_waiting_time, 
         l_waiting_time = l_waiting_time.x)

CSRU_flow <- CSRU_flow  %>% 
  left_join(y = CCU_flow %>% select(CHART_DATE, l_waiting_time), by =  c("CHART_DATE" = "CHART_DATE")) %>% 
  rename(l_waiting_time_ccu = l_waiting_time.y) %>% 
  left_join(y = MICU_flow %>% select(CHART_DATE, l_waiting_time), by =  c("CHART_DATE" = "CHART_DATE")) %>% 
  rename(l_waiting_time_micu = l_waiting_time) %>% 
  left_join(y = SICU_flow %>% select(CHART_DATE, l_waiting_time), by =  c("CHART_DATE" = "CHART_DATE")) %>% 
  rename(l_waiting_time_sicu = l_waiting_time) %>% 
  left_join(y = TSICU_flow %>% select(CHART_DATE, l_waiting_time), by =  c("CHART_DATE" = "CHART_DATE")) %>% 
  rename(l_waiting_time_tsicu = l_waiting_time,
         l_waiting_time = l_waiting_time.x)

MICU_flow <- MICU_flow  %>% 
  left_join(y = CCU_flow %>% select(CHART_DATE, l_waiting_time), by =  c("CHART_DATE" = "CHART_DATE")) %>% 
  rename(l_waiting_time_ccu = l_waiting_time.y) %>% 
  left_join(y = CSRU_flow %>% select(CHART_DATE, l_waiting_time), by =  c("CHART_DATE" = "CHART_DATE")) %>% 
  rename(l_waiting_time_csru = l_waiting_time) %>% 
  left_join(y = SICU_flow %>% select(CHART_DATE, l_waiting_time), by =  c("CHART_DATE" = "CHART_DATE")) %>% 
  rename(l_waiting_time_sicu = l_waiting_time) %>% 
  left_join(y = TSICU_flow %>% select(CHART_DATE, l_waiting_time), by =  c("CHART_DATE" = "CHART_DATE")) %>% 
  rename(l_waiting_time_tsicu = l_waiting_time,
         l_waiting_time = l_waiting_time.x)

SICU_flow <- SICU_flow  %>% 
  left_join(y = CCU_flow %>% select(CHART_DATE, l_waiting_time), by =  c("CHART_DATE" = "CHART_DATE")) %>% 
  rename(l_waiting_time_ccu = l_waiting_time.y) %>% 
  left_join(y = CSRU_flow %>% select(CHART_DATE, l_waiting_time), by =  c("CHART_DATE" = "CHART_DATE")) %>% 
  rename(l_waiting_time_csru = l_waiting_time) %>% 
  left_join(y = MICU_flow %>% select(CHART_DATE, l_waiting_time), by =  c("CHART_DATE" = "CHART_DATE")) %>% 
  rename(l_waiting_time_micu = l_waiting_time) %>% 
  left_join(y = TSICU_flow %>% select(CHART_DATE, l_waiting_time), by =  c("CHART_DATE" = "CHART_DATE")) %>% 
  rename(l_waiting_time_tsicu = l_waiting_time,
         l_waiting_time = l_waiting_time.x)

TSICU_flow <- TSICU_flow  %>% 
  left_join(y = CCU_flow %>% select(CHART_DATE, l_waiting_time), by =  c("CHART_DATE" = "CHART_DATE")) %>% 
  rename(l_waiting_time_ccu = l_waiting_time.y) %>% 
  left_join(y = CSRU_flow %>% select(CHART_DATE, l_waiting_time), by =  c("CHART_DATE" = "CHART_DATE")) %>% 
  rename(l_waiting_time_crsu = l_waiting_time) %>% 
  left_join(y = MICU_flow %>% select(CHART_DATE, l_waiting_time), by =  c("CHART_DATE" = "CHART_DATE")) %>% 
  rename(l_waiting_time_micu = l_waiting_time) %>% 
  left_join(y = SICU_flow %>% select(CHART_DATE, l_waiting_time), by =  c("CHART_DATE" = "CHART_DATE")) %>% 
  rename(l_waiting_time_sicu = l_waiting_time,
         l_waiting_time = l_waiting_time.x)


## OLS models

inflow_ccu_mod <- lm(INFLOW ~ ., CCU_flow %>% select(INFLOW, starts_with("from_"), -from_CCU))
outflow_ccu_mod <- lm(OUTFLOW ~ ., CCU_flow %>% select(OUTFLOW, STAFF, starts_with("l_waiting_time_")))

inflow_csru_mod <- lm(INFLOW ~ ., CSRU_flow %>% select(INFLOW, starts_with("from_"), -from_CSRU))
outflow_csru_mod <- lm(OUTFLOW ~ ., CSRU_flow %>% select(OUTFLOW, STAFF, starts_with("l_waiting_time_")))

inflow_micu_mod <- lm(INFLOW ~ ., MICU_flow %>% select(INFLOW, starts_with("from_"), -from_MICU))
outflow_micu_mod <-lm(OUTFLOW ~ ., MICU_flow %>% select(OUTFLOW, STAFF, starts_with("l_waiting_time_")))

inflow_sicu_mod <- lm(INFLOW ~ ., SICU_flow %>% select(INFLOW, starts_with("from_"), -from_SICU))
outflow_sicu_mod <-lm(OUTFLOW ~ ., SICU_flow %>% select(OUTFLOW, STAFF, starts_with("l_waiting_time_")))

inflow_tsicu_mod <- lm(INFLOW ~ ., TSICU_flow %>% select(INFLOW, starts_with("from_"), -from_TSICU))
outflow_tsiu_mod <-lm(OUTFLOW ~ ., TSICU_flow %>% select(OUTFLOW, STAFF, starts_with("l_waiting_time_")))


stargazer(inflow_ccu_mod, inflow_csru_mod, inflow_micu_mod, inflow_sicu_mod, inflow_tsicu_mod, title="Results", align=TRUE, type = "latex")
stargazer(outflow_ccu_mod, outflow_csru_mod, outflow_micu_mod, outflow_sicu_mod, inflow_tsicu_mod, title="Results", align=TRUE, type = "latex")



