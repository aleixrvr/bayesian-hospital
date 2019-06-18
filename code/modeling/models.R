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
  rename(l_waiting_time_tsicu = l_waiting_time)

summary(lm(INFLOW ~ ., CCU_flow %>% select(INFLOW, starts_with("from_"), from_CCU)))
summary(lm(OUTFLOW ~ ., CCU_flow %>% select(OUTFLOW, STAFF, starts_with("l_waiting_time_"))))








