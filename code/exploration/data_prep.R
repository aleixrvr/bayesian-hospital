#rm(list=ls())

# Library ====
library(bigrquery)
library(DBI)
library(data.table)
library(lubridate)
library(ggplot2)
library(dplyr)

# Connection to BigQuery ====
Sys.setenv(BIGQUERY_TEST_PROJECT="bgse-dsc")
billing <- bq_test_project()
con <- dbConnect(
    bigrquery::bigquery(),
    project = "bgse-dsc",
    dataset = "MIMIC3_V1_4",
    billing = billing
)

# Raw Data ====
skeleton_sql <- paste("
SELECT 
    EXTRACT(DATE FROM CHARTTIME_COLLAPSED) AS CHART_DATE,
    EXTRACT(HOUR FROM CHARTTIME_COLLAPSED) AS CHART_HOUR,
    CURR_CAREUNIT AS CURR_UNIT, 
    COUNT(DISTINCT CGID) AS STAFF,
    AVG(LOS_TRANS) AS AVG_LOS
FROM `MIMIC3_V1_4.CHARTEVENTS_DEPTS_CATS_TS_COLLAPSED_NEW`
GROUP BY CHART_DATE, CHART_HOUR, CURR_UNIT
ORDER BY CHART_DATE, CHART_HOUR
")
skeleton <- dbGetQuery(con, skeleton_sql)
skeleton <- as.data.table(skeleton)
inflow_sql <- paste("
SELECT 
    ICUSTAY_ID,
    CURR_CAREUNIT AS CURR_UNIT,
    IFNULL(ANY_VALUE(PREV_CAREUNIT), ", "'OUT'" , ") AS PREV_CAREUNIT,
    EXTRACT(DATE FROM ANY_VALUE(INTIME_TRANS_COLLAPSED)) AS CHART_DATE,
    EXTRACT(HOUR FROM ANY_VALUE(INTIME_TRANS_COLLAPSED)) AS CHART_HOUR
FROM `MIMIC3_V1_4.CHARTEVENTS_DEPTS_CATS_TS_COLLAPSED_NEW`
GROUP BY ICUSTAY_ID, CURR_UNIT
ORDER BY CHART_DATE, CHART_HOUR
")
inflow <- dbGetQuery(con, inflow_sql)
inflow <- as.data.table(inflow)
outflow_sql <- paste("
SELECT ICUSTAY_ID, CHART_DATE, CHART_HOUR, CURR_UNIT, IFNULL(NEXT_CAREUNIT,'OUT') AS NEXT_CAREUNIT
FROM 
(   SELECT 
        ICUSTAY_ID,
        CURR_CAREUNIT AS CURR_UNIT, 
        ANY_VALUE(EXTRACT(DATE FROM OUTTIME_TRANS_COLLAPSED)) AS CHART_DATE,
        ANY_VALUE(EXTRACT(HOUR FROM OUTTIME_TRANS_COLLAPSED)) AS CHART_HOUR
    FROM `MIMIC3_V1_4.CHARTEVENTS_DEPTS_CATS_TS_COLLAPSED_FINAL` 
    GROUP BY ICUSTAY_ID, CURR_UNIT) AS L
LEFT JOIN 
(   SELECT 
        ICUSTAY_ID,
        CURR_CAREUNIT AS NEXT_CAREUNIT, 
        ANY_VALUE(EXTRACT(DATE FROM INTIME_TRANS_COLLAPSED)) AS CHART_DATE,
        ANY_VALUE(EXTRACT(HOUR FROM INTIME_TRANS_COLLAPSED)) AS CHART_HOUR
    FROM `MIMIC3_V1_4.CHARTEVENTS_DEPTS_CATS_TS_COLLAPSED_FINAL` 
    GROUP BY ICUSTAY_ID, NEXT_CAREUNIT) AS R
USING(ICUSTAY_ID, CHART_DATE, CHART_HOUR)
ORDER BY CHART_DATE, CHART_HOUR
")
outflow <- dbGetQuery(con, outflow_sql)
outflow <- as.data.table(outflow)

# Make detailed Flows ====
detailed_inflow <- inflow[,.("from_OUT"=sum(PREV_CAREUNIT=="OUT"), 
                                "from_NWARD"=sum(PREV_CAREUNIT=="NWARD"),
                                "from_NICU"=sum(PREV_CAREUNIT=="NICU"),
                                "from_MICU"=sum(PREV_CAREUNIT=="MICU"),
                                "from_TSICU"=sum(PREV_CAREUNIT=="TSICU"),
                                "from_CSRU"=sum(PREV_CAREUNIT=="CSRU"),
                                "from_SICU"=sum(PREV_CAREUNIT=="SICU"),
                                "from_CCU"=sum(PREV_CAREUNIT=="CCU")), 
                             by=list(CHART_DATE, CHART_HOUR,CURR_UNIT)]

detailed_outflow <- outflow[,.("to_OUT"=sum(NEXT_CAREUNIT=="OUT"), 
                                 "to_NWARD"=sum(NEXT_CAREUNIT=="NWARD"),
                                 "to_NICU"=sum(NEXT_CAREUNIT=="NICU"),
                                 "to_MICU"=sum(NEXT_CAREUNIT=="MICU"),
                                 "to_TSICU"=sum(NEXT_CAREUNIT=="TSICU"),
                                 "to_CSRU"=sum(NEXT_CAREUNIT=="CSRU"),
                                 "to_SICU"=sum(NEXT_CAREUNIT=="SICU"),
                                 "to_CCU"=sum(NEXT_CAREUNIT=="CCU")), 
                              by=list(CHART_DATE, CHART_HOUR,CURR_UNIT)]

# Merge stuff ====
flow_data <- skeleton  %>% 
    left_join(y=detailed_inflow, by=c("CHART_DATE", "CHART_HOUR", "CURR_UNIT")) %>% 
    left_join(y=detailed_outflow, by=c("CHART_DATE", "CHART_HOUR", "CURR_UNIT")) %>% 
    as.data.table()

# Cleaning Dataset ====
in_cols <- c("from_OUT", "from_NWARD", "from_NICU", "from_MICU", 
                  "from_TSICU", "from_CSRU", "from_SICU", "from_CCU")
out_cols <- c("to_OUT", "to_NWARD", "to_NICU", "to_MICU", "to_TSICU",
              "to_CSRU", "to_SICU", "to_CCU")
relabel_cols <- c(in_cols, out_cols)
flow_data <- as.data.frame(flow_data)
flow_data[relabel_cols][is.na(flow_data[relabel_cols])] <- 0 
flow_data["INFLOW"] <- rowSums(flow_data[in_cols])
flow_data["OUTFLOW"] <- rowSums(flow_data[out_cols])
flow_data <- as.data.table(flow_data)




# old queries ====
inflow_sql <- paste("
SELECT 
    CHART_DATE, CHART_HOUR, CURR_UNIT, STAFF, AVG_LOS, PREV_CAREUNIT, AVG_LOS / STAFF AS LOS_PRO
FROM 
(   SELECT 
        EXTRACT(DATE FROM CHARTTIME_COLLAPSED) AS CHART_DATE,
        EXTRACT(HOUR FROM CHARTTIME_COLLAPSED) AS CHART_HOUR,
        CURR_CAREUNIT AS CURR_UNIT, 
        COUNT(DISTINCT CGID) AS STAFF,
        AVG(LOS_TRANS) AS AVG_LOS
    FROM `MIMIC3_V1_4.CHARTEVENTS_DEPTS_CATS_TS_COLLAPSED_NEW`
    GROUP BY CHART_DATE, CHART_HOUR, CURR_UNIT) AS L
LEFT JOIN 
(   SELECT 
        ICUSTAY_ID,
        CURR_CAREUNIT AS CURR_UNIT,
        IFNULL(ANY_VALUE(PREV_CAREUNIT), ", "'OUT'" , ") AS PREV_CAREUNIT,
        EXTRACT(DATE FROM ANY_VALUE(INTIME_TRANS_COLLAPSED)) AS CHART_DATE,
        EXTRACT(HOUR FROM ANY_VALUE(INTIME_TRANS_COLLAPSED)) AS CHART_HOUR
    FROM `MIMIC3_V1_4.CHARTEVENTS_DEPTS_CATS_TS_COLLAPSED_NEW`
    GROUP BY ICUSTAY_ID, CURR_UNIT) AS R
USING (CHART_DATE, CHART_HOUR, CURR_UNIT)
ORDER BY CHART_DATE, CHART_HOUR")
inflow <- dbGetQuery(con, inflow_sql)
inflow <- as.data.table(inflow)

outflow_sql_old <- paste("
SELECT 
    ICUSTAY_ID,
    CURR_CAREUNIT, 
    EXTRACT(DATE FROM ANY_VALUE(OUTTIME_TRANS_COLLAPSED)) AS OUT_DATE,
    EXTRACT(HOUR FROM ANY_VALUE(OUTTIME_TRANS_COLLAPSED)) AS OUT_HOUR
FROM `MIMIC3_V1_4.CHARTEVENTS_DEPTS_CATS_TS_COLLAPSED_NEW`
GROUP BY ICUSTAY_ID, CURR_CAREUNIT
ORDER BY OUT_DATE, OUT_HOUR")
outflow <- dbGetQuery(con, outflow_sql_old) %>% 
    select(-c(ICUSTAY_ID)) %>% 
    group_by(CURR_CAREUNIT, OUT_DATE, OUT_HOUR) %>% 
    summarise(OUTFLOW = n())
outflow <- as.data.table(outflow)
flow_data <- inflow  %>% 
    left_join(y = outflow, by =  c("CURR_UNIT" = "CURR_CAREUNIT", 
                                   "CHART_DATE" = "OUT_DATE", 
                                   "CHART_HOUR" = "OUT_HOUR")) %>% as.data.table()
flow_data[is.na(OUTFLOW), OUTFLOW := 0]







flow_data[,PREV_CAREUNIT:= NULL] %>% as.data.frame() -> flow_data
relabel_cols <- c("from_OUT", "from_NWARD", "from_NICU", "from_MICU", 
                  "from_TSICU", "from_CSRU", "from_SICU", "from_CCU")
flow_data[relabel_cols][is.na(flow_data[relabel_cols])] <- 0

flow_data <- flow_data %>% 
  mutate(INFLOW = select(., from_OUT:from_CSRU) %>% rowSums(na.rm = TRUE)) %>%
  select(CHART_DATE, CHART_HOUR, CURR_UNIT, STAFF, AVG_LOS, LOS_PRO, OUTFLOW, INFLOW, everything()) %>% 
  as.data.table() 

View(flow_data[1:100])



