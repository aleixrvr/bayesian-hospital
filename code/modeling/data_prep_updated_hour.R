#rm(list=ls())

# Library ====
library(bigrquery)
library(DBI)
library(data.table)
library(lubridate)
library(ggplot2)
library(dplyr)
library(stargazer)

# functions ====
lagpad <- function(x, k = 1) {
    if (!is.vector(x))
        stop('x must be a vector')
    if (!is.numeric(x))
        stop('x must be numeric')
    if (!is.numeric(k))
        stop('k must be numeric')
    if (1 != length(k))
        stop('k must be a single number')
    c(rep(NA, k), x)[1 : length(x)]
}
theta_thres <- function(net_flow, w_max){
    apply(net_flow, 1, function(x) max(x,(1/w_max)))
}

# Connection to BigQuery ====
Sys.setenv(BIGQUERY_TEST_PROJECT="bgse-dsc")
billing <- bq_test_project()
con <- dbConnect(
    bigrquery::bigquery(),
    project = "bgse-dsc",
    dataset = "MIMIC3_V1_4",
    billing = billing
)

# build date hour framework ====
dates_sql <- paste("SELECT 
                        MIN(EXTRACT(DATE FROM CHARTTIME_COLLAPSED)) AS min_date, 
                        MAX(EXTRACT(DATE FROM CHARTTIME_COLLAPSED)) AS max_date,
                        MIN(EXTRACT(HOUR FROM CHARTTIME_COLLAPSED)) AS min_hour,
                        MAX(EXTRACT(HOUR FROM CHARTTIME_COLLAPSED)) AS max_hour
                   FROM `MIMIC3_V1_4.CHARTEVENTS_DEPTS_CATS_TS_COLLAPSED_NEW`")
dates <- as.data.frame(dbGetQuery(con, dates_sql))
units_sql <- paste("SELECT 
                        DISTINCT CURR_CAREUNIT
                   FROM `MIMIC3_V1_4.CHARTEVENTS_DEPTS_CATS_TS_COLLAPSED_NEW`")
units <- as.data.frame(dbGetQuery(con, units_sql))$CURR_CAREUNIT

frame <- as.data.frame(rep(seq(as.Date(dates[1,1]),as.Date(dates[1,2]), by="day"), each=(24*6)))
frame$CHART_HOUR <- rep(0:23,each=6)
frame$CURR_UNIT <- rep(units,24)
colnames(frame)[1] <- "CHART_DATE"

# Raw Data ====
skeleton_sql <- paste("
SELECT 
    EXTRACT(DATE FROM CHARTTIME_COLLAPSED) AS CHART_DATE,
    EXTRACT(HOUR FROM CHARTTIME_COLLAPSED) AS CHART_HOUR,
    CURR_CAREUNIT AS CURR_UNIT, 
    COUNT(DISTINCT CGID) AS STAFF,
    AVG(LOS_TRANS) AS AVG_LOS,
    COUNT(DISTINCT ICUSTAY_ID) AS PATIENTS
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
flow_data <- frame %>% 
    left_join(y=skeleton, by=c("CHART_DATE", "CHART_HOUR", "CURR_UNIT")) %>%
    left_join(y=detailed_inflow, by=c("CHART_DATE", "CHART_HOUR", "CURR_UNIT")) %>% 
    left_join(y=detailed_outflow, by=c("CHART_DATE", "CHART_HOUR", "CURR_UNIT")) %>% 
    as.data.table()
flow_data[,AVG_LOS:=NULL]
flow_data[,PATIENTS:=NULL]

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

rm(list=c("billing", "con", "detailed_inflow", "detailed_outflow","in_cols", 
          "inflow", "inflow_sql", "out_cols", "outflow", "outflow_sql",
          "relabel_cols", "skeleton", "skeleton_sql", "dates_sql", "units",
          "units_sql", "dates", "frame"))

