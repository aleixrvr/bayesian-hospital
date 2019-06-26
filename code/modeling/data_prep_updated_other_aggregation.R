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

# Specify shift length ====
total_hours <- 24
splits <- 2
shift_selection <- "CASE \n"
pos = 1
for( time in seq(splits, total_hours, splits)){
  shift_selection <- paste0(shift_selection, "WHEN EXTRACT(HOUR FROM CHARTTIME_COLLAPSED) < ")
  shift_selection <- paste0(shift_selection, time)
  shift_selection <- paste0(shift_selection, ' THEN ')
  shift_selection <- paste0(shift_selection, pos)
  shift_selection <- paste0(shift_selection, '\n')
  pos = pos + 1
  
}
shift_selection <- paste0(shift_selection, 'END')

# build date hour framework ====
dates_sql <- paste("SELECT 
                        MIN(EXTRACT(DATE FROM CHARTTIME_COLLAPSED)) AS min_date, 
                        MAX(EXTRACT(DATE FROM CHARTTIME_COLLAPSED)) AS max_date
                   FROM `MIMIC3_V1_4.CHARTEVENTS_DEPTS_CATS_TS_COLLAPSED_NEW`")
dates <- as.data.frame(dbGetQuery(con, dates_sql))
units_sql <- paste("SELECT 
                        DISTINCT CURR_CAREUNIT
                   FROM `MIMIC3_V1_4.CHARTEVENTS_DEPTS_CATS_TS_COLLAPSED_NEW`")
units <- as.data.frame(dbGetQuery(con, units_sql))$CURR_CAREUNIT

frame <- as.data.frame(rep(seq(as.Date(dates[1,1]),as.Date(dates[1,2]), by="day"), each=(3*6)))
frame$CHART_SHIFT <- rep(1:3,each=6)
frame$CURR_UNIT <- rep(units,3)
colnames(frame)[1] <- "CHART_DATE"

# Raw Data ====
skeleton_sql <- paste("
SELECT 
    EXTRACT(DATE FROM CHARTTIME_COLLAPSED) AS CHART_DATE, ",
    shift_selection, "AS CHART_SHIFT,
    CURR_CAREUNIT AS CURR_UNIT, 
    COUNT(DISTINCT CGID) AS STAFF,
    AVG(LOS_TRANS) AS AVG_LOS,
    COUNT(DISTINCT ICUSTAY_ID) AS PATIENTS
FROM `MIMIC3_V1_4.CHARTEVENTS_DEPTS_CATS_TS_COLLAPSED_NEW`
GROUP BY CHART_DATE, CHART_SHIFT, CURR_UNIT
ORDER BY CHART_DATE, CHART_SHIFT
")
skeleton <- dbGetQuery(con, skeleton_sql)
skeleton <- as.data.table(skeleton)
inflow_sql <- paste("
SELECT 
    ICUSTAY_ID,
    CURR_CAREUNIT AS CURR_UNIT,
    IFNULL(ANY_VALUE(PREV_CAREUNIT), ", "'OUT'" , ") AS PREV_CAREUNIT,
    EXTRACT(DATE FROM ANY_VALUE(INTIME_TRANS_COLLAPSED)) AS CHART_DATE,",
                    shift_selection, " AS CHART_SHIFT
FROM `MIMIC3_V1_4.CHARTEVENTS_DEPTS_CATS_TS_COLLAPSED_NEW`
GROUP BY ICUSTAY_ID, CURR_UNIT
ORDER BY CHART_DATE, CHART_SHIFT
")
inflow <- dbGetQuery(con, inflow_sql)
inflow <- as.data.table(inflow)
outflow_sql <- paste("
SELECT ICUSTAY_ID, CHART_DATE, CHART_SHIFT, CURR_UNIT, IFNULL(NEXT_CAREUNIT,'OUT') AS NEXT_CAREUNIT
FROM 
(   SELECT 
        ICUSTAY_ID,
        CURR_CAREUNIT AS CURR_UNIT, 
        ANY_VALUE(EXTRACT(DATE FROM OUTTIME_TRANS_COLLAPSED)) AS CHART_DATE,",
                     shift_selection, " AS CHART_SHIFT
    FROM `MIMIC3_V1_4.CHARTEVENTS_DEPTS_CATS_TS_COLLAPSED_FINAL` 
    GROUP BY ICUSTAY_ID, CURR_UNIT) AS L
LEFT JOIN 
(   SELECT 
        ICUSTAY_ID,
        CURR_CAREUNIT AS NEXT_CAREUNIT, 
        ANY_VALUE(EXTRACT(DATE FROM INTIME_TRANS_COLLAPSED)) AS CHART_DATE,",
                     shift_selection, " AS CHART_SHIFT
    FROM `MIMIC3_V1_4.CHARTEVENTS_DEPTS_CATS_TS_COLLAPSED_FINAL` 
    GROUP BY ICUSTAY_ID, NEXT_CAREUNIT) AS R
USING(ICUSTAY_ID, CHART_DATE, CHART_SHIFT)
ORDER BY CHART_DATE, CHART_SHIFT
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
                          by=list(CHART_DATE, CHART_SHIFT,CURR_UNIT)]

detailed_outflow <- outflow[,.("to_OUT"=sum(NEXT_CAREUNIT=="OUT"), 
                               "to_NWARD"=sum(NEXT_CAREUNIT=="NWARD"),
                               "to_NICU"=sum(NEXT_CAREUNIT=="NICU"),
                               "to_MICU"=sum(NEXT_CAREUNIT=="MICU"),
                               "to_TSICU"=sum(NEXT_CAREUNIT=="TSICU"),
                               "to_CSRU"=sum(NEXT_CAREUNIT=="CSRU"),
                               "to_SICU"=sum(NEXT_CAREUNIT=="SICU"),
                               "to_CCU"=sum(NEXT_CAREUNIT=="CCU")), 
                            by=list(CHART_DATE, CHART_SHIFT,CURR_UNIT)]

# Merge stuff ====
flow_data <- frame %>% 
    left_join(y=skeleton, by=c("CHART_DATE", "CHART_SHIFT", "CURR_UNIT")) %>%
    left_join(y=detailed_inflow, by=c("CHART_DATE", "CHART_SHIFT", "CURR_UNIT")) %>% 
    left_join(y=detailed_outflow, by=c("CHART_DATE", "CHART_SHIFT", "CURR_UNIT")) %>% 
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

# Create relevant features for each dataset ====
unit_flow <- split(flow_data,by="CURR_UNIT")
unit_flow <- lapply(unit_flow, function(X){
  X$waiting_time <- 1 / theta_thres(data.frame(X$OUTFLOW - X$INFLOW), 1)
  X[is.na(X$STAFF),]$waiting_time <- 0
  X$l1_waiting_time <- lagpad(X$waiting_time)
  X$l2_waiting_time <- lagpad(X$waiting_time, 2)
  # X[is.na(X$STAFF),"STAFF"] <- median(X$STAFF, na.rm = TRUE)
  X$l_OUTFLOW = lagpad(X$OUTFLOW)
  X$l_STAFF = lagpad(X$STAFF)
  X$l_INFLOW = lagpad(X$INFLOW)
  #X <- X[complete.cases(X)]
  as.data.table(X)
})

# Merge  relevant features for each dataset ====
units <- names(unit_flow)
for(i in units){
  to_merge <- units[units != i]
  for(m in to_merge){
    unit_flow[[i]] <- merge(unit_flow[[i]], unit_flow[[m]][,c("CHART_DATE", 
                                                              "CHART_SHIFT", 
                                                              "l1_waiting_time", 
                                                              "l2_waiting_time", 
                                                              "l_STAFF", 
                                                              "l_INFLOW")], 
                            by = c("CHART_DATE", "CHART_SHIFT"), 
                            suffixes = c("", paste0("_", m)))
  }
}

unit_flow_shift <- unit_flow

rm(list=c("billing", "con", "detailed_inflow", "detailed_outflow","in_cols", 
          "inflow", "inflow_sql", "out_cols", "outflow", "outflow_sql",
          "relabel_cols", "skeleton", "skeleton_sql", "dates_sql", "units",
          "units_sql", "dates", "frame","to_merge", "i", "m", "lagpad", 
          "flow_data", "unit_flow"))

