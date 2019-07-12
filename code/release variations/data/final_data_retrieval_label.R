#rm(list=ls())

# Library ====
library(bigrquery)
library(DBI)
library(data.table)
library(lubridate)
library(ggplot2)
library(dplyr)
library(stargazer)

# Functions ====
lagpad <- function(x, k = 1) {
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

# Specify shift and maximum waiting time length ====
total_hours <- 24
splits <- 4
shift_selection <- "CASE \n" # shift for charttime
pos <- 1
for( time in seq(splits, total_hours, splits)){
  shift_selection <- paste0(shift_selection, 
                            "WHEN EXTRACT(HOUR FROM CHARTTIME_COLLAPSED) < ",
                            time, " THEN ", pos, "\n")
  pos <- pos + 1
}
shift_selection <- paste0(shift_selection, "END")

shift_selection_2 <- "CASE \n" # shift for intime
pos <- 1
for( time in seq(splits, total_hours, splits)){
  shift_selection_2 <- paste0(shift_selection_2, 
                              "WHEN EXTRACT(HOUR FROM ANY_VALUE(INTIME_TRANS_COLLAPSED)) < ",
                              time, " THEN ", pos, "\n")
  pos <- pos + 1
}
shift_selection_2 <- paste0(shift_selection_2, "END")

shift_selection_3 <- "CASE \n" # shift for outtime
pos <- 1
for( time in seq(splits, total_hours, splits)){
  shift_selection_3 <- paste0(shift_selection_3, 
                              "WHEN EXTRACT(HOUR FROM ANY_VALUE(OUTTIME_TRANS_COLLAPSED)) < ",
                              time, " THEN ", pos, "\n")
  pos <- pos + 1
}
shift_selection_3 <- paste0(shift_selection_3, "END")

# Build date hour framework ====
dates_sql <- paste("SELECT 
                   MIN(EXTRACT(DATE FROM CHARTTIME_COLLAPSED)) AS min_date, 
                   MAX(EXTRACT(DATE FROM CHARTTIME_COLLAPSED)) AS max_date
                   FROM `MIMIC3_V1_4.CHARTEVENTS_DEPTS_CATS_TS_COLLAPSED_NEW`")
dates <- as.data.frame(dbGetQuery(con, dates_sql))
units_sql <- paste("SELECT 
                   DISTINCT CURR_CAREUNIT
                   FROM `MIMIC3_V1_4.CHARTEVENTS_DEPTS_CATS_TS_COLLAPSED_NEW`")
units <- as.data.frame(dbGetQuery(con, units_sql))$CURR_CAREUNIT

frame <- as.data.frame(rep(seq(as.Date(dates[1,1]),as.Date(dates[1,2]), by="day"), 
                           each=(6 * total_hours / splits )))
frame$CHART_SHIFT <- rep(1:(total_hours / splits),each=6)
frame$CURR_UNIT <- rep(units,(total_hours / splits))
colnames(frame)[1] <- "CHART_DATE"

# Download staff data ====
staff_sql <- paste("
                   SELECT * FROM((
                   SELECT 
                   EXTRACT(DATE FROM CHARTTIME_COLLAPSED) AS CHART_DATE, ",
                   shift_selection, "AS CHART_SHIFT,
                   CURR_CAREUNIT AS CURR_UNIT, 
                   CGID
                   FROM `MIMIC3_V1_4.CHARTEVENTS_DEPTS_CATS_TS_COLLAPSED_NEW`
                   GROUP BY CHART_DATE, CHART_SHIFT, CURR_UNIT, CGID) AS L 
                   LEFT JOIN (
                   SELECT CGID, LABEL FROM `MIMIC3_V1_4.CAREGIVERS` ) AS R
                   USING(CGID))
                   ORDER BY CHART_DATE, CHART_SHIFT 
                   ")
staff <- dbGetQuery(con, staff_sql)
staff <- as.data.table(staff)
staff_labels <- staff[, LABEL] %>% unique()
co_labels <- which(staff_labels %>% tolower() %>% substr(1, 2) == 'co')
staff[is.na(LABEL), LABEL := 'Unknown']

staff <- staff[,.("RN"=sum(LABEL %in% c("RN", "Rn", "RNs", "RNC", "rn")),
                  "RRT"=sum(LABEL %in% c("RRT", "rrt", "RRt", "RRts", "RRTs")),
                  "IMD"=sum(LABEL=="IMD"),
                  "RT"=sum(LABEL=="RT"),
                  "MD"=sum(LABEL=="MD"),
                  "MS"=sum(LABEL=="MS"),
                  "PCA"=sum(LABEL %in% c("PCA", "PCT", "NA", "PC")),
                  "CO"=sum(LABEL %in% staff_labels[co_labels]),
                  "Unknown"=sum(LABEL=="Unknown")),
               by=list(CHART_DATE, CHART_SHIFT,CURR_UNIT)]

colnames(staff) <- c(colnames(staff)[1:3], paste0("STAFF_", colnames(staff)
                                                  [4:length(colnames(staff))]))

# Skeleton
skeleton_sql <- paste("
                      SELECT 
                      EXTRACT(DATE FROM CHARTTIME_COLLAPSED) AS CHART_DATE, ",
                      shift_selection, "AS CHART_SHIFT,
                      CURR_CAREUNIT AS CURR_UNIT, 
                      COUNT(DISTINCT CGID) AS STAFF,
                      AVG(LOS_TRANS) AS AVG_LOS
                      FROM `MIMIC3_V1_4.CHARTEVENTS_DEPTS_CATS_TS_COLLAPSED_NEW`
                      GROUP BY CHART_DATE, CHART_SHIFT, CURR_UNIT
                      ORDER BY CHART_DATE, CHART_SHIFT
                      ")
skeleton <- dbGetQuery(con, skeleton_sql)
skeleton <- as.data.table(skeleton)

# Inflow data ====
inflow_sql <- paste("
                    SELECT 
                    ICUSTAY_ID,
                    CURR_CAREUNIT AS CURR_UNIT,
                    IFNULL(ANY_VALUE(PREV_CAREUNIT), ", "'OUT'" , ") AS PREV_CAREUNIT,
                    EXTRACT(DATE FROM ANY_VALUE(INTIME_TRANS_COLLAPSED)) AS CHART_DATE,",
                    shift_selection_2, " AS CHART_SHIFT
                    FROM `MIMIC3_V1_4.CHARTEVENTS_DEPTS_CATS_TS_COLLAPSED_NEW`
                    GROUP BY ICUSTAY_ID, CURR_UNIT
                    ORDER BY CHART_DATE, CHART_SHIFT
                    ")
inflow <- dbGetQuery(con, inflow_sql)
inflow <- as.data.table(inflow)
detailed_inflow <- inflow[,.("from_OUT"=sum(PREV_CAREUNIT=="OUT"), 
                             "from_NWARD"=sum(PREV_CAREUNIT=="NWARD"),
                             "from_NICU"=sum(PREV_CAREUNIT=="NICU"),
                             "from_MICU"=sum(PREV_CAREUNIT=="MICU"),
                             "from_TSICU"=sum(PREV_CAREUNIT=="TSICU"),
                             "from_CSRU"=sum(PREV_CAREUNIT=="CSRU"),
                             "from_SICU"=sum(PREV_CAREUNIT=="SICU"),
                             "from_CCU"=sum(PREV_CAREUNIT=="CCU")), 
                          by=list(CHART_DATE, CHART_SHIFT,CURR_UNIT)]

# Outflow data ====
outflow_sql <- paste("
                     SELECT ICUSTAY_ID, CHART_DATE, CHART_SHIFT, CURR_UNIT, IFNULL(NEXT_CAREUNIT,'OUT') AS NEXT_CAREUNIT
                     FROM 
                     (   SELECT 
                     ICUSTAY_ID,
                     CURR_CAREUNIT AS CURR_UNIT, 
                     ANY_VALUE(EXTRACT(DATE FROM OUTTIME_TRANS_COLLAPSED)) AS CHART_DATE,",
                     shift_selection_3, " AS CHART_SHIFT
                     FROM `MIMIC3_V1_4.CHARTEVENTS_DEPTS_CATS_TS_COLLAPSED_FINAL` 
                     GROUP BY ICUSTAY_ID, CURR_UNIT) AS L
                     LEFT JOIN 
                     (   SELECT 
                     ICUSTAY_ID,
                     CURR_CAREUNIT AS NEXT_CAREUNIT, 
                     ANY_VALUE(EXTRACT(DATE FROM INTIME_TRANS_COLLAPSED)) AS CHART_DATE,",
                     shift_selection_2, " AS CHART_SHIFT
                     FROM `MIMIC3_V1_4.CHARTEVENTS_DEPTS_CATS_TS_COLLAPSED_FINAL` 
                     GROUP BY ICUSTAY_ID, NEXT_CAREUNIT) AS R
                     USING(ICUSTAY_ID, CHART_DATE, CHART_SHIFT)
                     ORDER BY CHART_DATE, CHART_SHIFT
                     ")
outflow <- dbGetQuery(con, outflow_sql)
outflow <- as.data.table(outflow)
detailed_outflow <- outflow[,.("to_OUT"=sum(NEXT_CAREUNIT=="OUT"), 
                               "to_NWARD"=sum(NEXT_CAREUNIT=="NWARD"),
                               "to_NICU"=sum(NEXT_CAREUNIT=="NICU"),
                               "to_MICU"=sum(NEXT_CAREUNIT=="MICU"),
                               "to_TSICU"=sum(NEXT_CAREUNIT=="TSICU"),
                               "to_CSRU"=sum(NEXT_CAREUNIT=="CSRU"),
                               "to_SICU"=sum(NEXT_CAREUNIT=="SICU"),
                               "to_CCU"=sum(NEXT_CAREUNIT=="CCU")), 
                            by=list(CHART_DATE, CHART_SHIFT,CURR_UNIT)]

# Patient data ====
patients_sql <- paste("
                      SELECT 
                      CHART_DATE, CHART_SHIFT, CURR_UNIT, PATIENTS
                      FROM `bgse-dsc.MIMIC3_V1_4.PATIENTS_DATA` 
                      ORDER BY CHART_DATE, CHART_SHIFT
                      ")
patients <- dbGetQuery(con, patients_sql) 
patients <- as.data.table(patients)

# Merge data ====
flow_data <- frame %>% 
  left_join(y = staff, by=c("CHART_DATE", "CHART_SHIFT", "CURR_UNIT")) %>%
  left_join(y = detailed_inflow, by=c("CHART_DATE", "CHART_SHIFT", "CURR_UNIT")) %>% 
  left_join(y = detailed_outflow, by=c("CHART_DATE", "CHART_SHIFT", "CURR_UNIT")) %>% 
  left_join(y = patients, by=c("CHART_DATE", "CHART_SHIFT", "CURR_UNIT")) %>% 
  as.data.table()

# Cleaning dataset ====
in_cols <- c("from_OUT", "from_NWARD", "from_NICU", "from_MICU", 
             "from_TSICU", "from_CSRU", "from_SICU", "from_CCU")
out_cols <- c("to_OUT", "to_NWARD", "to_NICU", "to_MICU", "to_TSICU",
              "to_CSRU", "to_SICU", "to_CCU")
# staff_cols <- c("STAFF_Nurse", "STAFF_Respiratory", "STAFF_IMD", "STAFF_PCT_NA", 
#                 "STAFF_Resident", "STAFF_Others")
staff_cols <- paste0('STAFF_', c("RN", "IMD", "RT", "MD", "MS", "PCA", "CO", "Unknown"))
relabel_cols <- c(in_cols, out_cols, staff_cols)
flow_data <- as.data.frame(flow_data)
flow_data[, relabel_cols][is.na(flow_data[relabel_cols])] <- 0 
flow_data["INFLOW"] <- rowSums(flow_data[in_cols])
flow_data["OUTFLOW"] <- rowSums(flow_data[out_cols])
flow_data["STAFF_TOTAL"] <- rowSums(flow_data[staff_cols])
flow_data <- as.data.table(flow_data)

# Deleting NICU and NWARD ====
see <- flow_data
flow_data <- flow_data[flow_data$CURR_UNIT != "NICU",]
flow_data %>% select(-contains('NICU')) %>% select(-contains('NWARD')) -> flow_data

# Save clean dataset ====
save(flow_data, file="release/data/clean_dataset_label.RData")

