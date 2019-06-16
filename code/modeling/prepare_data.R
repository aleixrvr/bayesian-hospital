library("bigrquery")
library(DBI)
library(dplyr)
library(data.table)
library(ggplot2)
library(magrittr)
library(jsonlite)


Sys.setenv(BIGQUERY_TEST_PROJECT="bgse-dsc")
billing <- bq_test_project()

con <- dbConnect(
  bigrquery::bigquery(),
  project = "bgse-dsc",
  dataset = "MIMIC3_V1_4",
  billing = billing
)


readLines('code/modeling/directions.json') %>% 
  .[1] %>% 
  fromJSON() ->
  directions


inflow_sql <- 'SELECT
  COUNT(DISTINCT SUBJECT_ID) as in_patients,
  date, 
  hour,
  CURR_CAREUNIT as careunit
FROM(
  SELECT 
    SUBJECT_ID,
    CURR_CAREUNIT, 
    DATE(INTIME_SHIFT_1_COLLAPSED) as date,
    EXTRACT(HOUR FROM INTIME_SHIFT_1_COLLAPSED) as hour
  FROM `MIMIC3_V1_4.CHARTEVENTS_DEPTS_CATS_TS_COLLAPSED_NEW` )
GROUP BY date, hour, CURR_CAREUNIT'


outflow_sql <- 'SELECT
  COUNT(DISTINCT SUBJECT_ID) as out_patients,
  date, 
  hour,  
  CURR_CAREUNIT as careunit
FROM(
  SELECT 
    SUBJECT_ID,
    CURR_CAREUNIT, 
    DATE(OUTTIME_TRANS_SHIFT_1_COLLAPSED) as date,
    EXTRACT(HOUR FROM OUTTIME_TRANS_SHIFT_1_COLLAPSED) as hour
  FROM `MIMIC3_V1_4.CHARTEVENTS_DEPTS_CATS_TS_COLLAPSED_NEW` )
GROUP BY date, hour, CURR_CAREUNIT'


caregivers_sql <- 'SELECT
  COUNT(DISTINCT CGID) as caregivers,
  date, 
  hour,
  CURR_CAREUNIT as careunit
FROM(
  SELECT 
    CGID,
    CURR_CAREUNIT, 
    DATE(CHARTTIME_COLLAPSED_SHIFT_1) as date,
    EXTRACT(HOUR FROM CHARTTIME_COLLAPSED_SHIFT_1) as hour
  FROM `MIMIC3_V1_4.CHARTEVENTS_DEPTS_CATS_TS_COLLAPSED_NEW` )
GROUP BY date, hour, CURR_CAREUNIT'


inflow <- dbGetQuery(con, inflow_sql)
outflow <- dbGetQuery(con, outflow_sql)
caregivers <- dbGetQuery(con, caregivers_sql)



