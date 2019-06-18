# METADATA ====
# Description: Time collapsing 
# Created: 2019-05-27 (Reid Falconer)
# Updated: 2019-06-03 (Reid Falconer)
# Reviewed: 

# SUMMARY: 
# INITIALISE ====
#rm(list=ls())

#> Libraries ----
library(bigrquery)
library(DBI)
library(data.table)
library(hms)
library(lubridate)
library(tidyverse)

#> Set options ----

# disable scientific notation
options(scipen = 999)

# FUNCTIONS ====

# source the function from `stats_function.R` script in order to test the time 
source("code/utils/stats_function.R") 

# CONNECT TO BigQuery ====

Sys.setenv(BIGQUERY_TEST_PROJECT="bgse-dsc")
billing <- bq_test_project()
con <- dbConnect(
  bigrquery::bigquery(),
  project = "bgse-dsc",
  dataset = "MIMIC3_V1_4",
  billing = billing
)
dbListTables(con)

# If the date shift filter is on, deid replaces all dates in its input with 
# surrogate dates. The shift.txt file contains a 
# randomly assigned date shift (between 1000 and 3000 days) 

# SQL QUERY ====

# see `shifting_stability.sql`

# TIME SHIFT GENERATION ====

sql <- paste0("
SELECT
  *
  FROM (
    SELECT
    *
  FROM
    `MIMIC3_V1_4.ICUSTAYS`) AS ICUSTAYS
LEFT JOIN (
  SELECT
    ICUSTAY_ID,
    INTIME AS INTIME_TRANS,
    OUTTIME AS OUTTIME_TRANS,
    LOS AS LOS_TRANS
  FROM
    `MIMIC3_V1_4.TRANSFERS`) AS TRANS
USING
  (ICUSTAY_ID)
WHERE
ICUSTAYS.INTIME <= TRANS.INTIME_TRANS
AND TRANS.OUTTIME_TRANS <= ICUSTAYS.OUTTIME
              ")

time_collapse <- dbGetQuery(con, sql)
glimpse(time_collapse)

collapse_times <- function(source = "metavision") {
  input = 1000:3000
  multiple_of_7 = (input %% 7) == 0
  input = input[multiple_of_7]
  df <- time_collapse
  set.seed(2019)
  df <- time_collapse %>% 
    mutate(SHIFT_1 = sample(input, size = nrow(df), replace = TRUE),
           SHIFT_2 = sample(input, size = nrow(df), replace = TRUE),
           SHIFT_3 = sample(input, size = nrow(df), replace = TRUE),
           SHIFT_4 = sample(input, size = nrow(df), replace = TRUE),
           SHIFT_5 = sample(input, size = nrow(df), replace = TRUE),
           INTIME_SHIFT_1 = INTIME + days(SHIFT_1),
           INTIME_SHIFT_2 = INTIME + days(SHIFT_2),
           INTIME_SHIFT_3 = INTIME + days(SHIFT_3),
           INTIME_SHIFT_4 = INTIME + days(SHIFT_4),
           INTIME_SHIFT_5 = INTIME + days(SHIFT_5),
           INTIME_TRANS_SHIFT_1 = INTIME_TRANS + days(SHIFT_1),
           INTIME_TRANS_SHIFT_2 = INTIME_TRANS + days(SHIFT_2),
           INTIME_TRANS_SHIFT_3 = INTIME_TRANS + days(SHIFT_3),
           INTIME_TRANS_SHIFT_4 = INTIME_TRANS + days(SHIFT_4),
           INTIME_TRANS_SHIFT_5 = INTIME_TRANS + days(SHIFT_5))
  if(source == "carevue") {
    years <- 7
    start_date = "2001-01-01"
    df <- df %>% 
      filter(DBSOURCE == source)
  } else if ( source == "metavision") {
    years <- 5
    start_date = "2008-01-01"
    df <- df %>% 
      filter(DBSOURCE == source  | DBSOURCE == "both")
  } else {
    years <- 12
    start_date = "2001-01-01"
  }
  df_collapsed <- df
  df_time <-df %>% 
    select(INTIME, INTIME_SHIFT_1, INTIME_SHIFT_2, INTIME_SHIFT_3, INTIME_SHIFT_4, INTIME_SHIFT_5,
           INTIME_TRANS, INTIME_TRANS_SHIFT_1, INTIME_TRANS_SHIFT_2, INTIME_TRANS_SHIFT_3, 
           INTIME_TRANS_SHIFT_4, INTIME_TRANS_SHIFT_5)
  for (time in colnames(df_time)) {
  INTIME_a <- df[[time]] - min(df[[time]])
  INTIME_a <- lubridate::as.period(INTIME_a, unit = "days")
  INTIME_b <- max(df[[time]]) - min(df[[time]])
  INTIME_b <- lubridate::as.period(INTIME_b, unit = "days")
  INTIME_new <- (INTIME_a/INTIME_b)*years*364
  
  INTIME_dates <- lubridate::as_date(start_date) + INTIME_new
  INTIME_COLLAPSED_date <- lubridate::floor_date(INTIME_dates, 
                                                 unit = "weeks") + (lubridate::wday(df[[time]]) - 1)
  time_of_day <- hms::hms(lubridate::second(df[[time]]),
                     lubridate::minute(df[[time]]),
                     lubridate::hour(df[[time]]))
  INTIME_COLLAPSED_date <- ymd_hms(INTIME_COLLAPSED_date, truncated = 3, tz = "Etc/GMT+5") + lubridate::hms(time_of_day)
  print(identical(lubridate::wday(INTIME_COLLAPSED_date), lubridate::wday(df$INTIME)))
  print(identical(lubridate::wday(INTIME_COLLAPSED_date), lubridate::wday(df$INTIME_TRANS)))
  intime_var_name <- paste0(time,"_COLLAPSED")
  outtime_var_name <- paste0("OUTTIME",substring(time, 7),"_COLLAPSED")
  df_collapsed[[intime_var_name]] <- with(df_collapsed, INTIME_COLLAPSED_date)
  df_collapsed[[outtime_var_name]] <- with(df_collapsed, INTIME_COLLAPSED_date + lubridate::as.difftime(df$LOS_TRANS, units = "days"))
  }
  return(df_collapsed)
}
  
carvue <- collapse_times(source = "carevue")
metavision <- collapse_times(source = "metavision")
#all <- collapse_times(source = "all")
# nrow(carvue) + nrow(metavision)
# nrow(all)

df_collapsed <- rbind(carvue, metavision) %>% 
  select(-c(starts_with("OUTTIME_SHIFT")))
 

glimpse(df_collapsed)

# write table to BigQuery
dbWriteTable(con, name = "ICUSTAYS_TRANS_COLLAPSED", df_collapsed, row.names = TRUE, overwrite = TRUE)
