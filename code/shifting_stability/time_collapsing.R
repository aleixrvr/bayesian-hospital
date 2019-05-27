# METADATA ====
# Description: Time collapsing 
# Created: 2019-05-27 (Reid Falconer)
# Updated: 2019-05-27 (Reid Falconer)
# Reviewed: 

# SUMMARY: 
# INITIALISE ====
 rm(list=ls())

#> Libraries ----
library(bigrquery)
library(DBI)
library(data.table)
library(tidyverse)
library(hms)
library(lubridate)

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

sql <- paste0("SELECT * ",
              "FROM MIMIC3_V1_4.ICUSTAYS AS ICUSTAYS")
time_collapse <- dbGetQuery(con, sql)

INTIME_a <- time_collapse$INTIME - min(time_collapse$INTIME)
INTIME_a <- lubridate::as.period(INTIME_a, unit = "days")
INTIME_b <- max(time_collapse$INTIME) - min(time_collapse$INTIME)
INTIME_b <- lubridate::as.period(INTIME_b, unit = "days")
INTIME_new <- (INTIME_a/INTIME_b)*12*364

INTIME_dates <- lubridate::as_date("2001-01-01") + INTIME_new
INTIME_collapsed_date <- lubridate::floor_date(INTIME_dates, unit = "weeks") + (lubridate::wday(time_collapse$INTIME) - 1)

time_of_day <- hms::hms(lubridate::second(time_collapse$INTIME),
                     lubridate::minute(time_collapse$INTIME),
                     lubridate::hour(time_collapse$INTIME))

INTIME_collapsed_date <- ymd_hms(INTIME_collapsed_date, truncated = 3) + lubridate::hms(time_of_day)

identical(lubridate::wday(INTIME_collapsed_date), lubridate::wday(time_collapse$INTIME))

time_collapse <- time_collapse %>% 
  mutate(INTIME_COLLAPSED = INTIME_collapsed_date,
         OUTIME_COLLAPSED  = INTIME_COLLAPSED + lubridate::as.difftime(time_collapse$LOS, units = "days"))

#glimpse(time_collapse)

# write table to BigQuery
dbWriteTable(con, name = "ICUSTAYS_COLLAPSED", time_collapse, row.names = TRUE, overwrite = TRUE)
