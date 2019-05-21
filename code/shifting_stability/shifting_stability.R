# METADATA ====
# Description: Time shifting stability 
# Created: 2019-05-15 (Reid Falconer)
# Updated: 2019-05-20 (Reid Falconer)
# Reviewed: 

# SUMMARY: Since data has been shifted, the objective of this task is make
# more shiftings in order to see which distributional features remain invariant.

# With this idea in mind, we should take 5-10 subsamples and apply a shifting 
# (the closest to the one used in MIMIC, check out MIMICs documentation)
# to each one of them. Then uploading it to Bigquery. 
# The tables should have the same format as Chartevents.

# INITIALISE ====
# rm(list=ls())

#> Libraries ----
library(bigrquery)
library(DBI)
library(data.table)
library(tidyverse) #load tidyverse last to avoid namespace conflicts
library(lubridate)

#> Set options ----

# disable scientific notation
options(scipen = 999)

# FUNCTIONS ====

# source the function from `stats_function.R` script in order to test the time 
# shifting stability 
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

sql <- paste0("SELECT DISTINCT(ICUSTAY_ID) AS ICUSTAY_ID ",
"FROM MIMIC3_V1_4.CHARTEVENTS_DEPTS_CATS AS CHART")

time_shifts <- dbGetQuery(con, sql)

input = 1000:3000
multiple_of_7 = (input %% 7) == 0
input = input[multiple_of_7]

time_shifts <- time_shifts %>% 
  mutate(SHIFT_1 = sample(input, size = nrow(time_shifts), replace = TRUE),
         SHIFT_2 = sample(input, size = nrow(time_shifts), replace = TRUE),
         SHIFT_3 = sample(input, size = nrow(time_shifts), replace = TRUE),
         SHIFT_4 = sample(input, size = nrow(time_shifts), replace = TRUE),
         SHIFT_5 = sample(input, size = nrow(time_shifts), replace = TRUE))

time_shifts

# write table to BigQuery
#dbWriteTable(con, name = "TIME_SHIFT", time_shifts, row.names = TRUE, overwrite = TRUE)

# TIME SHIFT MIN AND MAX ====
sql_max <- paste0("SELECT MAX(CHARTTIME) AS MAX_CHARTTIME, ",
                  "MAX(CHARTTIME_SHIFT_1) AS MAX_CHARTTIME_SHIFT_1, ",
                  "MAX(CHARTTIME_SHIFT_2) AS MAX_CHARTTIME_SHIFT_2, ",
                  "MAX(CHARTTIME_SHIFT_3) AS MAX_CHARTTIME_SHIFT_3, ",
                  "MAX(CHARTTIME_SHIFT_4) AS MAX_CHARTTIME_SHIFT_4, ",
                  "MAX(CHARTTIME_SHIFT_5) AS MAX_CHARTTIME_SHIFT_5 ",
                  "FROM MIMIC3_V1_4.CHARTEVENTS_DEPTS_CATS_TS")
time_shifts_max <- dbGetQuery(con, sql_max)

sql_min <- paste0("SELECT MIN(CHARTTIME) AS MIN_CHARTTIME, ",
                  "MIN(CHARTTIME_SHIFT_1) AS MIN_CHARTTIME_SHIFT_1, ",
                  "MIN(CHARTTIME_SHIFT_2) AS MIN_CHARTTIME_SHIFT_2, ",
                  "MIN(CHARTTIME_SHIFT_3) AS MIN_CHARTTIME_SHIFT_3, ",
                  "MIN(CHARTTIME_SHIFT_4) AS MIN_CHARTTIME_SHIFT_4, ",
                  "MIN(CHARTTIME_SHIFT_5) AS MIN_CHARTTIME_SHIFT_5 ",
                  "FROM MIMIC3_V1_4.CHARTEVENTS_DEPTS_CATS_TS")
time_shifts_min <- dbGetQuery(con, sql_min)

# Check Shifts 

sql_ts_check <- paste0("SELECT CHARTTIME, CHARTTIME_SHIFT_1,  CHARTTIME_SHIFT_2,
                    CHARTTIME_SHIFT_3, CHARTTIME_SHIFT_4, CHARTTIME_SHIFT_5 ",
"FROM `MIMIC3_V1_4.CHARTEVENTS_DEPTS_CATS_TS` ",
"WHERE ICUSTAY_ID = 209603")
ts_check <- dbGetQuery(con, sql_ts_check)

CHARTTIME <- wday(lubridate::as_datetime(ts_check[[1]])) 
CHARTTIME_SHIFT_1 <- wday(lubridate::as_datetime(ts_check[[2]]))
CHARTTIME_SHIFT_2<- wday(lubridate::as_datetime(ts_check[[3]]))
CHARTTIME_SHIFT_3<- wday(lubridate::as_datetime(ts_check[[4]]))
CHARTTIME_SHIFT_4 <- wday(lubridate::as_datetime(ts_check[[5]]))
CHARTTIME_SHIFT_5 <- wday(lubridate::as_datetime(ts_check[[6]]))

identical(CHARTTIME, CHARTTIME_SHIFT_1, CHARTTIME_SHIFT_2, CHARTTIME_SHIFT_3, CHARTTIME_SHIFT_4, CHARTTIME_SHIFT_5)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 