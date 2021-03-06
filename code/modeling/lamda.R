# Library ====
library(bigrquery)
library(DBI)
library(data.table)
library(lubridate)
library(ggplot2)
library(dplyr)
library(lubridate)

# Connection to BigQuery ====
Sys.setenv(BIGQUERY_TEST_PROJECT="bgse-dsc")
billing <- bq_test_project()
con <- dbConnect(
  bigrquery::bigquery(),
  project = "bgse-dsc",
  dataset = "MIMIC3_V1_4",
  billing = billing
)
SKELETON_SQL <- paste("
SELECT *
FROM `bgse-dsc.MIMIC3_V1_4.LAMBDA_DATA_4`
ORDER BY ICUSTAY_ID, CURR_UNIT, DATE_HOUR
")
# This query take appoximatly 30minutes so it is reccomended that you read in the RDS file
lambda_df <- dbGetQuery(con, SKELETON_SQL) %>%
  as.data.table()

# Save an object to a file
#saveRDS(lambda_df, file = "code/modeling/lambda_data.RDS")

# Read in data
lambda_df <- readRDS("code/modeling/lambda_data.RDS")

# create censoring tags
lambda_df <- lambda_df[, left_censored := ifelse(INTIME_TRANS_COLLAPSED < DATE_HOUR &
                                                   OUTTIME_TRANS_COLLAPSED <= DATE_HOUR_PLUS, 1, 0)]
lambda_df <- lambda_df[, right_censored := ifelse(INTIME_TRANS_COLLAPSED >= DATE_HOUR &
                                                    INTIME_TRANS_COLLAPSED <= DATE_HOUR_PLUS &
                                                    OUTTIME_TRANS_COLLAPSED > DATE_HOUR_PLUS, 1, 0)]
lambda_df <- lambda_df[, full_censored := ifelse(INTIME_TRANS_COLLAPSED < DATE_HOUR &
                                                   OUTTIME_TRANS_COLLAPSED > DATE_HOUR_PLUS, 1, 0)]
lambda_df <- lambda_df[, not_censored := ifelse(INTIME_TRANS_COLLAPSED >= DATE_HOUR &
                                                  OUTTIME_TRANS_COLLAPSED <= DATE_HOUR_PLUS, 1, 0)]
# create censored times based on window
lambda_df <- lambda_df[, time := (OUTTIME_TRANS_COLLAPSED - DATE_HOUR)*(left_censored == 1 & right_censored == 0 & full_censored == 0) +
                         (DATE_HOUR_PLUS - INTIME_TRANS_COLLAPSED)*(right_censored == 1 & left_censored == 0  & full_censored == 0) +
                         (DATE_HOUR_PLUS - DATE_HOUR)*(full_censored == 1 & right_censored == 0 & left_censored == 0 ) +
                         (OUTTIME_TRANS_COLLAPSED - INTIME_TRANS_COLLAPSED)*(right_censored == 0 & left_censored == 0  & full_censored == 0)
                       ]

data <- lambda_df
# simple lambda function 
lambda <- function(data) {
  time <- as.vector((((data$time)/3600)/4))
  not_censored <- as.vector(data$not_censored)
#  icustay_id <- as.vector(data$ICUSTAY_ID)
  n = length(not_censored)
  r = sum(not_censored)
  t = sum(time)
  lamda = round(r/t, 5)
  return(list(lamda, n))
}

lambda_data <- lambda_df[, by = .(DATE_HOUR, CURR_UNIT, CHART_SHIFT), 
                         lambda(.SD), .SDcols=c("not_censored", "time"),] %>% 
  rename(LAMBDA = V1,
         PATIENTS = V2) %>% 
  mutate(CHART_DATE = as.Date(DATE_HOUR),
         CHART_HOUR = hour(DATE_HOUR),
         SHIFT = (CHART_HOUR+4)/4)

lambda_data <- lambda_data %>% 
  select(CURR_UNIT, CHART_SHIFT, PATIENTS, CHART_DATE) %>% 
  as.data.table()
# Save data to a file
#saveRDS(lambda_df, file = "code/modeling/lambda_data_final.RDS")

# write table to BigQuery
dbWriteTable(con, name = "PATIENTS_DATA", lambda_data, row.names = TRUE, overwrite = TRUE)

