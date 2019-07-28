library("bigrquery")
library(DBI)
library(dplyr)
library(data.table)
 
Sys.setenv(BIGQUERY_TEST_PROJECT="bgse-dsc")
billing <- bq_test_project()

con <- dbConnect(
  bigrquery::bigquery(),
  project = "bgse-dsc",
  dataset = "MIMIC3_V1_4",
  billing = billing
)

dbListTables(con)

admissions <- tbl(con, "ADMISSIONS")
admissions <- as.data.table(admissions)


sql <- "select SUBJECT_ID
from `MIMIC3_V1_4.ADMISSIONS` 
limit 100"
res <- dbGetQuery(con, sql, n = 10)

