library("bigrquery")
library(DBI)
library(dplyr)
library(data.table)
library(ggplot2)
library(magrittr)

Sys.setenv(BIGQUERY_TEST_PROJECT="bgse-dsc")
billing <- bq_test_project()

con <- dbConnect(
  bigrquery::bigquery(),
  project = "bgse-dsc",
  dataset = "MIMIC3_V1_4",
  billing = billing
)

items <- as.data.table(tbl(con, "D_ITEMS"))
items[, unique(CATEGORY)]
fwrite(items[, .(unique(CATEGORY), unique(CATEGORY))], 
       'code/exploration/categories.csv',
       col.names = FALSE)