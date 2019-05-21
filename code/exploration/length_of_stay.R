# METADATA ====
# Description: Exploring length of stay in the ICU
# Created: 2019-05-21 (Reid Falconer)
# Updated: 2019-05-21 (Reid Falconer)
# Reviewed: 

# SUMMARY: Exploring length of stay in the ICU

# INITIALISE ====
# rm(list=ls())

#> Libraries ----
library(bigrquery)
library(DBI)
library(data.table)
library(tidyverse) #load tidyverse last to avoid namespace conflicts
library(lubridate)

# CONNECT TO BigQuery ====

# create a connection to the  database
Sys.setenv(BIGQUERY_TEST_PROJECT="bgse-dsc")
billing <- bq_test_project()
con <- dbConnect(
  bigrquery::bigquery(),
  project = "bgse-dsc",
  dataset = "MIMIC3_V1_4",
  billing = billing
)
dbListTables(con)


sql_los <- paste0("SELECT LOS FROM MIMIC3_V1_4.ICUSTAYS WHERE LOS >= 0 AND LOS <=20")

# assign ICU length of stay to variable
iculos = dbGetQuery(con, sql_los)

# plot histogram of length of stay in the ICU
qplot(iculos$LOS, geom="histogram", binwidth = 1, main = "Length of stay in the ICU", 
      xlab = "Length of stay, days", fill=I("#9ebcda"), col=I("#FFFFFF"))
