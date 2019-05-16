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

transfers <- as.data.table(tbl(con, "TRANSFERS"))
transfers[is.na(CURR_CAREUNIT), CURR_CAREUNIT := 'OUT']
transfers[is.na(PREV_CAREUNIT), PREV_CAREUNIT := 'OUT']




transfers[, .(n = .N), .(PREV_CAREUNIT, CURR_CAREUNIT)] %>% 
  .[, prop := n/sum(n), PREV_CAREUNIT] %>% 
  .[(PREV_CAREUNIT != 'OUT') | (CURR_CAREUNIT != 'OUT')] %>% 
  .[PREV_CAREUNIT != CURR_CAREUNIT] ->
  trans_summary

transfers[, .N, PREV_CAREUNIT] %T>% 
  setorder(N) %>% 
  .[, PREV_CAREUNIT] ->
  careunits

trans_summary[, PREV_CAREUNIT := factor(PREV_CAREUNIT, levels = careunits)]
trans_summary[, CURR_CAREUNIT := factor(CURR_CAREUNIT, levels = careunits)]

trans_summary %>% 
  ggplot(aes(PREV_CAREUNIT, CURR_CAREUNIT)) +
    geom_tile(aes(fill = prop), color = "white")
