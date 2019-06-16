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

transfers <- as.data.table(tbl(con, "TRANSFERS"))
transfers[is.na(CURR_CAREUNIT), CURR_CAREUNIT := 'OUT']
transfers[is.na(PREV_CAREUNIT), PREV_CAREUNIT := 'OUT']


transfers[, .(n = .N), .(PREV_CAREUNIT, CURR_CAREUNIT)] %>% 
  .[(CURR_CAREUNIT != 'OUT') & (PREV_CAREUNIT != 'OUT')] %>% 
  .[, prop := n/sum(n), PREV_CAREUNIT] %>% 
  .[PREV_CAREUNIT != CURR_CAREUNIT] ->
  trans_summary

transfers[, .N, PREV_CAREUNIT] %T>%
  setorder(N) %>%
  .[, PREV_CAREUNIT] ->
  careunits

careunits <- setdiff(careunits, 'OUT')

# 
# trans_summary[, PREV_CAREUNIT := factor(PREV_CAREUNIT, levels = careunits)]
# trans_summary[, CURR_CAREUNIT := factor(CURR_CAREUNIT, levels = careunits)]
# 
# trans_summary %>% 
#   .[ prop > prop_threshold ] %>%
#   ggplot(aes(PREV_CAREUNIT, CURR_CAREUNIT)) +
#   geom_tile(aes(fill = prop), color = "white") +
#   scale_fill_gradient(low = "#e3ebed",
#                       high = "#8aa6ad", 
#                       name="PROBABILITY") +
#   xlab("PREVIOUS CARE UNIT") +
#   ylab("CURRENT CARE UNIT") +
#   scale_x_discrete(limits = rev(levels(trans_summary$PREV_CAREUNIT))) +
#   coord_flip() +
#   theme_light() +
#   theme(legend.position = "right", legend.direction = "vertical",
#         panel.grid.major = element_blank(),
#         panel.border = element_blank())
# 

directions <- list()
for( careunit in careunits ){
  directions[[careunit]] <- list()
  directions[[careunit]]$to <- trans_summary[PREV_CAREUNIT == careunit, as.vector(CURR_CAREUNIT)]
  directions[[careunit]]$from <- trans_summary[CURR_CAREUNIT == careunit, as.vector(PREV_CAREUNIT)]
}

write(toJSON(directions), 'code/modeling/directions.json')



