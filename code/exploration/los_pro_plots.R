# Library ====
library(bigrquery)
library(DBI)
library(data.table)
library(lubridate)
library(ggplot2)
library(dplyr)

# Connection to BigQuery ====
Sys.setenv(BIGQUERY_TEST_PROJECT="bgse-dsc")
billing <- bq_test_project()
con <- dbConnect(
  bigrquery::bigquery(),
  project = "bgse-dsc",
  dataset = "MIMIC3_V1_4",
  billing = billing
)

# Data ====
aggregation <- "month"
sql <- paste("SELECT unit, year,", aggregation,", AVG(LOS_TRANS) AS avg_los,", 
             "COUNT(DISTINCT CGID) AS distinct_staff, AVG(LOS_TRANS) / COUNT(DISTINCT CGID) AS los_pro 
FROM 
(SELECT
    ICUSTAY_ID, INTIME_TRANS_COLLAPSED AS INTIME_TRANS_COLLAPSED, LOS_TRANS, CGID, CURR_CAREUNIT AS UNIT,
    EXTRACT(ISOYEAR FROM INTIME_TRANS_COLLAPSED) AS year,
    EXTRACT(QUARTER FROM INTIME_TRANS_COLLAPSED) AS quarter,
    EXTRACT(MONTH FROM INTIME_TRANS_COLLAPSED) AS month,
    EXTRACT(ISOWEEK FROM INTIME_TRANS_COLLAPSED) AS week,
    EXTRACT(DAYOFWEEK FROM INTIME_TRANS_COLLAPSED) AS day,
    EXTRACT(HOUR FROM INTIME_TRANS_COLLAPSED) AS hour
    FROM `MIMIC3_V1_4.CHARTEVENTS_DEPTS_CATS_TS_COLLAPSED_NEW`)
GROUP BY unit, year,", aggregation,"ORDER BY unit, year,", aggregation,";")
statistics <- dbGetQuery(con, sql)
statistics <- as.data.table(statistics)

# Time Series ====
statistics[, newdate := paste0(year,get(aggregation))]
statistics <- statistics[los_pro < 10,]

ggplot(statistics, aes(newdate, los_pro, group = unit)) + 
    geom_line(color=I("#9ebcda"),size=.4) + 
    facet_grid(unit~.) + 
    labs(x=element_blank(), y=element_blank()) +
    theme(axis.text.x = element_blank(), 
        strip.text.y = element_text(size=10, face="bold"),
        strip.text.x = element_text(size=10, face="bold"),
        strip.background = element_rect(colour="white", fill=I("#9ebcda")),
        panel.background = element_rect(fill = 'white', colour = 'white'))

# Weekday/Hour ====
sql <- paste("SELECT unit, year, month, week, day, hour, AVG(LOS_TRANS) AS avg_los,", 
             "COUNT(DISTINCT CGID) AS distinct_staff, AVG(LOS_TRANS) / COUNT(DISTINCT CGID) AS los_pro 
FROM 
(SELECT
    ICUSTAY_ID, INTIME_TRANS_COLLAPSED AS INTIME_TRANS_COLLAPSED, 
    LOS_TRANS, 
    CGID, 
    CURR_CAREUNIT AS unit,
    EXTRACT(ISOYEAR FROM INTIME_TRANS_COLLAPSED) AS year,
    EXTRACT(QUARTER FROM INTIME_TRANS_COLLAPSED) AS quarter,
    EXTRACT(MONTH FROM INTIME_TRANS_COLLAPSED) AS month,
    EXTRACT(ISOWEEK FROM INTIME_TRANS_COLLAPSED) AS week,
    EXTRACT(DAYOFWEEK FROM INTIME_TRANS_COLLAPSED) AS day,
    EXTRACT(HOUR FROM intime) AS hour
    FROM `MIMIC3_V1_4.CHARTEVENTS_DEPTS_CATS_TS_COLLAPSED_NEW`)",
             "GROUP BY unit, year, month, week, day, hour",
             "ORDER BY unit, year, month, week, day, hour;")
statistics <- dbGetQuery(con, sql)
statistics <- as.data.table(statistics)
#statistics <- statistics[los_pro < 5,]

statistics %>% 
    .[, .(los_pro = median(na.omit(los_pro))), .(day, hour)] %>% 
    ggplot(aes(hour, los_pro)) +
    geom_line(color=I("#9ebcda"),size=.6) + 
    labs(x="Hour of the Day", y="LOS/PRO") +
    theme(strip.text.y = element_text(size=10, face="bold"),
          strip.background = element_rect(colour="white", fill=I("#9ebcda")),
          panel.background = element_rect(fill = 'white', colour = 'white')) +
    facet_grid(day~.)


# InterUnit ====
sql <- paste("SELECT 
ICUSTAY_ID,
CURR_CAREUNIT,
ANY_VALUE(PREV_CAREUNIT) AS PREV_CAREUNIT,
EXTRACT(DATE FROM ANY_VALUE(INTIME_TRANS_COLLAPSED)) AS in_date,
EXTRACT(HOUR FROM ANY_VALUE(INTIME_TRANS_COLLAPSED)) AS in_hour,
COUNT(DISTINCT CGID) AS staff,
ANY_VALUE(LOS_TRANS) AS los,
ANY_VALUE(LOS_TRANS) / COUNT(DISTINCT CGID) AS LOS_PRO
FROM MIMIC3_V1_4.CHARTEVENTS_DEPTS_CATS_TS_COLLAPSED_NEW
GROUP BY ICUSTAY_ID, CURR_CAREUNIT
ORDER BY ICUSTAY_ID, in_date, in_hour
")
transfers <- dbGetQuery(con, sql)
transfers <- as.data.table(transfers)

transfers %>%    
  .[is.na(CURR_CAREUNIT), CURR_CAREUNIT := 'OUT'] %>% 
  .[is.na(PREV_CAREUNIT), PREV_CAREUNIT := 'OUT'] %>% 
  .[(PREV_CAREUNIT != 'OUT') | (CURR_CAREUNIT != 'OUT')] %>% 
  .[!is.na(ICUSTAY_ID)] ->
  transfers_mod

setorder(transfers_mod, ICUSTAY_ID, in_date, in_hour)
transfers_mod %>% 
  .[, NEXT_CAREUNIT := shift(CURR_CAREUNIT, type="lead"), ICUSTAY_ID] %>% 
  .[, NEXT_LOS_PRO := shift(LOS_PRO, type="lead"), ICUSTAY_ID] 


transfers_mod <- transfers_mod[LOS_PRO < 40,]
transfers_mod <- transfers_mod[NEXT_LOS_PRO < 40,]

transfers_mod %>% 
  .[CURR_CAREUNIT != NEXT_CAREUNIT] %>% 
  ggplot(aes(NEXT_LOS_PRO, LOS_PRO)) +
  geom_point(color=I("#9ebcda"),size=2,alpha=.4) +
  labs(y="LOS/PRO - Current Unit", x="LOS/PRO  - Next Unit") +
  theme(strip.text.y = element_text(size=10, face="bold"),
        strip.background = element_rect(colour="white", fill=I("#9ebcda")),
        panel.background = element_rect(fill = 'white', colour = 'white')) +
  facet_grid(NEXT_CAREUNIT~CURR_CAREUNIT) 
 



