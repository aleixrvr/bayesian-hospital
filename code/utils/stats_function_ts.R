# Library ====
library(bigrquery)
library(DBI)
library(data.table)

# Connection to BigQuery ====
Sys.setenv(BIGQUERY_TEST_PROJECT="bgse-dsc")
billing <- bq_test_project()
con <- dbConnect(
    bigrquery::bigquery(),
    project = "bgse-dsc",
    dataset = "MIMIC3_V1_4",
    billing = billing
)
dbListTables(con)

# Function ====
calc_stats <- function(window_start, window_end, chart_time = "CHARTTIME", time_aggr_1 = "TOTAL", time_aggr_2 = "NONE", unit_aggr = T){
    if(unit_aggr){
        uorcg <- "CURR_CAREUNIT"
    }
    else{
        uorcg <- "CATEGORY_GROUP"
    }
    if (! chart_time %in% c("CHARTTIME", "CHARTTIME_SHIFT_1", "CHARTTIME_SHIFT_2", "CHARTTIME_SHIFT_3",
                            "CHARTTIME_SHIFT_4", "CHARTTIME_SHIFT_5")){
        stop("Chart Time must be of the following: 
             CHARTTIME, CHARTTIME_SHIFT_1, CHARTTIME_SHIFT_2, 
             CHARTTIME_SHIFT_3, CHARTTIME_SHIFT_4, CHARTTIME_SHIFT_5")
    }
    if(! time_aggr_1 %in% c("TOTAL", "HOUR", "DAYOFYEAR", "DAYOFWEEK", "ISOWEEK", "MONTH", 
                          "QUARTER", "ISOYEAR")){
        stop("Time Aggregation must be of the following: 
             TOTAL, HOUR, DAYOFYEAR, DAYOFWEEK, ISOWEEK, MONTH, QUARTER, ISOYEAR")
    }
    if(! time_aggr_2 %in% c("TOTAL", "HOUR", "DAYOFYEAR", "DAYOFWEEK", "ISOWEEK", "MONTH", 
                            "QUARTER", "ISOYEAR", "NONE")){
        stop("Time Aggregation must be of the following: 
             TOTAL, HOUR, DAYOFYEAR, DAYOFWEEK, ISOWEEK, MONTH, QUARTER, ISOYEAR, NONE")
    }
    if(time_aggr_2 != "NONE"){
        time_aggr <- paste0(time_aggr_1,", " , time_aggr_2)
        time_features <- paste0("EXTRACT(",time_aggr_1, " FROM ",chart_time,") AS ", time_aggr_1,", ", 
                                "EXTRACT(",time_aggr_2, " FROM ",chart_time,") AS ", time_aggr_2,", ")
    }
    else{
        time_aggr <- time_aggr_1
        time_features <- paste0("EXTRACT(",time_aggr, " FROM ",chart_time,") AS ", time_aggr,", ")
    }
    if(time_aggr == "TOTAL"){
        sql <- paste0("SELECT ", 
                      "DISTINCT ", uorcg, ", ", 
                      "COUNT(DISTINCT ITEMID) AS treatments, COUNT(DISTINCT SUBJECT_ID) AS patients, ", 
                      "COUNT(DISTINCT CGID) AS staff, AVG(LOS) AS avg_los ", 
                      "FROM `MIMIC3_V1_4.CHARTEVENTS_DEPTS_CATS_TS` ", 
                      "WHERE DATE(",chart_time,") >= '", window_start, "' ",
                      "AND DATE(",chart_time,") <= '" , window_end, "' ",
                      "GROUP BY ", uorcg, " ",
                      "ORDER BY ", uorcg, " ", ";")
    }
    else {
        sql <- paste0("SELECT ", 
                      "DISTINCT ", uorcg, ", ", 
                      time_features,
                      "COUNT(DISTINCT ITEMID) AS treatments, COUNT(DISTINCT SUBJECT_ID) AS patients, ", 
                      "COUNT(DISTINCT CGID) AS staff, AVG(LOS) AS avg_los ", 
                      "FROM `MIMIC3_V1_4.CHARTEVENTS_DEPTS_CATS_TS` ", 
                      "WHERE DATE(",chart_time,") >= '", window_start, "' ",
                      "AND DATE(",chart_time,") <= '" , window_end, "' ",
                      "GROUP BY ", uorcg, ", ", time_aggr, " ",
                      "ORDER BY ", uorcg, ", ", time_aggr, ";")
    }
    statistics <- dbGetQuery(con, sql)
    as.data.table(statistics)
}

# Check Function ====

#check_intervals <- c("TOTAL", "HOUR", "DAYOFYEAR", "DAYOFWEEK", "ISOWEEK", "MONTH", "QUARTER", "ISOYEAR", "OTHER")
#for(i in check_intervals) print(calc_stats(window_start="2100-03-01",window_end="2108-03-30", time_aggr_1 = i, unit_aggr = F))

#time_shifts <- c("CHARTTIME", "CHARTTIME_SHIFT_1", "CHARTTIME_SHIFT_2", "CHARTTIME_SHIFT_3", "CHARTTIME_SHIFT_4", "CHARTTIME_SHIFT_5")
#for(i in time_shifts) print(calc_stats(window_start="2100-03-01",window_end="2108-03-30", time_aggr_1 = "TOTAL", chart_time = i, unit_aggr = F))
