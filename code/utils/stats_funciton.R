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
calc_stats <- function(window_start, window_end, time_aggr = "TOTAL", unit_aggr = T){
    # aggregation for treatment level is still open for implementation
    if(! time_aggr %in% c("TOTAL", "HOUR", "DAYOFYEAR", "DAYOFWEEK", "ISOWEEK", "MONTH", 
                          "QUARTER", "ISOYEAR")){
        stop("Time Aggregation must be of the following: 
             TOTAL, HOUR, DAYOFYEAR, DAYOFWEEK, ISOWEEK, MONTH, QUARTER, ISOYEAR")
    }
    if(time_aggr == "TOTAL"){
        sql <- paste0("SELECT ", 
                      "DISTINCT CURR_CAREUNIT, ", 
                      "COUNT(DISTINCT ITEMID) AS treatments, COUNT(DISTINCT SUBJECT_ID) AS patients, ", 
                      "COUNT(DISTINCT CGID) AS staff, AVG(LOS) AS avg_los ", 
                      "FROM `MIMIC3_V1_4.CHARTEVENTS_DEPTS_CATS` ", 
                      "WHERE DATE(CHARTTIME) >= '", window_start, "' ",
                      "AND DATE(CHARTTIME) <= '" , window_end, "' ",
                      "GROUP BY CURR_CAREUNIT ",
                      "ORDER BY CURR_CAREUNIT", ";")
    }
    else {
        sql <- paste0("SELECT ", 
                      "DISTINCT CURR_CAREUNIT, ", 
                      "EXTRACT(",time_aggr, " FROM CHARTTIME) AS ", time_aggr,", ",
                      "COUNT(DISTINCT ITEMID) AS treatments, COUNT(DISTINCT SUBJECT_ID) AS patients, ", 
                      "COUNT(DISTINCT CGID) AS staff, AVG(LOS) AS avg_los ", 
                      "FROM `MIMIC3_V1_4.CHARTEVENTS_DEPTS_CATS` ", 
                      "WHERE DATE(CHARTTIME) >= '", window_start, "' ",
                      "AND DATE(CHARTTIME) <= '" , window_end, "' ",
                      "GROUP BY CURR_CAREUNIT, ", time_aggr, " ",
                      "ORDER BY CURR_CAREUNIT, ", time_aggr, ";")
    }
    statistics <- dbGetQuery(con, sql)
    as.data.table(statistics)
}

# Check Function ====
check_intervals <- c("TOTAL", "HOUR", "DAYOFYEAR", "DAYOFWEEK", "ISOWEEK", "MONTH", 
                     "QUARTER", "ISOYEAR", "OTHER")
for(i in check_intervals){
    print(calc_stats(window_start="2100-03-01",window_end="2108-03-30", time_aggr = i))
}



