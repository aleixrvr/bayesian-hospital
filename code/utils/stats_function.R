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
calc_stats <- function(window_start, window_end, time_shift = "CHARTTIME", 
                       time_aggregation = "TOTAL", unit_aggregation = T){
    # check input
    allowed_shifts <- c("CHARTTIME", "CHARTTIME_SHIFT_1", "CHARTTIME_SHIFT_2", "CHARTTIME_SHIFT_3","CHARTTIME_SHIFT_4", "CHARTTIME_SHIFT_5")
    if (! time_shift %in% allowed_shifts){
        stop("Chart Time must be of the following: CHARTTIME, CHARTTIME_SHIFT_1, CHARTTIME_SHIFT_2, CHARTTIME_SHIFT_3, CHARTTIME_SHIFT_4, CHARTTIME_SHIFT_5")
    }
    allowed_times <- c("TOTAL", "HOUR", "DAYOFYEAR", "DAYOFWEEK", "ISOWEEK", "MONTH", "QUARTER", "ISOYEAR", "DATE")
    if(sum(time_aggregation %in% allowed_times) != length(time_aggregation)){
        stop("Time Aggregation must be of the following: TOTAL, HOUR, DAYOFYEAR, DAYOFWEEK, ISOWEEK, MONTH, QUARTER, ISOYEAR, DATE")
    }
    # aggregation levels
    unit_or_category <- ifelse(unit_aggregation, "CURR_CAREUNIT", "CATEGORY_GROUP")
    if(time_aggregation =="TOTAL"){ 
        time_aggs <- "" 
        times <- ""
    }
    else{
        time_aggs <- paste(",", paste0(time_aggregation, collapse = ", "))
        times <- paste(paste("EXTRACT(",time_aggregation, "FROM", time_shift, ") AS", time_aggregation, collapse = ", "), ",")
    }
    # building the query
    sql <- paste("SELECT", "DISTINCT", unit_or_category, ",", times, "COUNT(DISTINCT ITEMID) AS treatments,", "COUNT(DISTINCT SUBJECT_ID) AS patients,", "COUNT(DISTINCT CGID) AS staff, AVG(LOS) AS avg_los", 
                 "FROM `MIMIC3_V1_4.CHARTEVENTS_DEPTS_CATS_TS`", 
                 "WHERE DATE(", time_shift, ") >=", paste0("'", window_start, "'"), "AND DATE(", time_shift, ") <=", paste0("'" , window_end, "'"),
                 "GROUP BY ", unit_or_category, time_aggs,
                 "ORDER BY ", unit_or_category, time_aggs, ";")
    # execute the query and return results
    statistics <- dbGetQuery(con, sql)
    return(as.data.table(statistics))
}

# Check Function ====
#check_intervals <- c("TOTAL", "HOUR", "DAYOFYEAR", "DAYOFWEEK", "ISOWEEK", "MONTH", "QUARTER", "ISOYEAR", "OTHER")
#for(i in check_intervals) print(calc_stats(window_start="2100-03-01",window_end="2108-03-30", time_aggregation = i, unit_aggr = F))



