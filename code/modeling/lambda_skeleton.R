total_hours <- 24
splits <- 4
shift_selection <- "CASE \n"
pos = 1
for( time in seq(splits, total_hours, splits)){
  shift_selection <- paste0(shift_selection, "WHEN EXTRACT(HOUR FROM CHARTTIME_COLLAPSED) < ")
  shift_selection <- paste0(shift_selection, time)
  shift_selection <- paste0(shift_selection, ' THEN ')
  shift_selection <- paste0(shift_selection, pos)
  shift_selection <- paste0(shift_selection, '\n')
  pos = pos + 1
  
}
shift_selection <- paste0(shift_selection, 'END')
shift_selection_3 <- "CASE \n"
pos = 1
for( time in seq(splits, total_hours, splits)){
  shift_selection_3 <- paste0(shift_selection_3, "WHEN EXTRACT(HOUR FROM ANY_VALUE(OUTTIME_TRANS_COLLAPSED)) < ")
  shift_selection_3 <- paste0(shift_selection_3, time)
  shift_selection_3 <- paste0(shift_selection_3, ' THEN ')
  shift_selection_3 <- paste0(shift_selection_3, pos)
  shift_selection_3 <- paste0(shift_selection_3, '\n')
  pos = pos + 1
  
}
shift_selection_3 <- paste0(shift_selection_3, 'END')
shift_selection_2 <- "CASE \n"
pos = 1
for( time in seq(splits, total_hours, splits)){
  shift_selection_2 <- paste0(shift_selection_2, "WHEN EXTRACT(HOUR FROM ANY_VALUE(INTIME_TRANS_COLLAPSED)) < ")
  shift_selection_2 <- paste0(shift_selection_2, time)
  shift_selection_2 <- paste0(shift_selection_2, ' THEN ')
  shift_selection_2 <- paste0(shift_selection_2, pos)
  shift_selection_2 <- paste0(shift_selection_2, '\n')
  pos = pos + 1
  
}
shift_selection_2 <- paste0(shift_selection_2, 'END')

# build date hour framework ====
dates_sql <- paste("SELECT 
                        MIN(EXTRACT(DATE FROM CHARTTIME_COLLAPSED)) AS min_date, 
                        MAX(EXTRACT(DATE FROM CHARTTIME_COLLAPSED)) AS max_date
                   FROM `MIMIC3_V1_4.CHARTEVENTS_DEPTS_CATS_TS_COLLAPSED_NEW`")
dates <- as.data.frame(dbGetQuery(con, dates_sql))
units_sql <- paste("SELECT 
                        DISTINCT CURR_CAREUNIT
                   FROM `MIMIC3_V1_4.CHARTEVENTS_DEPTS_CATS_TS_COLLAPSED_NEW`")
units <- as.data.frame(dbGetQuery(con, units_sql))$CURR_CAREUNIT

frame <- as.data.frame(rep(seq(as.Date(dates[1,1]),as.Date(dates[1,2]), by="day"), 
                           each=(6 * total_hours / splits )))
frame$CHART_SHIFT <- rep(1:(total_hours / splits),each=6)
frame$CURR_UNIT <- rep(units,(total_hours / splits))
colnames(frame)[1] <- "CHART_DATE"


frame <- frame %>% 
  mutate(DATE_HOUR = as.POSIXct(CHART_DATE + lubridate::hours((((CHART_SHIFT-1)*(splits))))),
         DATE_HOUR_PLUS = as.POSIXct(CHART_DATE + lubridate::hours((((CHART_SHIFT)*(splits))))))

# write table to BigQuery
#dbWriteTable(con, name = "SKELETON_4", frame, row.names = TRUE, overwrite = TRUE)


