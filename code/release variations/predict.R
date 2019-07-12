library(dplyr)
library(data.table)

results_table <- readRDS('release/model/do_unit.RDS')
median_stats <- reaadRDS('release/model/median_stats.RDS')

get_resource_types <- function(){
  results_table[, unique(resources)]
}

# get_shift_types <- function(){
#   results_table[, unique(shifts)]
# }

get_resource_numbers <- function(){
  results_table
}

get_unit_types <- function(){
  results_table[, unit_name %>% unique]
}


do_unit(unit_name, resource_number, staff_type){
  results_table %>% 
    .[staff==staff_type] %>% 
    .[resources==resource_number] ->
    do_value
  
  median_number <- floor(median_stats[[staff_type]][[unit_name]])
  results_table %>% 
    .[staff==staff_type] %>% 
    .[resources==median_number] %>% 
    .[, .(do_unit, mean_outflow=outflow)]->
    do_mean_value
  
  do_value %>% 
    left_join(do_mean_value, by='do_unit') %>% 
    .[, out_inc := outflow - mean_outflow]
}

