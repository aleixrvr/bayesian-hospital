pkgTest <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}

# load packages ====
pkgTest("dplyr")
pkgTest("data.table")
pkgTest("shinyWidgets")
pkgTest("shinyBS")
pkgTest("shinydashboard")
pkgTest("ggplot2")

# load data ====
results_table <- readRDS('release/model/do_unit.RDS')
median_stats <- readRDS('release/model/median_stats.RDS')

get_shift_types <- function(){
  results_table[, unique(shift_num)]
}

get_unit_types <- function(){
  results_table[, unit_name %>% unique %>% as.character()]
}

do_unit <- function(unit_name_sel, resources_sel_inc=1, shift_num_sel=1)
{
  
  # median_stats[[1]]$TSICU <- resource_tsicu
  # median_stats[[1]]$CCU <- resource_ccu
  # median_stats[[1]]$MICU <- resource_micu
  # median_stats[[1]]$CSRU <- resource_csru
  # median_stats[[1]]$SICU <- resource_sicu
  
  median_number <- floor(median_stats[[as.character(shift_num_sel)]][[unit_name_sel]])
  resources_sel <- median_number + resources_sel_inc
  results_table %>% 
    .[unit_name==unit_name_sel] %>% 
    .[resources==resources_sel] %>% 
    .[shift_num==shift_num_sel] ->
    do_value
  
  results_table %>% 
    .[unit_name==unit_name_sel] %>% 
    .[resources==median_number] %>% 
    .[shift_num==shift_num_sel] ->
    do_value_mean
  
  do_value %>% 
    merge(do_value_mean, by='do_unit') %>% 
    .[, out_inc := outflow.x - outflow.y] ->
    do_inc
  
  results <- do_inc[, .(do_unit, out_inc)]
  results[out_inc * sign(resources_sel_inc) <0, out_inc := 0]
  results <- cbind(results,  do.call(rbind,  median_stats[[shift_num_sel]]))
  colnames(results)[3] <- "resources"
  return(results) 
}


