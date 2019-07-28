library(magrittr)
library(data.table)

results_table <- readRDS('model-building/model/do_unit.RDS')
median_stats <- readRDS('model-building/model/median_stats.RDS')


median_stats %>% 
  do.call(rbind, .) %>% 
  data.table %>% 
  .[, shift_num := as.integer(rownames(.))] %>% 
  melt(id.vars='shift_num', variable.name='unit_name', value.name='resources_med') %>% 
  .[, resources_med := unlist(resources_med)] ->
  med_res

results_table %>% 
  merge(med_res, by=c('unit_name', 'shift_num')) %>% 
  .[abs(resources - resources_med) < 20] ->
  res_table


plots <- list()
for( shift_num_iter in shift_types ){
  res_table[shift_num==shift_num_iter] %>%
    ggplot(aes(resources, outflow, group=unit_name, color=unit_name)) +
    geom_line() +
    facet_grid(.~do_unit, scales = 'free')+
    ggtitle(paste0('Shift:', shift_num_iter)) ->
    plots[[shift_num_iter]]
}
sapply(plots, plot)
