library("bigrquery")
library(DBI)
library(dplyr)
library(data.table)
library(ggplot2)
library(magrittr)
library(ggdag)

Sys.setenv(BIGQUERY_TEST_PROJECT="bgse-dsc")
billing <- bq_test_project()

con <- dbConnect(
  bigrquery::bigquery(),
  project = "bgse-dsc",
  dataset = "MIMIC3_V1_4",
  billing = billing
)

get_transfers <- function(){
  as.data.table(tbl(con, "TRANSFERS"))
}

get_iculos <- function(){
  iculos = dbGetQuery(con, "SELECT LOS FROM MIMIC3_V1_4.ICUSTAYS WHERE LOS >= 0 AND LOS <=20")
}

plot_network <- function(){
  transfers <- get_transfers()  
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
    geom_tile(aes(fill = prop), color = "white") +
    scale_fill_gradient(low = "#e3ebed",
                        high = "#8aa6ad", 
                        name="Probability") +
      xlab("Previous Care Unit") +
      ylab("Current Care Unit") +
    scale_x_discrete(limits = rev(levels(trans_summary$PREV_CAREUNIT))) +
    coord_flip() +
    theme_light() +
    theme(legend.position = "right", legend.direction = "vertical",
          panel.grid.major = element_blank(),
          panel.border = element_blank()) ->
    trans_mat
  
  ### Transition Rate
  
  total_years <- 12
  transfers[, .(n = .N/total_years), .(PREV_CAREUNIT, CURR_CAREUNIT)] %>% 
    .[(PREV_CAREUNIT != 'OUT') | (CURR_CAREUNIT != 'OUT')] %>% 
    .[PREV_CAREUNIT != CURR_CAREUNIT] ->
    trans_summary_rate
  
  trans_summary_rate[, PREV_CAREUNIT := factor(PREV_CAREUNIT, levels = careunits)]
  trans_summary_rate[, CURR_CAREUNIT := factor(CURR_CAREUNIT, levels = careunits)]
  
  trans_summary_rate %>% 
    ggplot(aes(PREV_CAREUNIT, CURR_CAREUNIT)) +
    geom_tile(aes(fill = n), color = "white") +
    scale_fill_gradient(low = "#e3ebed",
                        high = "#8aa6ad", 
                        name="Transitions") +
    xlab("Previous Care Unit") +
    ylab("Current Care Unit") +
    scale_x_discrete(limits = rev(levels(trans_summary$PREV_CAREUNIT))) +
    coord_flip() +
    theme_light() +
    theme(legend.position = "right", legend.direction = "vertical",
          panel.grid.major = element_blank(),
          panel.border = element_blank()) ->
    trans_rate_mat
  
  ### Hospital network
  ord_dept <- function(prev, curr){
    c(prev %>% as.character, curr %>% as.character) %>% 
      as.character %>% 
      sort %>% 
      paste(collapse = '-')
  }
  
  trans_summary[, 
                ord_dept_res := ord_dept(PREV_CAREUNIT, CURR_CAREUNIT), 
                1:nrow(trans_summary)] 
  
  setorder(trans_summary, ord_dept_res, -n)
  
  trans_summary %>% 
    .[, .SD[1, ], ord_dept_res] %>% 
    .[, paste(PREV_CAREUNIT, CURR_CAREUNIT, sep='~')] %>% 
    lapply(as.formula) %>% 
    do.call(dagify, .) %>% 
      ggdag -> hospt_dag
  
  # plot histogram of length of stay in the ICU
  iculos_data <- get_iculos()
  ggplot(data = iculos_data, aes(LOS)) + 
      geom_histogram(binwidth = 1,fill=I("#9ebcda"), col=I("#FFFFFF")) +
      theme(panel.background = element_rect(fill = 'white', colour = 'white')) +
      labs(x="Length of stay in Days", y="Count") -> icu_los
  
  # LOS plot
  transfers %>%    
    .[is.na(CURR_CAREUNIT), CURR_CAREUNIT := 'OUT'] %>% 
    .[is.na(PREV_CAREUNIT), PREV_CAREUNIT := 'OUT'] %>% 
    .[(PREV_CAREUNIT != 'OUT') | (CURR_CAREUNIT != 'OUT')] %>% 
    .[!is.na(ICUSTAY_ID)] ->
    transfers_mod
  
  setorder(transfers_mod, ICUSTAY_ID, INTIME)
  transfers_mod %>% 
    .[, NEXT_CAREUNIT := shift(CURR_CAREUNIT, type="lead"), ICUSTAY_ID] %>% 
    .[, NEXT_LOS := shift(LOS, type="lead"), ICUSTAY_ID] 
  
  transfers_mod %>% 
    .[CURR_CAREUNIT != NEXT_CAREUNIT] %>% 
    ggplot(aes(NEXT_LOS, LOS)) +
    geom_point(color=I("#9ebcda"),size=2,alpha=.4) +
    labs(y="Length of Stay - Current Unit", x="Length of Stay  - Next Unit") +
    theme(strip.text.y = element_text(size=10, face="bold"),
          strip.background = element_rect(colour="white", fill=I("#9ebcda")),
          panel.background = element_rect(fill = 'white', colour = 'white')) +
    facet_grid(NEXT_CAREUNIT~CURR_CAREUNIT) ->
    trans_los
  
  # WEEK DATA
  
  transfers %>% 
    .[, time := ymd_hms(INTIME)] %>% 
    .[, hour := hour(time)] %>% 
    .[, wday := wday(time, label = TRUE)] ->
    week_trans
  
  ess_max_los <- as.numeric(week_trans[, quantile(na.omit(LOS), prob =.95)])
  
  week_trans %>% 
    .[, .(LOS = median(na.omit(LOS))), .(wday, hour)] %>% 
    na.omit %>% 
    ggplot(aes(hour, LOS)) +
    geom_line(color=I("#9ebcda"),size=.6) + 
    labs(x="Hour of the Day", y="Length of Stay") +
    theme(strip.text.y = element_text(size=10, face="bold"),
          strip.background = element_rect(colour="white", fill=I("#9ebcda")),
          panel.background = element_rect(fill = 'white', colour = 'white')) +
    facet_grid(wday~.) ->
    week_los
  
  return(list(
    trans_mat=trans_mat, 
    hospt_dag=hospt_dag, 
    icu_los=icu_los, 
    trans_los=trans_los,
    week_los=week_los,
    trans_rate_mat=trans_rate_mat))
}

plot_los <- function(){
  
}


