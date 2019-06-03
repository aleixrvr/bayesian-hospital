# source('../utils/stats_function.R')
source('../utils/hospital_network.R')
source('../utils/time_series.R') 
library(data.table)

function(input, output) {

  plot_network_call <- reactive({
    plots <- plot_network()
    plots
  })
  
  output$transition_matrix <- renderPlot({
    plot_network_call()$trans_mat
  })
  
  output$dag <- renderPlot({
    plot_network_call()$hospt_dag
  })
  
  output$los_correlation <- renderPlot({
    plot_network_call()$trans_los
  })
  
  output$week_los <- renderPlot({
    plot_network_call()$week_los
  })
  
  
  output$departs <- renderTable({
    fread('../../departs.csv')
  })
  
  output$iculos <- renderPlot({
    plot_network_call()$icu_los
  })
  
  output$timeseries <- renderPlot({
    aggregation <- reactive({ input$aggregation })
    time_window <- reactive({ input$time_window })
    shift <- reactive({ input$shift })
    objective <- reactive({ input$objective })
    plot_timeseries(objective(),shift(),time_window(),aggregation())
    
  })
}

