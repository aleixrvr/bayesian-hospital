# source('../utils/stats_function.R')
source('../utils/hospital_network.R')
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
  
  output$departs <- renderTable({
    fread('../../departs.csv')
  })
  
  output$iculos <- renderPlot({
    plot_network_call()$icu_los
  })
}

