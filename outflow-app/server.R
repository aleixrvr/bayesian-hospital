server <- function(input, output, session) {
  # Hospital reactive ====
  base_hospital <- reactive({
    hospital_df <- cbind(
      unit_names <- c("CCU", "SICU", "MICU", "CSRU", "TSICU"),
      values <- rbind(
        sum(
          do_unit(
            ("CCU"),
            resources_sel_inc = input$resources,
            shift_num_sel = input$time
          )[[2]]
        ),
        sum(
          do_unit(
            ("SICU"),
            resources_sel_inc = input$resources,
            shift_num_sel = input$time
          )[[2]]
        ),
        sum(
          do_unit(
            ("MICU"),
            resources_sel_inc = input$resources,
            shift_num_sel = input$time
          )[[2]]
        ),
        sum(
          do_unit(
            ("CSRU"),
            resources_sel_inc = input$resources,
            shift_num_sel = input$time
          )[[2]]
        ),
        sum(
          do_unit(
            ("TSICU"),
            resources_sel_inc = input$resources,
            shift_num_sel = input$time
          )[[2]]
        )
      )
    )
    hospital_df <- hospital_df %>%
      as.data.table() %>%
      rename("unit" = "V1",
             "outflow" = "V2") %>%
      mutate(outflow = as.numeric(outflow))
  })
  
  # Individual unit reactives ====
  base_hospital_ccu <- reactive({
    do_unit(
      ("CCU"),
      resources_sel_inc = input$resources,
      shift_num_sel = input$time
    )
  })
  base_hospital_sicu <- reactive({
    do_unit(
      ("SICU"),
      resources_sel_inc = input$resources,
      shift_num_sel = input$time
    )
  })
  base_hospital_csru <- reactive({
    do_unit(
      ("CSRU"),
      resources_sel_inc = input$resources,
      shift_num_sel = input$time
    )
  })
  base_hospital_micu <- reactive({
    do_unit(
      ("MICU"),
      resources_sel_inc = input$resources,
      shift_num_sel = input$time
    )
  })
  base_hospital_tsicu <- reactive({
    do_unit(
      ("TSICU"),
      resources_sel_inc = input$resources,
      shift_num_sel = input$time
    )
  })
  
  # Plot reactive ====
  hospital_plot_df <- reactive({ 
    
    url_hash <- getUrlHash()
    url_hash_sub <- (substr(url_hash, 2, 1000000))
    qs <- parseQueryString(url_hash_sub)
    
    unit_components <- do_unit(
      ifelse(is.null(qs$unit), "SICU", qs$unit)  ,
      resources_sel_inc = input$resources,
      shift_num_sel = input$time
    )
    
    unit_components$Unit <- levels(unit_components$do_unit)
    unit_components$type <-
      ifelse(unit_components$out_inc < 0, "below", "above")
    # unit_components <-
    #  unit_components[order(unit_components$out_inc),]  # sort
    unit_components$Unit <-
      factor(unit_components$Unit, levels = unit_components$Unit)
    
    unit_components$spacing <- c(1,3,5,7,9)
    
    unit_components <- unit_components %>% as.data.frame()
  })
  
  # Recomendation reactive ====
  base_hospital_reccomendation <- reactive({
    reccomendation_df <- cbind(
      unit_names <- c("CCU", "SICU", "MICU", "CSRU", "TSICU"),
      values <- rbind(base_hospital_ccu <- base_hospital_ccu() %>%
                        select(out_inc) %>%
                        colSums(),
                      base_hospital_sicu <- base_hospital_sicu() %>%
                        select(out_inc) %>%
                        colSums(),
                      base_hospital_micu <- base_hospital_micu() %>%
                        select(out_inc) %>%
                        colSums(),
                      base_hospital_csru <- base_hospital_csru() %>%
                        select(out_inc) %>%
                        colSums(),
                      base_hospital_tsicu <- base_hospital_tsicu() %>%
                        select(out_inc) %>%
                        colSums()))
    
    reccomendation_df <- reccomendation_df %>%
      as.data.table() %>% 
      rename("outflow" = "out_inc") %>% 
      mutate(outflow = as.numeric(outflow))
    reccomendation_df <- reccomendation_df[which.max(reccomendation_df$outflow),]
  })
  
  # CCU ValueBox ====
  output$net_ccu <- renderValueBox({
    base_hospital_ccu <- base_hospital_ccu() %>%
      as.data.frame()
    base_hospital_reccomendation <- base_hospital_reccomendation()
    
    sum_ccu <- base_hospital_ccu() %>%
      select(out_inc) %>%
      colSums() %>%
      unname() %>%
      round(2)
    
    color <- 'yellow'
    if (sum_ccu < -10.1)
      color <- 'red'
    if (sum_ccu > 10.1)
      color <- 'green'
    if (base_hospital_reccomendation[[1]] == "CCU")
      color <- 'yellow'
    if (sum_ccu == 0)
      color <- 'yellow'
    if (as.character(hospital_plot_df()[which.max(abs(hospital_plot_df()$out_inc)), ][[1]]) == "CCU")
      color <- 'aqua'
    
    icon_ccu <- icon("heartbeat")
    if (base_hospital_reccomendation[[1]] == "CCU")
      icon_ccu <- icon("heartbeat")
    if (sum_ccu == 0)
      icon_ccu <- icon("heartbeat")
    if (as.character(hospital_plot_df()[which.max(abs(hospital_plot_df()$out_inc)), ][[1]]) == "CCU")
      icon_ccu <- icon("heartbeat", class = "bolt2")
    
    subtitle_ccu <- paste0(
      round(base_hospital_ccu[[2]][1], 2),
      "+",
      round(base_hospital_ccu[[2]][2], 2),
      "+",
      round(base_hospital_ccu[[2]][3], 2),
      "+",
      round(base_hospital_ccu[[2]][4], 2),
      "+",
      round(base_hospital_ccu[[2]][5], 2),
      " = ",
      sum_ccu
    )
    
    if (is.na(base_hospital_ccu[[2]][1])) subtitle_ccu <- "Resource Limit"
    
    resources <- base_hospital_micu()$resources[2]
    
    valueBox(
      value = paste0("CCU (",resources,")"),
      icon = icon_ccu,
      color = color,
      subtitle = subtitle_ccu,
      href = "#?unit=CCU"
    )
  })
  
  # SICU ValueBox ====
  output$net_sicu <- renderValueBox({
    base_hospital_sicu <- base_hospital_sicu() %>%
      as.data.frame()
    base_hospital_reccomendation <- base_hospital_reccomendation()
    
    sum_sicu  <- base_hospital_sicu() %>%
      select(out_inc) %>%
      colSums() %>%
      unname() %>%
      round(2)
    
    color <- 'yellow'
    if (sum_sicu < -10.1)
      color <- 'red'
    if (sum_sicu > 10.1)
      color <- 'green'
    if (base_hospital_reccomendation[[1]] == "SICU")
      color <- 'yellow'
    if (as.character(hospital_plot_df()[which.max(abs(hospital_plot_df()$out_inc)), ][[1]]) == "SICU")
      color <- 'aqua'
    
    icon_sicu <- icon("syringe")
    if (base_hospital_reccomendation[[1]] == "SICU")
      icon_sicu <- icon("syringe")
    if (as.character(hospital_plot_df()[which.max(abs(hospital_plot_df()$out_inc)), ][[1]]) == "SICU")
      icon_sicu <- icon("syringe", class = "bolt2")
    
    subtitle_sicu <- paste0(
      round(base_hospital_sicu[[2]][1], 2),
      "+",
      round(base_hospital_sicu[[2]][2], 2),
      "+",
      round(base_hospital_sicu[[2]][3], 2),
      "+",
      round(base_hospital_sicu[[2]][4], 2),
      "+",
      round(base_hospital_sicu[[2]][5], 2),
      " = ",
      sum_sicu
    )
    
    if (is.na(base_hospital_sicu[[2]][1])) subtitle_sicu <- "Resource Limit"
    
    
    resources <- base_hospital_micu()$resources[5]
    
    valueBox(
      value = paste0("SICU (",resources,")"),
      icon = icon_sicu,
      color = color,
      subtitle = subtitle_sicu,
      href = "#?unit=SICU"
    )
  })
  
  # MICU ValueBox ====
  output$net_micu <- renderValueBox({
    base_hospital_micu <- base_hospital_micu() %>%
      as.data.frame()
    base_hospital_reccomendation <- base_hospital_reccomendation()
    
    sum_micu  <- base_hospital_micu() %>%
      select(out_inc) %>%
      colSums() %>%
      unname() %>%
      round(2)
    
    color <- 'yellow'
    if (sum_micu < -10.1)
      color <- 'red'
    if (sum_micu > 10.1)
      color <- 'green'
    if (base_hospital_reccomendation[[1]] == "MICU")
      color <- 'yellow'
    if (as.character(hospital_plot_df()[which.max(abs(hospital_plot_df()$out_inc)), ][[1]]) == "MICU")
      color <- 'aqua'
    
    icon_micu <- icon("user-md")
    if (base_hospital_reccomendation[[1]] == "MICU")
      icon_micu <- icon("user-md")
    if (as.character(hospital_plot_df()[which.max(abs(hospital_plot_df()$out_inc)), ][[1]]) == "MICU")
      icon_micu <- icon("user-md", class = "bolt2")
    
    subtitle_micu <- paste0(
      round(base_hospital_micu[[2]][1], 2),
      "+",
      round(base_hospital_micu[[2]][2], 2),
      "+",
      round(base_hospital_micu[[2]][3], 2),
      "+",
      round(base_hospital_micu[[2]][4], 2),
      "+",
      round(base_hospital_micu[[2]][5], 2),
      " = ",
      sum_micu)
    
    if (is.na(base_hospital_micu[[2]][1])) subtitle_micu <- "Resource Limit"
    
    resources <- base_hospital_micu()$resources[3]
    
    valueBox(
      value = paste0("MICU (",resources,")"),
      icon = icon_micu,
      color = color,
      subtitle = subtitle_micu,
      href = "#?unit=MICU"
    )
  })
  
  # CSRU ValueBox ====
  output$net_csru <- renderValueBox({
    base_hospital_csru <- base_hospital_csru() %>%
      as.data.frame()
    base_hospital_reccomendation <- base_hospital_reccomendation()
    
    sum_csru  <- base_hospital_csru() %>%
      select(out_inc) %>%
      colSums() %>%
      unname() %>%
      round(2)
    
    color <- 'yellow'
    if (sum_csru < -10.1)
      color <- 'red'
    if (sum_csru > 10.1)
      color <- 'green'
    if (base_hospital_reccomendation[[1]] == "CSRU")
      color <- 'yellow'
    if (as.character(hospital_plot_df()[which.max(abs(hospital_plot_df()$out_inc)), ][[1]]) == "CSRU")
      color <- 'aqua'
    
    icon_csru <- icon("procedures")
    if (base_hospital_reccomendation[[1]] == "CSRU")
      icon_csru <- icon("procedures")
    if (as.character(hospital_plot_df()[which.max(abs(hospital_plot_df()$out_inc)), ][[1]]) == "CSRU")
      icon_csru <- icon("procedures", class = "bolt2")
    
    subtitle_csru <- paste0(
      round(base_hospital_csru[[2]][1], 2),
      "+",
      round(base_hospital_csru[[2]][2], 2),
      "+",
      round(base_hospital_csru[[2]][3], 2),
      "+",
      round(base_hospital_csru[[2]][4], 2),
      "+",
      round(base_hospital_csru[[2]][5], 2),
      " = ",
      sum_csru
    )
    
    if (is.na(base_hospital_csru[[2]][1])) subtitle_csru <- "Resource Limit"
    
    resources <- base_hospital_micu()$resources[4]
    
    valueBox(
      value = paste0("CSRU (",resources,")"),
      icon = icon_csru,
      color = color,
      subtitle = subtitle_csru,
      href = "#?unit=CSRU"
    )
  })
  
  
  # TSICU ValueBox ====
  output$net_tsicu <- renderValueBox({
    base_hospital_tsicu <- base_hospital_tsicu() %>%
      as.data.frame()
    base_hospital_reccomendation <- base_hospital_reccomendation()
    
    sum_tsicu  <- base_hospital_tsicu() %>%
      select(out_inc) %>%
      colSums() %>%
      unname() %>%
      round(2)
    
    color <- 'yellow'
    if (sum_tsicu < -10.1)
      color <- 'red'
    if (sum_tsicu > 10.1)
      color <- 'green'
    if (base_hospital_reccomendation[[1]] == "TSICU")
      color <- 'yellow'
    if (as.character(hospital_plot_df()[which.max(abs(hospital_plot_df()$out_inc)), ][[1]]) == "TSICU")
      color <- 'aqua'
    if (sum_tsicu == 0)  color <- 'yellow'
    
    icon_tsicu <- icon("ambulance")
    if (base_hospital_reccomendation[[1]] == "TSICU")
      icon_tsicu <- icon("ambulance")
    if (as.character(hospital_plot_df()[which.max(abs(hospital_plot_df()$out_inc)), ][[1]]) == "TSICU")
      icon_tsicu <- icon("ambulance", class = "bolt2")
    if (sum_tsicu == 0)
      icon_tsicu <- icon("ambulance")
    
    subtitle_tsicu <- paste0(
      round(base_hospital_tsicu[[2]][1], 2),
      "+",
      round(base_hospital_tsicu[[2]][2], 2),
      "+",
      round(base_hospital_tsicu[[2]][3], 2),
      "+",
      round(base_hospital_tsicu[[2]][4], 2),
      "+",
      round(base_hospital_tsicu[[2]][5], 2),
      " = ",
      sum_tsicu
    )
    
    if (is.na(base_hospital_tsicu[[2]][1])) subtitle_tsicu <- "Resource Limit"
    
    resources <- base_hospital_micu()$resources[1]
    
    valueBox(
      value = paste0("TSICU (",resources,")"),
      icon = icon_tsicu,
      color = color,
      subtitle = subtitle_tsicu,
      href = "#?unit=TSICU"
    )
  })
  
  
  # Reccomendation ValueBox ====
  output$reccomendation <- renderValueBox({
    base_hospital_reccomendation <- base_hospital_reccomendation()
    color <- 'yellow'
    infoBox(
      subtitle = ifelse(base_hospital_reccomendation[[1]] == "CCU", "", paste(base_hospital_reccomendation[[1]],"( €",round(base_hospital_reccomendation[[2]]*10000, 2),")")),
      color = color,
      title = "Recommendation",
      icon = icon("thumbs-up")
    )
  })
  
  # # Economic ValueBox ====
  # output$economic <- renderValueBox({
  #   base_hospital_reccomendation <- base_hospital_reccomendation()
  #   color <- 'yellow'
  #   infoBox(
  #     subtitle = ifelse(base_hospital_reccomendation[[1]] == "CCU", "",  paste0("€",round(base_hospital_reccomendation[[2]]*100))),
  #     color = color,
  #     title = "Economic Impact",
  #     icon = icon("money-bill-wave")
  #   )
  # })
  
  # Render Plot ====
  output$outflow_breakdown <- renderPlot({
    outflow <- hospital_plot_df()$out_inc
    ggplot(hospital_plot_df(), aes(x = spacing, y = out_inc, label = out_inc)) +
      geom_bar(stat = 'identity', aes(fill = type), width =1.5) +
      scale_fill_manual(
        name = "Outflow",
        labels = c("Positive", "Negative"),
        values = c("above" = "#00A65A", "below" = "#DD4B39")
      ) +
      #    ggtitle(paste0(as.character(hospital_plot_df()[which.max(abs(hospital_plot_df()$out_inc)), ][[1]]))) +
      #labs(title= "Outflow Breakdown") +
      theme(
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        # axis.text = element_text(size = 16),
        axis.text = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = '#222d32', colour = '#222d32'),
        plot.background = element_rect(fill = '#222d32', colour = '#222d32')
      ) +
      scale_y_continuous(
        limits = c( -max(abs(outflow))-(max(abs(outflow))/4)-0.01, 
                    max(abs(outflow))+ (max(abs(outflow))/4)+0.01)) +
      geom_hline(yintercept = 0, size = 2, color = "#5a5a5a") +
      theme(plot.margin = unit(c(0, 1, 1, 1), "cm")) +
      geom_text(aes(y = ifelse(sign(out_inc) == -1, out_inc+(max(abs(outflow))/4)*sign(out_inc), out_inc+(max(abs(outflow))/4)*sign(out_inc)),
                    label= ifelse( round(out_inc,3) == 0.000,
                                   "" ,
                                   format(round(out_inc,3))) ),
                position=position_dodge(width = 1),  
                size=10, colour = "#bebebe") +
      # ylab("Outflow") +
      coord_flip()
  })
  
  
  
}
