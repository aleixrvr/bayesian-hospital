ui <- dashboardPage(
  skin = "black",
  # Header ====
  dashboardHeader(
    title = tags$a(
      href = '',
      tags$img(
        src = 'img/Acc_Applied_Intell_Wordmark_BLK_2.png',
        width = "385",
        height = "45"
      )
    ),
    titleWidth = 420
  ),
  # Sidebar ====
  dashboardSidebar(
    width = 420,
    includeCSS("www/css/style.css"),
    br(),
    radioGroupButtons(
      direction = "vertical",
      width = 400,
      inputId = "time",
      label = "Time Shift:",
      size = "lg",
      choices = list(
        "00:00 - 04:00" = 1,
        "04:00 - 08:00" = 2,
        "08:00 - 12:00 " = 3,
        "12:00 - 16:00" = 4,
        "16:00 - 20:00" = 5,
        "20:00 - 24:00" = 6
      )#,
     # selectize = TRUE
    ),
    br(),
    sidebarMenu(
      sliderInput(
        width = 400,
        inputId = "resources",
        label = "Resource Policy:",
        min = -10,
        max = 10,
        value = 2
      ),
      # Copy the line below to make a number input box into the UI.
      # numericInput("resource_ccu", label = h4("CCU Resources"), value = 22, min = 20, max = 40),
      # numericInput("resource_micu", label = h4("MICU Resources"), value = 57),
      # numericInput("resource_sicu", label = h4("SICU Resources"), value = 36),
      # numericInput("resource_tsicu", label = h4("TSICU Resources"), value = 26),
      # numericInput("resource_csru", label = h4("CSRU Resources"), value = 25),
      br(),
      br(),
      #actionButton("ccu_button", "CCU"),
      
      valueBoxOutput("reccomendation",width = 12)
      
      #br(),

      #helpText("Copyright @ 2019 Accenture. All rights reserved.")
     # menuItem(img(src='img/bh_icon.png', width = "290", height = "300", posistion = "center")),
    )
  ),
  # Body ====
  dashboardBody(
    includeCSS("www/css/style.css"),
    box(width = 5,
     # title = "ICU Optimizer",
      id = "tabs_1",# title = "Units",
      #  fluidRow(valueBoxOutput("reccomendation",width = 12)),
     (valueBoxOutput("net_sicu", width = 12)),
     (valueBoxOutput("net_csru", width = 12)),
     (valueBoxOutput("net_micu", width = 12)),
     (valueBoxOutput("net_ccu", width = 12)),
     (valueBoxOutput("net_tsicu", width = 12))
    ),
    # box(
    #   width = 8,
    #   title = "Outflow Breakdown",
    #   id = "tabs_1",
    #   selectInput(
    #     inputId = "unit_name",
    #     label = "Effect Of Resource Policy On:",
    #     choices = list("CSRU",
    #                    "CCU",
    #                    "SICU",
    #                    "TSICU",
    #                    "MICU"),
    #     selectize = TRUE
    #   )
    # ),
    box(#title = "Outflow Per Unit",
      width = 7,
      plotOutput("outflow_breakdown", height = "88vh")
    )
  )
)
