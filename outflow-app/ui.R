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
      )
    ),
    sidebarMenu(
      sliderInput(
        width = 400,
        inputId = "resources",
        label = "Resource Policy:",
        min = -10,
        max = 10,
        value = 2
      ),
      br(),
      br(),
      infoBoxOutput("reccomendation",width = 12)
    )
  ),
  # Body ====
  dashboardBody(
    includeCSS("www/css/style.css"),
    box(width = 5,
     # title = "ICU Optimizer",
      id = "tabs_1",# title = "Units",
     (valueBoxOutput("net_sicu", width = 12)),
     (valueBoxOutput("net_csru", width = 12)),
     (valueBoxOutput("net_micu", width = 12)),
     (valueBoxOutput("net_ccu", width = 12)),
     (valueBoxOutput("net_tsicu", width = 12))
    ),
    box(#title = "Outflow Per Unit",
      width = 7,
      plotOutput("outflow_breakdown", height = "88vh")
    )
  )
)
