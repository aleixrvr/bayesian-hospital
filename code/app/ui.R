library(shinydashboard)

dashboardPage(
  dashboardHeader(title = "Hospital Dynamics"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Time Stats", tabName = "time_stats", icon = icon("th")),
      menuItem("Hospital Graph", tabName = "hospital_graph", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "time_stats",
              fluidRow(
                box(h2("Time Series for different Features"),
                    plotOutput("timeseries"), width = 9),
                box(h2("Parameter Selection Time Series"), width = 3,
                    selectInput(inputId="objective", 
                                label="Select Objective (y-axis): ", 
                                choices= c("Patients"= "avg_pat", 
                                           "Treatments"= "avg_treat", 
                                           "Medical Staff"= "avg_staff", 
                                           "Length of Stay"="avg_los")),
                    #" ", br(), 
                    selectInput(inputId="aggregation", 
                                label="Select Aggregation Level (x-axis): ", 
                                choices= c("Quarter"= "QUARTER", 
                                           "Month"= "MONTH", 
                                           "Week"="ISOWEEK")),
                    #" ", br(), 
                    selectInput(inputId="shift", 
                                label="Select Shift: ", 
                                choices= c("Original"= "CHARTTIME", 
                                           "Shift 1"= "CHARTTIME_SHIFT_1", 
                                           "Shift 2"= "CHARTTIME_SHIFT_2", 
                                           "Shift 3"= "CHARTTIME_SHIFT_3",
                                           "Shift 4"= "CHARTTIME_SHIFT_4",
                                           "Shift 5"= "CHARTTIME_SHIFT_5",
                                           "Collapsed"= "INTIME_COLLAPSED",
                                           "Collapsed Shift 1"= "INTIME_COLLAPSED_SHIFT_1",
                                           "Collapsed Shift 2"= "INTIME_COLLAPSED_SHIFT_2",
                                           "Collapsed Shift 3"= "INTIME_COLLAPSED_SHIFT_3",
                                           "Collapsed Shift 4"= "INTIME_COLLAPSED_SHIFT_4",
                                           "Collapsed Shift 5"= "INTIME_COLLAPSED_SHIFT_5")), 
                    #" ", br(), 
                    dateRangeInput(inputId = "time_window", 
                                   label = "Select the time window for the series", 
                                   start = "1990-01-01", end = "2210-01-01", 
                                   startview = "decade", weekstart = 1), 
                    submitButton("Update View", icon("refresh"))
                )
              ), 
              fluidRow(box(h2("Hospital Stats"),
                           plotOutput("iculos"),width = 6)
              )),
      # Second tab content
      tabItem(tabName = "hospital_graph",
              fluidRow(
                h2("Department Transition Matrix"),
                box(plotOutput("transition_matrix")),
                box(tableOutput("departs"))
              ),
              fluidRow(
                h2("Hospital Graph"),
                box(plotOutput("dag"))
              )
      )
    )
  )
)
