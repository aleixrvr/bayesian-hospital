library(shinydashboard)

dashboardPage(
  dashboardHeader(title = "Hospital Dynamics"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Stats", tabName = "stats", icon = icon("th")),
      menuItem("Hospital Graph", tabName = "hospital_graph", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "stats",
              fluidRow(tags$head(
                tags$style(
                  HTML(".shiny-notification {
              height: 50px;
              width: 800px;
              position:fixed;
              top: calc(50% - 50px);;
              left: calc(50% - 400px);;
            }
           "
                  )
                )
              ),
                box(h2("Time Series for the Selected Parameters"),
                    plotOutput("timeseries"), width = 9),
                box(h2("Parameter Selection"), width = 3,
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
                                           # "Shift 1"= "CHARTTIME_SHIFT_1", 
                                           # "Shift 2"= "CHARTTIME_SHIFT_2", 
                                           # "Shift 3"= "CHARTTIME_SHIFT_3",
                                           # "Shift 4"= "CHARTTIME_SHIFT_4",
                                           # "Shift 5"= "CHARTTIME_SHIFT_5",
                                           # "Collapsed"= "INTIME_COLLAPSED",
                                           "Collapsed Shift 1"= "CHARTTIME_COLLAPSED_SHIFT_1",
                                           "Collapsed Shift 2"= "CHARTTIME_COLLAPSED_SHIFT_2",
                                           "Collapsed Shift 3"= "CHARTTIME_COLLAPSED_SHIFT_3",
                                           "Collapsed Shift 4"= "CHARTTIME_COLLAPSED_SHIFT_4",
                                           "Collapsed Shift 5"= "CHARTTIME_COLLAPSED_SHIFT_5")), 
                    #" ", br(), 
                    dateRangeInput(inputId = "time_window", 
                                   label = "Select the time window for the series", 
                                   start = "1990-01-01", end = "2210-01-01", 
                                   startview = "decade", weekstart = 1), 
                    submitButton("Update View", icon("refresh"))
                )
              ), 
              fluidRow(
                box(
                  h2("Distribution of Length of Stay at the ICU"),
                  plotOutput("iculos"),width = 6),
                box(h2('Inter-Unit Correlation of Length of Stay'),
                    plotOutput('los_correlation'), width = 6)
              ),
              fluidRow(
                box(h2('Length of Stay by Weekday and Admission Hour'),
                    plotOutput('week_los'), width = 6)
              )),
      # Second tab content
      tabItem(tabName = "hospital_graph",
              fluidRow(
                
                box(
                  h2("Inter-Unit Transition Matrix"),
                  plotOutput("transition_matrix"), width=6),
                box(
                  h2("Description of the ICU-Units"),
                  tableOutput("departs"), width=6)
              ),
              fluidRow(
                box(
                  h2("Average Transitions per Year"),
                  plotOutput("trans_rate_matrix"), width=6),#
                box(
                  h2("Hospital Graph"),
                  plotOutput("dag"), width=6)
              )
      )
    )
  )
)
