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
              h2("Hospital Stats"),
              box(plotOutput("iculos"))
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
