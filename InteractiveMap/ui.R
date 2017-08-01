library(shinydashboard)
library(rCharts)
library(leaflet)

dashboardPage(title="ITS Mapping Module",
              dashboardHeader(title="ITS Mapping Module"),
              # Sidebar with a slider input for number of bins
              dashboardSidebar(),
              dashboardBody(
                # Show a plot of the generated distribution
                mainPanel(
                  leafletOutput("map",width="150%", height = 600)
                )
              )
)