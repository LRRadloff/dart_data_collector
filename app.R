library(shiny)
library(DT)

source("Dart_Collector_support_2.R")
source("Dart_Collector_ShinySupport.R")
source("collection_panel.R")

ui <- fluidPage(
    titlePanel("Data Collection and Analysis on your dart throws"),
    tabsetPanel(
        collection_panel_ui("collection"),
        tabPanel("Data management"),
        tabPanel("Visualization and Estimation"),
        tabPanel("Statistical testing"),
        tabPanel("Nonparametric simulation")
    )
)

server <- function(input, output, session) {
    collection_panel_server("collection")
}

# Run the application 
shinyApp(ui = ui, server = server)
