library(shiny)
library(DT)

source("plot_support_functions.R")
source("delete_button_support.R")
source("collection_panel.R")
source("management_panel.R")
source("visualization_panel.R")
source("test_panel.R")

ui <- fluidPage(
    titlePanel("Data Collection and Analysis on your dart throws"),
    tabsetPanel(
        collection_panel_ui("collection"),
        management_panel_ui("management"),
        visualization_panel_ui("visualization"),
        test_panel_ui("testing")
    )
)

server <- function(input, output, session) {
    collection <- collection_panel_server("collection")
    analysis_data <- management_panel_server("management", 
                                collection$send,
                                collection$throws
                            )
    visualization_panel_server("visualization", analysis_data)
    test_panel_server("testing", analysis_data)
}

# Run the application 
shinyApp(ui = ui, server = server)
