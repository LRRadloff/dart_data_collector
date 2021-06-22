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
    p(
        "The ",
        tags$a("dart_optimizer", href="https://lradloff.shinyapps.io/dart_optimizer/"),
        " provides information about which point to target when playing dart - given 
        your precision. But how do you know your precision? A statistician's approach 
        would be to collect data. In this case that means cartesian coordinates of 
        target and hit points of your throws on a dart board. That's impracticable 
        to do by hand, but this app simplifies the process."
    ),
    p(
        "Further, estimation of the ",
        tags$a("dart_optimizer", href="https://lradloff.shinyapps.io/dart_optimizer/"),
        "'s parameters and statistical testing of its assumptions are provided."
    ),
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
