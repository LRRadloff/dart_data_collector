library(shiny)

source("Dart_Optimizer_support_2.R")

ui <- fluidPage(
    titlePanel("Collect data on your dart throws"),
    sidebarLayout(
        sidebarPanel(
            radioButtons(
                "point_type",
                label = "Kind of point to mark",
                choices = c("Target point", "Hitting point")
            ),
            actionButton("collect_throw", label = "Collect throw!")
        ),

        mainPanel(
           plotOutput("dart_board", click = "board_click")
        )
    )
)

server <- function(input, output) {
    
    dart_points_df <-  eventReactive(input$board_click, {
        df[df$type == "hit",c("x", "y")] <- list(x = 1, y = 1)
    })
    
    output$dart_board <- renderPlot({
        dart_board_plt
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
