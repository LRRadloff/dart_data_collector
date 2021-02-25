library(shiny)

source("Dart_Optimizer_support_2.R")

ui <- fluidPage(
    titlePanel("Collect data on your dart throws"),
    sidebarLayout(
        sidebarPanel(
            radioButtons(
                "point_type",
                label = "Kind of point to mark",
                choices = c("Target point", "Hit point")
            ),
            actionButton("save_throw", label = "Save throw!")
        ),

        mainPanel(
           plotOutput("dart_board", click = "board_click"),
           verbatimTextOutput("info")
        )
    )
)

server <- function(input, output) {

    board = reactiveValues(
        points = tibble(type = c("target", "hit"),
               x = c(0, 0),
               y = c(0, 0))
    )
    observeEvent(input$board_click, {
        if (input$point_type == "Target point") {
            board$points[board$points$type == "target", c("x", "y")] <- list(x = input$board_click$x, y = input$board_click$y)
        } else if (input$point_type == "Hit point") {
            board$points[board$points$type == "hit", c("x", "y")] <- list(x = input$board_click$x, y = input$board_click$y)
        }
    })
    
    output$dart_board <- renderPlot({
        dart_board_plt +
             geom_point(data = board$points, 
                        mapping = aes(x = x, y = y, color = type, shape = type),
                        size = 3, 
                        stroke = 2) +
            scale_shape_manual(values = c(4, 3)) + 
            scale_color_manual(values = c("red", "darkgreen"))
    })
    
    output$info <- renderText({
        paste0("x=", input$board_click$x, "\ny=", input$board_click$y, "\npoint_type=", input$point_type)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
