library(shiny)

source("Dart_Collector_support_2.R")

ui <- fluidPage(
    titlePanel("Data Collection and Analysis on your dart throws"),
    tabsetPanel(
        tabPanel("Data collection",
            sidebarLayout(
                sidebarPanel(
                    radioButtons(
                        "point_type",
                        label = "Kind of point to mark",
                        choices = c("Target point", "Hit point")
                    ),
                    actionButton("reset_target", label = "Reset target point to center!")
                ),
        
                mainPanel(
                   plotOutput("dart_board", click = "board_click"),
                   splitLayout(
                       tableOutput("current_choices"),
                       actionButton("save_throw", label = "Save throw!")
                   )
                )
            ),
            sidebarLayout(
                sidebarPanel(
                    actionButton("delete_all", "Delete all throws!"),
                    actionButton("send_to_analysis", "Send throws to analysis section"),
                    downloadButton("download_throws", "Download throws as .csv")
                ),
                mainPanel(
                    tableOutput("data_collected")
                )
            )
        ),
        tabPanel("Analysis data management"),
        tabPanel("Visualization and Estimation"),
        tabPanel("Statistical testing"),
        tabPanel("Nonparametric simulation")
    )
)

server <- function(input, output) {
    # initialize reactive values
    board <- reactiveValues(
        points = tibble(
            type = c("target", "hit"),
            x = c(0, 0),
            y = c(0, 0),
            cat_result = c("Bulls Eye", "Bulls Eye")
        ) 
    )
    data <- reactiveValues(
        collected = tibble(
            x_target = NULL,
            y_target = NULL,
            x_hit = NULL,
            y_hit = NULL,
            hit_result = NULL
        )
    )
    
    # note point clicked on board plot
    observeEvent(input$board_click, {
        if (input$point_type == "Target point") {
            board$points[board$points$type == "target", c("x", "y")] <- list(x = input$board_click$x, y = input$board_click$y)
        } else if (input$point_type == "Hit point") {
            board$points[board$points$type == "hit", c("x", "y")] <- list(x = input$board_click$x, y = input$board_click$y)
        }
        board$points <- board$points %>%
            mutate(cat_result = dart_result_cat(board$points %>% select(x,y)))
    })
    
    # Reset target button
    observeEvent(input$reset_target, {
        board$points[board$points$type == "target", c("x", "y", "cat_result")] <-list(x = 0, y = 0, cat_result = "Bulls Eye")
    })
    
    # add throw to collected_data, when respective buttton is hit
    observeEvent(input$save_throw, {
        new_row <- tibble(
            x_target = board$points %>% filter(type == "target") %>% select(x) %>% pull(),
            y_target = board$points %>% filter(type == "target") %>% select(y) %>% pull(),
            x_hit = board$points %>% filter(type == "hit") %>% select(x) %>% pull(),
            y_hit = board$points %>% filter(type == "hit") %>% select(x) %>% pull(),
            hit_result = board$points %>% filter(type == "hit") %>% select(cat_result) %>% pull()
        )
        data$collected <- data$collected %>%
            bind_rows(new_row)
    })
    
    # show board_plot including current target and hit point
    output$dart_board <- renderPlot({
        dart_board_plt +
             geom_point(data = board$points, 
                        mapping = aes(x = x, y = y, color = type, shape = type),
                        size = 3, 
                        stroke = 2) +
            scale_shape_manual(values = c(4, 3)) + 
            scale_color_manual(values = c("red", "darkgreen"))
    })
    
    # show current points selected, including the corresponding results as a category
    output$current_choices <- renderTable({
        board$points
    },
    digits = 3)
    
    # show table of data collected up to this point
    output$data_collected <- renderTable({
        data$collected
    })
    
    # Delete all throws button
    observeEvent(input$delete_all, {
        data$collected <- tibble(
            x_target = NULL,
            y_target = NULL,
            x_hit = NULL,
            y_hit = NULL,
            hit_result = NULL
        )
    })
    
    # Download .csv file
    output$download_throws <- downloadHandler(
        filename = function() {
            "dart_throws.csv"
        },
        content = function(file) {
            write_csv(data$collected, file)
        }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
