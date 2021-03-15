visualization_panel_ui <- function(id, label = "visualization") {
  ns <- NS(id)
  tabPanel("Visualization and estimation",
           sidebarLayout(
             sidebarPanel(
                 
             ),
             mainPanel(
               plotOutput(ns("analysis_data_plot"))
             )
           )
  )
}

visualization_panel_server <- function(id, analysis_data = NULL) {
  moduleServer(
    id, 
    function(input, output, session) { 
      
      centered_analysis_data <- reactive({
        analysis_data() %>%
          mutate(x_centered = x_hit - x_target,
                 y_centered = y_hit - y_target
          )
      })
      
      # TODO: wrong output of plot
      output$analysis_data_plot <- renderPlot({
          dart_board_plt +
            geom_point(
              data = centered_analysis_data(),
              mapping = aes(x = x_centered,
                            y = y_centered),
              color = "darkblue", 
              shape = 4,
              size = 3,
              stroke = 2
            )
      })  
    }
  )
}