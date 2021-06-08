visualization_panel_ui <- function(id, label = "visualization") {
  ns <- NS(id)
  tabPanel("Visualization and estimation",
           p(
             "The estimated values of the vertical and horizontal standard deviations of your throws are",
              tableOutput(ns("sd_table")),
              "You might be interested in going to the ",
              tags$a("dart_optimizer", href="https://lradloff.shinyapps.io/dart_optimizer/"),
              "and plugging these values into the application."
           ),
           tabsetPanel(
             tabPanel("Centered Data Visualization",
               sidebarLayout(
                 sidebarPanel(
                   h4("Customize plot"),
                   checkboxInput(ns("color_by_target"), label = "Color points by original target", value = 0),
                   checkboxInput(ns("show_ellipses"), label = "Show 66% and 90% prediction ellipses", value = 0)
                 ),
                 mainPanel(
                   plotOutput(ns("analysis_data_plot"))
                 )
               )
             ),
             tabPanel("Raw Data Visualization",
               plotOutput(ns("raw_analysis_data_plot"))
             )
           )
  )
}

visualization_panel_server <- function(id, analysis_data = NULL) {
  moduleServer(
    id, 
    function(input, output, session) { 
      
      # compute data which are corrected for target position
      # i.e. compute deviations as if all targets were the board's center 
      centered_analysis_data <- reactive({
        analysis_data() %>%
          mutate(x_centered = x_hit - x_target,
                 y_centered = y_hit - y_target
          )
      })
      
      # compute estimations of standard deviations in both directions
      # based on the analysis data
      sd_horiz_estimate <- reactive({
        sd(centered_analysis_data()$x_centered)
      })
      sd_vert_estimate <- reactive({
        sd(centered_analysis_data()$y_centered)
      })
      
      # produce output table for displaying estimations of standard deviations
      output$sd_table <- renderTable({
        tibble(
          "Vertical Standard Deviation" = round(sd_vert_estimate(), 2),
          "Horizontal Standard Deviation" = round(sd_horiz_estimate(), 2)
        )
      })
      
      # produce plot for visualizing the analysis data
      # this plot shows the centered data
      output$analysis_data_plot <- renderPlot({
        if (input$show_ellipses) {
          centered_throws_plot(centered_analysis_data(), 
                               color_by_target = input$color_by_target, 
                               sds_for_ellipses = c("horiz" = sd_horiz_estimate(), 
                                                    "vert" = sd_vert_estimate()))
        } else {
          centered_throws_plot(centered_analysis_data(), 
                               color_by_target = input$color_by_target)
        }
      })
      
      # produce plot for visualizing the analysis data (2)
      # here the raw data are shown, i.e. those not centered to "target = Bully Eye"
      output$raw_analysis_data_plot <- renderPlot({
        raw_throws_plot(analysis_data())
      })
    }
  )
}