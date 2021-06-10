library("MVN")

test_panel_ui <- function(id, label = "testing") {
  ns <- NS(id)
  tabPanel("Statistical Testing",
           p(
             "Here you can perform statistical hypothesis tests for the assumptions made in the ",
             tags$a("dart_optimizer", href="https://lradloff.shinyapps.io/dart_optimizer/"),
             "."
           ),
           tabsetPanel(
             tabPanel("Normality",
                "P-values for four different tests for multivariat normality are provided. Tests are performed using the MVN library. Have a look at the ",
                tags$a("documentation", href="https://cran.r-project.org/web/packages/MVN/vignettes/MVN.pdf"),
                "for more details.", 
                tableOutput(ns("mvn_p_values_table"))         
             ),
             tabPanel("Unbiasedness"),
             tabPanel("Uncorrelated Deviations"),
             tabPanel("Homogenity of Standard Deviations")
           )
  )
}

test_panel_server <- function(id, analysis_data = NULL) {
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
      
      output$mvn_p_values_table <- renderTable({
        p_values_table <- NULL
        test_types <- c("Royston", "Henze-Zirkler", "Doornik-Hansen", "Energy")
        test_shortcuts <- c("royston", "hz", "dh", "energy")
        for (test in test_shortcuts) {
          p_values_table <- p_values_table %>%
            bind_cols(test = mvn(centered_analysis_data() %>% select(x_centered, y_centered), mvnTest = test, covariance = FALSE)$multivariateNormality["p value"])
        }
        names(p_values_table) <- test_types
        p_values_table
      })
    }
  )
}