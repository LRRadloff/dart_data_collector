library("MVN")
library("infer")
library("MVTests")

test_panel_ui <- function(id, label = "testing") {
  ns <- NS(id)
  tabPanel("Statistical Testing",
           p(
             "Outcomes of statistical hypothesis tests for the assumptions made in
             the ",
             tags$a("dart_optimizer", href="https://lradloff.shinyapps.io/dart_optimizer/"),
             "are given. For each assumption one or more tests are performed 
             and p-values presented as their results. Note that all of those tests 
             rely on certain assumptions."
           ),
           tabsetPanel(
             tabPanel("Normality",
                "P-values for four different tests for multivariat normality are provided. Tests are performed using the ",
                tags$a("MVN", href="https://cran.r-project.org/web/packages/MVN/vignettes/MVN.pdf"),
                "library.", 
                tableOutput(ns("mvn_p_values_table"))         
             ),
             tabPanel("Unbiasedness",
                "P-values for t-tests for horizontal and vertical deviations, 
                respectively, as well as for a ",
                tags$a("Hotelling test", href = "https://www.rdocumentation.org/packages/MVTests/versions/2.0.3/topics/OneSampleHT2"),
                ". The latter tests for a multivariate mean having a particular 
                value. In each case it is tested whether the mean is zero.",
                tableOutput(ns("unbiased_p_value_table"))
             ),
             tabPanel("Uncorrelated Deviations",
                "P-value for a test on the correlation between vertical and horizontal
                deviations from target being zero. The test is based on a 
                permutation procedure using the ",
                tags$a("infer", href = "https://infer.netlify.app"),
                " library.",
                tableOutput(ns("zero_corr_p_value_table"))
             ),
             tabPanel("Homogenity of Standard Deviations",
             "P-Values for Levene-test are given. Levels are the different targets, 
             responses are vertical and horizontal deviations from target.",
             tableOutput(ns("var_homog_p_value_table"))
             )
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
            bind_cols(test = mvn(centered_analysis_data() %>% select(x_centered, y_centered), 
                                 mvnTest = test, covariance = FALSE)$multivariateNormality["p value"])
        }
        names(p_values_table) <- test_types
        p_values_table
      })
      
      output$unbiased_p_value_table <- renderTable({
        p_t_vertical <- t_test(centered_analysis_data(), response = y_centered, mu = 0)$p_value
        p_t_horizontal <- t_test(centered_analysis_data(), response = x_centered, mu = 0)$p_value
        p_hotelling <- OneSampleHT2(centered_analysis_data()[c("x_centered", "y_centered")],
                                    mu0 = c(0,0))$p.value
        bind_cols("t-test vertical" = p_t_vertical, 
                  "t-test horizontal" = p_t_horizontal,
                  "Hotelling" = p_hotelling)
      })
      
      output$zero_corr_p_value_table <- renderTable({
        corr_est <- centered_analysis_data() %>%
          summarize(cor(x_centered, y_centered)) %>%
          pull()
        centered_analysis_data() %>%
          specify(x_centered ~ y_centered) %>%
          hypothesize(null = "independence") %>%
          generate(reps = 1000, type = "permute") %>%
          calculate(stat = "correlation") %>%
          get_p_value(obs_stat = corr_est, direction = "two-sided")
      })
      
      output$var_homog_p_value_table <- renderTable({
        levene_transformed_data <- centered_analysis_data() %>% 
          group_by(cat_target) %>%
          mutate(x_transformed = abs(x_centered - mean(x_centered)),
                 y_transformed = abs(y_centered - mean(y_centered))) %>%
          ungroup() 
      
      horiz_F_hat <- levene_transformed_data %>%
        specify(x_transformed ~ cat_target) %>%
        calculate(stat = "F")
      horiz_p_value <- levene_transformed_data %>%
        specify(x_transformed ~ cat_target) %>%
        hypothesize(null = "independence") %>%
        generate(reps = 1000, type = "permute") %>%
        calculate(stat = "F") %>%
        get_p_value(obs_stat = horiz_F_hat, direction = "greater")
      
      vert_F_hat <- levene_transformed_data %>%
        specify(y_transformed ~ cat_target) %>%
        calculate(stat = "F")
      vert_p_value <- levene_transformed_data %>%
        specify(y_transformed ~ cat_target) %>%
        hypothesize(null = "independence") %>%
        generate(reps = 1000, type = "permute") %>%
        calculate(stat = "F") %>%
        get_p_value(obs_stat = vert_F_hat, direction = "greater")
      
      tibble("Vertical Deviations" = vert_p_value,
             "Horizontal Deviations" = horiz_p_value)
      })
    }
  )
}