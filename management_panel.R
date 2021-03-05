management_panel_ui <- function(id, label = "management") {
  ns <- NS(id)
  tabPanel("Data management",
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("file1"), "Choose CSV File",
                  multiple = FALSE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
        actionButton(ns("delete_all_analysis"), label = "Delete all throws!", icon = icon("trash")),
        downloadButton(ns("download_analysis_data"), "Download throws as .csv")         
      ),
      mainPanel(
        h2("All data for analysis"),
        dataTableOutput(ns("analysis_data"))       
      )
    )
  )
}

management_panel_server <- function(id) {
  moduleServer(
    id, 
    function(input, output, session) { 
      analysis <- reactiveValues(
        data = tibble(
          row_num = numeric(),
          x_target = numeric(),
          y_target = numeric(),
          x_hit = numeric(),
          y_hit = numeric(),
          hit_result = character(),
          origin = character(),
          delete = character()
        ),
        counter = 0
      )
      
      output$analysis_data <- DT::renderDataTable({
        analysis$data %>%
          select(-row_num) %>%
          datatable(escape = FALSE) %>%
          formatRound(columns = c("x_target", "y_target", "x_hit", "y_hit"), digits = 3)
      }
      )
    }
  )
}