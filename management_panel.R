management_panel_ui <- function(id, label = "management") {
  ns <- NS(id)
  tabPanel("Data management",
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("dart_throws_file"), "Upload saved dart throws",
                  multiple = FALSE,
                  accept = ".csv"),
        actionButton(ns("upload_file_button"), label = "Upload throws", icon = icon("upload")),
        tags$hr(),
        actionButton(ns("delete_all_analysis"), label = "Delete all throws!", icon = icon("trash")),
        downloadButton(ns("download_analysis_data"), "Download throws as .csv")         
      ),
      mainPanel(
        h2("All data currently loaded for analysis"),
        dataTableOutput(ns("analysis_data"))       
      )
    )
  )
}

management_panel_server <- function(id, click_send_to_analysis = NULL, data_collected = NULL) {
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
          delete = character(),
          cat_target = character()
        ),
        counter = 0,
        upload_counter = 0,
        collect_counter = 0
      )
      
      # "Upload Throws" button
      # TODO: handle case of upload of data already in the analysis data set.
      observeEvent(input$upload_file_button, {
        if (is.null(input$dart_throws_file)) {
          showModal(modalDialog(title = "Please choose a file.",
                                "You haven't selected a file for upload, yet.",
                                footer = modalButton("Got it.")))
        } else {
          new_throws <- read_csv(input$dart_throws_file$datapath)
          no_new_throws <- nrow(new_throws)
          new_throws <- new_throws %>%
            mutate(
              row_num = (analysis$counter + 1):(analysis$counter + no_new_throws),
              origin = paste0("upload_", analysis$upload_counter + 1),
              delete = map_chr(.x = row_num,  ~get_delete_button("delete_analysis", NS(id), "delete_analysis_pressed",.x))#,
              #cat_target = cat_target(x_target, y_target)
            )
          analysis$data <- bind_rows(
            analysis$data,
            new_throws
          )
          analysis$counter <- analysis$counter + no_new_throws
          analysis$upload_counter <- analysis$upload_counter + 1
        }
      })
      
      # TODO: handle case of empty data_collected DF
      # TODO: Handle case of throws coming in the second time
      observeEvent(click_send_to_analysis(),{
        if (nrow(data_collected()) == 0) {
          showModal(modalDialog(title = "Please collect some throws.",
                                "There are no data on throws to be sent to the analysis section, yet.",
                                footer = modalButton("Got it.")))
        } else {
          # get values from collection data
          new_throws <- data_collected() %>%
            select(-c(row_num, delete))
          no_new_throws <- nrow(new_throws)
          
          # add new data to analysis data
          new_throws <- new_throws %>%
            mutate(
              row_num = (analysis$counter + 1):(analysis$counter + no_new_throws),
              origin = paste0("collection_", analysis$collect_counter + 1),
              delete = map_chr(.x = row_num,  ~get_delete_button("delete_analysis", NS(id), "delete_analysis_pressed",.x))#,
              #cat_target = cat_target(x_target, y_target)
            )
          analysis$data <- bind_rows(
            analysis$data,
            new_throws
          )
          
          # increment counters
          analysis$counter <- analysis$counter + no_new_throws
          analysis$collect_counter <- analysis$collect_counter + 1
        }
      })
      
      # "Delete row" buttons for analysis data
      observeEvent(input$delete_analysis_pressed, {
        analysis$data <- analysis$data %>%
          filter(row_num != parse_delete_event(input$delete_analysis_pressed))
      })
      
      # "Delete all throws" button
      observeEvent(input$delete_all_analysis, {
        analysis$data <- analysis$data %>% filter(FALSE)
      })
      
      # Download .csv file
      output$download_analysis_data <- downloadHandler(
        filename = function() {
          "dart_throws.csv"
        },
        content = function(file) {
          analysis$data %>%
            select(-c(row_num, delete)) %>%
            write_csv(file)
        }
      )
      
      # Output table: current analysis data
      output$analysis_data <- DT::renderDataTable({
        analysis$data %>%
          select(-row_num, -cat_target) %>%
          datatable(escape = FALSE, options = list(pageLength = 100)) %>%
          formatRound(columns = c("x_target", "y_target", "x_hit", "y_hit"), digits = 3)
      }
      )
      
      return(reactive({analysis$data}))
    }
  )
}