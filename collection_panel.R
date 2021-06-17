collection_panel_ui <- function(id, label = "collection") {
  ns <- NS(id)
  tabPanel("Data collection",
    sidebarLayout(
      sidebarPanel(
        radioButtons(
          ns("point_type"),
          label = "Kind of point to mark",
          choices = c("Target point", "Hit point")
        ),
        actionButton(ns("reset_target"), label = "Reset target point to center!"),
        verbatimTextOutput(ns("current_choices")),
        actionButton(ns("save_throw"), label = "Save throw!")
      ),
      mainPanel(
        plotOutput(ns("dart_board"), click = ns("board_click"))
      )
    ),
    sidebarLayout(
      sidebarPanel(
        actionButton(ns("delete_all"), label = "Delete all throws!", icon = icon("trash")),
        actionButton(ns("send_to_analysis"), "Send throws to analysis section"),
        downloadButton(ns("download_throws"), "Download throws as .csv")
      ),
      mainPanel(
        dataTableOutput(ns("data_collected"))
      )
    )
  )
}
collection_panel_server <- function(id) {
  moduleServer(
    id, 
    function(input, output, session) {                                        
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
          row_num = numeric(),
          x_target = numeric(),
          y_target = numeric(),
          x_hit = numeric(),
          y_hit = numeric(),
          hit_result = character(),
          cat_target = character(),
          delete = character()
        ),
        counter = 0
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
      
      # "Reset target" button
      observeEvent(input$reset_target, {
        board$points[board$points$type == "target", c("x", "y", "cat_result")] <-list(x = 0, y = 0, cat_result = "Bulls Eye")
      })
      
      # add throw to collected_data, when respective button is hit
      observeEvent(input$save_throw, {
        data$counter <- data$counter + 1
        new_row <- tibble(
          row_num = data$counter,
          x_target = board$points %>% filter(type == "target") %>% select(x) %>% pull(),
          y_target = board$points %>% filter(type == "target") %>% select(y) %>% pull(),
          x_hit = board$points %>% filter(type == "hit") %>% select(x) %>% pull(),
          y_hit = board$points %>% filter(type == "hit") %>% select(y) %>% pull(),
          hit_result = board$points %>% filter(type == "hit") %>% select(cat_result) %>% pull(),
          delete = get_delete_button("delete_collect", NS(id), "deletePressed", data$counter)
        ) %>%
          mutate(cat_target = cat_target(x_target, y_target))
        
        data$collected <- data$collected %>%
          bind_rows(new_row)
      })
      
      # show board_plot including current target and hit point
      output$dart_board <- renderPlot({
        raw_throws_plot_without_mean(data$collected, alpha = 0.5) +
          new_scale("color") +
          geom_point(data = board$points,
                     mapping = aes(x = x, y = y, color = type, shape = type),
                     size = 3,
                     stroke = 2) +
          scale_shape_manual(values = c(4, 3)) +
          scale_color_manual(values = c("red", "darkgreen"))
      })
      
      # information about current choices of hit and target points
      # as cartesian coordinates
      output$current_choices <- renderText({
        x_target <- board$points %>% filter(type == "target") %>% select(x) %>% pull() %>% round(3)
        y_target <- board$points %>% filter(type == "target") %>% select(y) %>% pull() %>% round(3)
        x_hit <- board$points %>% filter(type == "hit") %>% select(x) %>% pull() %>% round(3)
        y_hit <- board$points %>% filter(type == "hit") %>% select(y) %>% pull() %>% round(3)
        cat_target <- board$points %>% filter(type == "target") %>% select(cat_result) %>% pull()
        cat_hit <- board$points %>% filter(type == "hit") %>% select(cat_result) %>% pull()
        
        paste0(
          "Current target:\n",
          "x: ", x_target, ", y: ", y_target, " (", cat_target, ")\n",
          "Current hit point:\n",
          "x: ", x_hit, ", y: ", y_hit, " (", cat_hit, ")"
        )
      })
      
      # "Delete Row" button
      observeEvent(input$deletePressed, {
        data$collected <- data$collected %>%
          filter(row_num != parse_delete_event(input$deletePressed))
      })
      
      # show table of data collected up to this point
      output$data_collected <- DT::renderDataTable({
        data$collected %>%
          select(-row_num, -cat_target) %>%
          datatable(escape = FALSE) %>%
          formatRound(columns = c("x_target", "y_target", "x_hit", "y_hit"), digits = 3)
      }
      )
      
      # "Delete all throws" button
      observeEvent(input$delete_all, {
        data$collected <- data$collected %>% filter(FALSE)
      })
      
      # Download .csv file
      output$download_throws <- downloadHandler(
        filename = function() {
          "dart_throws.csv"
        },
        content = function(file) {
          data$collected %>%
            select(-c(row_num, delete)) %>%
            write_csv(file)
        }
      )
      
      # return values for handling "send_to_analysis" button in management module
      return(
        list(send = reactive({ input$send_to_analysis }),
             throws = reactive({ data$collected }))
      )
    }
  )
}  