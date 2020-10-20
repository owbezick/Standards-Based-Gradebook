homework_UI <- function(id) {
  tabPanel(title = "Homework"
           , fluidRow(
                 box(width = 12, status = "primary"
                       , title = "Homework Grades"
                       , rHandsontableOutput(NS(id, "homework_table"))
                       , br()
                       , actionBttn(NS(id, "save"), "Save", style = "material-flat", block = T)
                 )
                 , br()
                 , box(width = 12, status = "primary"
                       , title = "Average Homework Grade"
                       , echarts4rOutput(NS(id,'homework_bar'))
                 )
             )
  )
}

homework_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    
    output$homework_bar <- renderEcharts4r({
      df <- r$df_homework_table %>%
        select(-c(`Student Name`))
      
      cols <- c(1:ncol(df))
      df[,cols] <- lapply(df[,cols], as.double)
      
      df <- colMeans(df) %>%
        as.matrix()
      
      df <- df[,1] %>%
        as.data.frame() %>%
        rename(Average = ".") %>%
        mutate(average = Average / 100) %>%
        mutate(Chart = "chart") %>%
        select(average, chart = Chart)
      
      df$id <- rownames(df)
        
      df %>%
        e_chart(id) %>%
        e_bar(average, barWidth = "50%") %>%
        e_legend(show = F) %>%
        e_y_axis(formatter = e_axis_formatter("percent", digits = 2)) %>%
        e_labels(position = "top", formatter  = htmlwidgets::JS("
                                    function(params){
                                      return(parseFloat(params.value[1]*100).toFixed(2) +'%');
                                    }
                                    ")) %>%
        #e_color(color = rgb(196, 18, 48, alpha = 230, max = 255)) %>%
        e_color(color = "#c41230") %>%
        e_grid(left = "15%", right = "5%", top = "10%", bottom = "10%")
      
    })
    
    output$homework_table <- renderRHandsontable({
      df_homework_table <- r$df_homework_table 
      rhandsontable(df_homework_table
                    , rowHeaders = NULL
                    , stretchH = 'all') %>%
        hot_heatmap() 
    })  
    
    observeEvent(input$save,{
      df_hot <- hot_to_r(input$homework_table)
      r$df_homework_table  <- df_hot
      sheet_write(
        ss =  "https://docs.google.com/spreadsheets/d/1xIC4pGhnnodwxqopHa45KRSHIVcOTxFSfJSEGPbQH20/edit#gid=2102408290"
        , data = df_hot
        , sheet = "homework_table"
      )
      showNotification("Saved to remote.")
    })
    
  }) #end module server
}

review_UI <- function(id) {
  tabPanel(title = "Reviews"
           , fluidRow(
             tabBox(width = 12
                    , tabPanel(title = "By Review"
                               , box(width = 12, status = "primary"
                                     , title = "Review Grade by Review"
                                     , uiOutput(NS(id, "review_picker"))
                                     , rHandsontableOutput(NS(id, "review_table_student"))
                                    
                               )
                               , actionBttn(NS(id, "saveStudent"), "Save", style = "material-flat", block = T)
                    )
                    , tabPanel(title = "By Student"
                               , box(width = 12, status = "primary"
                                     , title = "Review Grade by Student"
                                     , uiOutput(NS(id, "student_picker"))
                                     , rHandsontableOutput(NS(id, "review_table_topic"))
                               )
                               , actionBttn(NS(id, "saveTopic"), "Save", style = "material-flat", block = T)
                    )
             )
           )
  )
}

review_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    ls_students <- reactive({
      students <- r$df_student %>%
        select(name) %>%
        pull()
    })
    
    ls_reviews <- reactive({
      students <- r$df_review %>%
        select(id) %>%
        pull()
    })
    
    output$student_picker <- renderUI({
      pickerInput(inputId = NS(id, "review_student_input")
                  , label = "Filter by Student"
                  , choices = c("All", ls_students())
      )
    })
    
    output$review_picker <- renderUI({
      pickerInput(inputId = NS(id, "review_review_input")
                  , label = "Filter by Review"
                  , choices = c("All", ls_reviews())
      )
    })
    
    
    #TODO: Think about filtering by review? ----
    output$review_table_student <- renderRHandsontable({
      grade_types <- c("NA", "NC", "Fluent", "Getting There", "Needs Work")
      df_review_to_topic <- r$df_review_to_topic
      df_student <- r$df_student
      df_review <- df_review_to_topic %>%
        left_join(df_student, by = "student_id") %>%
        pivot_wider(id_cols = c(review_id, topic_id), names_from = name, values_from = grade) %>%
        rename(`Review ID` = review_id, `Topic ID` = topic_id)
      
      #browser()
      # Filter by picker input
      if (input$review_review_input == "All"){
        df_review <- df_review %>%
          group_by(`Review ID`) %>%
          arrange(`Review ID`)
      }
      else{
        df_review <- df_review %>%
          filter(`Review ID` == input$review_review_input)
      }
      
      rhandsontable(df_review
                    , rowHeaders = NULL
                    , stretchH = 'all') %>%
        hot_col(col = "Review ID", readOnly = T) %>%
        hot_col(col = "Topic ID", readOnly = T) %>%
        hot_cols(type = "dropdown", source = grade_types)
    })  
    
    # Saving
    observeEvent(input$saveStudent,{
      df_student <- r$df_student
      df_review_to_topic <- r$df_review_to_topic
      df <- hot_to_r(input$review_table_student)
      df_temp <- df %>%
        pivot_longer(cols = c(3:ncol(df))) %>%
        left_join(df_student, by = "name") %>%
        select(review_id = `Review ID`, topic_id = `Topic ID`, student_id, grade = value)
      r$df_review_to_topic <- df_temp
      sheet_write(
        ss =  "https://docs.google.com/spreadsheets/d/1xIC4pGhnnodwxqopHa45KRSHIVcOTxFSfJSEGPbQH20/edit#gid=2102408290"
        , data = df_temp
        , sheet = "review_to_topic"
      )
      showNotification("Saved to remote.")
    })
    
    output$review_table_topic <- renderRHandsontable({
      req(input$review_student_input)
      grade_types <- c("NA", "Fluent", "Getting There", "Needs Work")
      df_review_to_topic <- r$df_review_to_topic
      df_student <- r$df_student
      df_review_topic <- df_review_to_topic %>%
        left_join(df_student, by = "student_id") %>%
        pivot_wider(id_cols = c(review_id, name, topic_id)
                    , names_from = topic_id
                    , values_from = grade) %>%
        rename(`Review ID` = review_id, `Student Name` = name)
      
      # Filter by picker input
      if (input$review_student_input == "All"){
        df_review_topic <- df_review_topic %>%
          group_by(`Student Name`) %>%
          arrange(`Student Name`)
      }
      else{
        df_review_topic <- df_review_topic %>%
          filter(`Student Name` == input$review_student_input)
      }
      
      # TODO: Conditional formatting for cells that are actually NA vs character of NA ----
      rhandsontable(df_review_topic
                    , rowHeaders = NULL
                    , stretchH = 'all') %>%
        hot_col(col = "Review ID", readOnly = T, type = "character") %>%
        hot_col(col = "Student Name", readOnly = T, type = "character") %>%
        hot_cols(type = "dropdown", source = grade_types)
    })  
    # Saving
    observeEvent(input$saveTopic,{
      df_student <- r$df_student
      df_review_to_topic <- r$df_review_to_topic
      df <- hot_to_r(input$review_table_student)
      df_temp <- df %>%
        pivot_longer(cols = c(3:ncol(df))) %>%
        left_join(df_student, by = "name") %>%
        select(review_id = `Review ID`, topic_id = `Topic ID`, student_id, grade = value)
      r$df_review_to_topic <- df_temp
      sheet_write(
        ss =  "https://docs.google.com/spreadsheets/d/1xIC4pGhnnodwxqopHa45KRSHIVcOTxFSfJSEGPbQH20/edit#gid=2102408290"
        , data = df_temp
        , sheet = "review_to_topic"
      )
      showNotification("Saved to remote.")
    })
  })
}