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
                   , title = textOutput(NS(id, "title"))
                   , echarts4rOutput(NS(id,'homework_bar'))
             )
           )
  )
}

homework_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    # Average Homework ----
    avg_homework_grade <- reactive({
      df <- r$df_homework_grades %>%
        select(-c(`Student Name`)) %>%
        na.omit()
      
      
      cols <- c(1:ncol(df))
      df[,cols] <- lapply(df[,cols], as.double)
      
      round(mean(as.matrix(df)))
      
    })
    
    output$title <- renderText(
      paste0("Average Homework Grade: ", avg_homework_grade(), "%")
    )
    # Homework Bar Chart ----
    output$homework_bar <- renderEcharts4r({
      df <- r$df_homework_grades %>%
        select(-c(`Student Name`)) %>%
        na.omit()
      
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
      
      chart <- df %>%
        e_chart(id) %>%
        e_bar(average, barWidth = "50%") %>%
        e_legend(show = F) %>%
        e_y_axis(formatter = e_axis_formatter("percent", digits = 2)) %>%
        e_color(color = "#c41230") %>%
        e_grid(left = "15%", right = "5%", top = "10%", bottom = "10%") %>%
        e_labels(position = "top", formatter  = htmlwidgets::JS("
                                    function(params){
                                      return(parseFloat(params.value[1]*100).toFixed(2) +'%');
                                    }
                                    ")) %>%
        e_tooltip()
      #e_color(color = rgb(196, 18, 48, alpha = 230, max = 255)) %>%
    })
    
    # Homework Table ----
    output$homework_table <- renderRHandsontable({
      # TODO: add catch for empty table
      df_homework_grades <- r$df_homework_grades 
      rhandsontable(df_homework_grades
                    , rowHeaders = NULL
                    , stretchH = 'all') %>%
        hot_heatmap(cols = c(2:ncol(df_homework_grades)))
    })  
    
    # Saving ----
    observeEvent(input$save,{
      df_hot <- hot_to_r(input$homework_table)
      r$df_homework_grades  <- df_hot
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
                    # Review by Review ----
                    , tabPanel(title = "By Review"
                               , box(width = 12, status = "primary"
                                     , title = "Review Grade by Review"
                                     #, uiOutput(NS(id, "review_picker"))
                                     , rHandsontableOutput(NS(id, "review_table_review"))
                                     
                               )
                               , actionBttn(NS(id, "saveReview"), "Save", style = "material-flat", block = T)
                    )
                    # Review by Student ----
                    , tabPanel(title = "By Student"
                               , box(width = 12, status = "primary"
                                     , title = "Review Grade by Student"
                                     #, uiOutput(NS(id, "student_picker"))
                                     , rHandsontableOutput(NS(id, "review_table_student"))
                               )
                               , actionBttn(NS(id, "saveStudent"), "Save", style = "material-flat", block = T)
                    )
             )
           )
  )
}

review_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    # Filters ----
    # ls_students <- reactive({
    #   students <- r$df_student %>%
    #     select(name) %>%
    #     pull()
    # })
    # 
    # ls_reviews <- reactive({
    #   students <- r$df_review %>%
    #     select(id) %>%
    #     pull()
    # })
    # 
    # output$student_picker <- renderUI({
    #   pickerInput(inputId = NS(id, "review_student_input")
    #               , label = "Filter by Student"
    #               , choices = c("All", ls_students())
    #   )
    # })
    # 
    # output$review_picker <- renderUI({
    #   pickerInput(inputId = NS(id, "review_review_input")
    #               , label = "Filter by Review"
    #               , choices = c("All", ls_reviews())
    #   )
    # })
    
    # Review by Review ----
    output$review_table_review <- renderRHandsontable({
      #req(input$review_review_input)
      browser()
      grade_types <- c("NA", "Not Completed", "Fluent", "Progressing", "Needs Work")
      df_review_grades <- r$df_review_grades
      df_student <- r$df_student
      df_review <- df_review_grades %>%
        left_join(df_student, by = "student_id") %>%
        pivot_wider(id_cols = c(review_id, topic_id), names_from = name, values_from = grade) %>%
        rename(`Review ID` = review_id, `Topic ID` = topic_id)
      
      # browser()
      # if (input$review_review_input == "All"){
      #   df_review <- df_review %>%
      #     group_by(`Review ID`) %>%
      #     arrange(`Review ID`)
      # }
      # else{
      #   df_review <- df_review %>%
      #     filter(`Review ID` == input$review_review_input)
      # }
      df_review <- df_review %>%
        group_by(`Review ID`) %>%
        arrange(`Review ID`)
      
      rhandsontable(df_review
                    , rowHeaders = NULL
                    , stretchH = 'all') %>%
        hot_col(col = "Review ID", readOnly = T) %>%
        hot_col(col = "Topic ID", readOnly = T) %>%
        hot_cols(type = "dropdown", source = grade_types)%>%
        hot_cols(renderer = "
        function(instance, td, row, col, prop, value, cellProperties) {
          Handsontable.renderers.TextRenderer.apply(this, arguments);
           if (value == 'Fluent'){
          td.style.background = 'lightgreen';
          } 
          else if (value == 'Progressing'){
          td.style.background = 'lightyellow';
          } else if (value == 'Needs Work'){
          td.style.background = 'pink';
          } else if (value == 'Not Completed'){
          td.style.background = 'lightgrey';
          } 
        }
          ")
    })  
    
    # Saving
    observeEvent(input$saveReview,{
      #browser()
      # if (input$review_review_input == "All"){
      #   df_student <- r$df_student
      #   df_review_to_topic <- r$df_review_to_topic
      #   df_hot <- hot_to_r(input$review_table_review)
      #   df_temp <- df_hot %>%
      #     pivot_longer(cols = c(3:ncol(df_hot))) %>%
      #     left_join(df_student, by = "name") %>%
      #     select(review_id = `Review ID`, topic_id = `Topic ID`, student_id, grade = value)
      # }
      # else{
      #   df_review <- df_review %>%
      #     filter(`Review ID` == input$review_review_input)
      # }
      # 
      df_student <- r$df_student
      df_review_grades <- r$df_review_grades
      df_hot <- hot_to_r(input$review_table_review)
      df_temp <- df_hot %>%
        pivot_longer(cols = c(3:ncol(df_hot))) %>%
        left_join(df_student, by = "name") %>%
        select(review_id = `Review ID`, topic_id = `Topic ID`, student_id, grade = value)
      r$df_review_grades <- df_temp
      sheet_write(
        ss =  "https://docs.google.com/spreadsheets/d/1xIC4pGhnnodwxqopHa45KRSHIVcOTxFSfJSEGPbQH20/edit#gid=2102408290"
        , data = df_temp
        , sheet = "review_to_topic"
      )
      showNotification("Saved to remote.")
    })
    
    # Review by Student ----
    output$review_table_student <- renderRHandsontable({
      # browser()
      #req(input$review_student_input)
      grade_types <- c("NA", "Not Completed", "Fluent", "Progressing", "Needs Work")
      df_review_grades <- r$df_review_grades
      df_student <- r$df_student
      df_review_topic <- df_review_grades %>%
        left_join(df_student, by = "student_id") %>%
        pivot_wider(id_cols = c(review_id, name, topic_id)
                    , names_from = topic_id
                    , values_from = grade) %>%
        rename(`Review ID` = review_id, `Student Name` = name)
      
      # if (input$review_student_input == "All"){
      #   df_review_topic <- df_review_topic %>%
      #     group_by(`Student Name`) %>%
      #     arrange(`Student Name`)
      # }
      # else{
      #   df_review_topic <- df_review_topic %>%
      #     filter(`Student Name` == input$review_student_input)
      # }
      df_review_topic <- df_review_topic %>%
        group_by(`Student Name`) %>%
        arrange(`Student Name`) 
      
      rhandsontable(df_review_topic
                    , rowHeaders = NULL
                    , stretchH = 'all'
                    , options = c(filters = T)) %>%
        hot_col(col = "Review ID", readOnly = T, type = "character") %>%
        hot_col(col = "Student Name", readOnly = T, type = "character") %>%
        hot_cols(type = "dropdown", source = grade_types)  %>%
        hot_cols(renderer = "
        function(instance, td, row, col, prop, value, cellProperties) {
          Handsontable.renderers.TextRenderer.apply(this, arguments);
          if (value == 'Fluent'){
          td.style.background = 'lightgreen';
          } 
          else if (value == 'Progressing'){
          td.style.background = 'lightyellow';
          } else if (value == 'Needs Work'){
          td.style.background = 'pink';
          } else if (value == 'Not Completed'){
          td.style.background = 'lightgrey';
          } 
          
          if (col > 1){
                 if (!isNaN(value)) {
          td.style.background = 'grey';
          cellProperties.readOnly = true;
          }
          }

        }")
      
    })  
    
    # Saving
    observeEvent(input$saveStudent,{
      if (input$review_student_input == "All"){
        df_student <- r$df_student
        df_review_grades <- r$df_review_grades
        df <- hot_to_r(input$review_table_review)
        df_temp <- df %>%
          pivot_longer(cols = c(3:ncol(df))) %>%
          left_join(df_student, by = "name") %>%
          select(review_id = `Review ID`, topic_id = `Topic ID`, student_id, grade = value)
        
      }
      else{
        df_review_topic <- df_review_topic %>%
          filter(`Student Name` == input$review_student_input)
      }
      
      r$df_review_grades <- df_temp
      sheet_write(
        ss =  "https://docs.google.com/spreadsheets/d/1xIC4pGhnnodwxqopHa45KRSHIVcOTxFSfJSEGPbQH20/edit#gid=2102408290"
        , data = df_temp
        , sheet = "review_to_topic"
      )
      showNotification("Saved to remote.")
    })
  })
}