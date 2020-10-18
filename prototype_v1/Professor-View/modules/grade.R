homework_UI <- function(id) {
  tabPanel(title = "Homework"
           , fluidRow(
             box(width = 12
                 , box(width = 12
                       , title = "Homework Grades"
                       , rHandsontableOutput(NS(id, "homework_table"))
                 )
                 , actionBttn(NS(id, "save"), "Save", style = "material-flat", block = T)
             )
           )
  )
}

homework_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    
    
    output$homework_table <- renderRHandsontable({
      df_homework_table <- r$df_homework_table 
      rhandsontable(df_homework_table
                    , rowHeaders = NULL) %>%
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
                    , tabPanel(title = "By Student"
                               , box(width = 12
                                     , title = "Review Grade by Student"
                                     , rHandsontableOutput(NS(id, "review_table_student"))
                                    
                               )
                               , actionBttn(NS(id, "saveStudent"), "Save", style = "material-flat", block = T)
                    )
                    , tabPanel(title = "By Topic"
                               , box(width = 12
                                     , title = "Review Grade by Topic"
                                 
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
    #TODO: Think about filtering by review? ----
    output$review_table_student <- renderRHandsontable({
      grade_types <- c("NA", "Fluent", "Getting There", "Needs Work")
      df_review_to_topic <- r$df_review_to_topic
      df_student <- r$df_student
      df_review <- df_review_to_topic %>%
        left_join(df_student, by = "student_id") %>%
        pivot_wider(id_cols = c(review_id, topic_id), names_from = name, values_from = grade) %>%
        rename(`Review ID` = review_id, `Topic ID` = topic_id)
      
      rhandsontable(df_review
                    , rowHeaders = NULL) %>%
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
      grade_types <- c("NA", "Fluent", "Getting There", "Needs Work")
      df_review_to_topic <- r$df_review_to_topic
      df_student <- r$df_student
      df_review_topic <- df_review_to_topic %>%
        left_join(df_student, by = "student_id") %>%
        pivot_wider(id_cols = c(review_id, name, topic_id)
                    , names_from = topic_id
                    , values_from = grade) %>%
        rename(`Review ID` = review_id, `Student Name` = name)
      # TODO: Conditional formatting for cells that are actually NA vs character of NA ----
      rhandsontable(df_review_topic
                    , rowHeaders = NULL) %>%
        hot_col(col = "Review ID", readOnly = T, type = "character") %>%
        hot_col(col = "Student Name", readOnly = T, type = "character") %>%
        hot_cols(type = "dropdown", source = grade_types)
    })  
  })
}