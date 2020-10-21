topics_UI <- function(id){
  box(width = 12, title = "Review Table", status = "primary"
            , rHandsontableOutput(NS(id, "topicTable"))
            , footer = "Right click to add a row to the table."
      
      , actionBttn(NS(id, "save"), "Save", style = "material-flat", block = T)
      
  )
}

topics_server <- function(id, r){
  moduleServer(id, function(input,output,session){
    # Topic Table Output ----
    output$topicTable <- renderRHandsontable({
      df <- r$df_review_table
      df <- df %>%
        mutate(`Review Date` = ymd(`Review Date`)
               , `Review ID` = as.character(`Review ID`))
      rhandsontable(
        df
        , rowHeaders = NULL
        , stretchH = 'all'
      ) %>%
        hot_context_menu(allowComments = T)
    })
    
    
    
    observeEvent(input$save, {
      df <- hot_to_r(input$topicTable)
      # Review Table ----
      r$df_review_table <- df
      sheet_write(
        ss =  "https://docs.google.com/spreadsheets/d/1xIC4pGhnnodwxqopHa45KRSHIVcOTxFSfJSEGPbQH20/edit#gid=2102408290"
        , data = df
        , sheet = "review_table"
      )
      
      `%notin%` <- Negate(`%in%`)
      # Review grades ----
      df_review_to_topic <- r$df_review_to_topic
      
      data_from_hot <- df %>%
        pivot_longer(cols = c(4:ncol(df))) %>%
        na.omit()
      
      data_from_hot <- data_from_hot %>%
        mutate(topic_id = str_split_fixed(data_from_hot$name, " ", 2)[,2]) %>%
        select(review_id = `Review ID`, topic_id)
      
      ls_distinct_new_review_id <- data_from_hot %>%
        select(review_id) %>%
        distinct() %>%
        pull()
      
      
      ls_distinct_old_review_id <-df_review_to_topic %>%
        select(review_id) %>%
        distinct() %>%
        pull()
      
      # Check for any deletions, and remove
      temp <- subset(df_review_to_topic, review_id %in% ls_distinct_new_review_id)
      # Check for any additions
      new_review_data <- subset(data_from_hot, review_id %notin% ls_distinct_old_review_id)
      if(nrow(new_review_data) > 0){
        # Add in any new reviews with student grades as NA
        df_student_id <- r$df_student %>%
          select(student_id) 
        
        # replicate review_id and topic_id for as many students that are in the class
        new_review_rep <- do.call("rbind", replicate(nrow(df_student_id), new_review_data, simplify = FALSE))
        
        # replicate student_id the for as many topics being added
        student_id <- do.call("rbind", replicate(nrow(new_review_data), df_student_id, simplify = FALSE)) %>%
          arrange(student_id)
        
        df_new_review_data <- new_review_rep %>%
          mutate(student_id = student_id$student_id
                 , grade = rep("NA", nrow(new_review_rep)))
        
        
        temp <- rbind(temp, df_new_review_data) %>%
          arrange(review_id, topic_id)
      }
      
      # Refresh and save data ----
      r$df_review_to_topic <- temp
      sheet_write(
        ss =  "https://docs.google.com/spreadsheets/d/1xIC4pGhnnodwxqopHa45KRSHIVcOTxFSfJSEGPbQH20/edit#gid=2102408290"
        , data = df
        , sheet = "review_grades"
      )
      
      showNotification("Saved to remote.")
    })
    
  })
}
