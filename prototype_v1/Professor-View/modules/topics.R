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
    
    # Save topic table -----
    observeEvent(input$save, {
      df_hot <- hot_to_r(input$topicTable)
      # Save Review Table ----
      r$df_review_table <- df_hot
      sheet_write(
        ss =  "https://docs.google.com/spreadsheets/d/1xIC4pGhnnodwxqopHa45KRSHIVcOTxFSfJSEGPbQH20/edit#gid=2102408290"
        , data = df_hot
        , sheet = "review_table"
      )
      
      `%notin%` <- Negate(`%in%`)
      
      # Save review_grades ----
      df_review_grades <- r$df_review_grades
      
      
      df_student <- r$df_student %>%
        select(student_id)
      
      data_from_hot <- df_hot %>%
        pivot_longer(cols = c(4:ncol(df_hot))) %>%
        na.omit() 
      
      review_topic_id_hot <- data_from_hot %>%
        filter(value == "TRUE")
      
      review_topic_id_hot <- review_topic_id_hot %>%
        mutate(topic_id = str_split_fixed(review_topic_id_hot$name, " ", 2)[,2]) %>%
        select(review_id = `Review ID`, topic_id)
      
      number_of_topics <- nrow(review_topic_id_hot)
      # replicate review_id and topic_id for as many students that are in the class
      new_review_topic_rep <- do.call("rbind", replicate(nrow(df_student), review_topic_id_hot, simplify = FALSE))
      
      # replicate student_id the for as many topics being added
      student_id <- do.call("rbind", replicate(nrow(review_topic_id_hot), df_student, simplify = FALSE)) %>%
        arrange(student_id)
      
      df_new_review_data <- new_review_topic_rep %>%
        mutate(student_id = student_id$student_id
               , grade = rep("NA", nrow(new_review_topic_rep))
               , id = paste(review_id, topic_id, student_id)) 
      df_review_grades <- df_review_grades %>%
        mutate(id = paste(review_id, topic_id, student_id))
      
      df_new_review_data <- subset(df_new_review_data, id != df_review_grades$id) %>%
        select(-c(id))
      df_review_grades <- df_review_grades %>%
        select(-c(id))
      temp <- rbind(df_review_grades, df_new_review_data) %>%
        arrange(review_id, topic_id)
      
      
      # Refresh and save data ----
      r$df_review_grades <- temp
      sheet_write(
        ss =  "https://docs.google.com/spreadsheets/d/1xIC4pGhnnodwxqopHa45KRSHIVcOTxFSfJSEGPbQH20/edit#gid=2102408290"
        , data = temp
        , sheet = "review_grades"
      )
      
      
      
      showNotification("Saved to remote.")
    })
    # data_from_hot <- df %>%
    #   pivot_longer(cols = c(4:ncol(df))) %>%
    #   na.omit()
    # 
    # data_from_hot <- data_from_hot %>%
    #   mutate(topic_id = str_split_fixed(data_from_hot$name, " ", 2)[,2]) %>%
    #   select(review_id = `Review ID`, topic_id)
    # 
    # ls_distinct_new_review_id <- data_from_hot %>%
    #   select(review_id) %>%
    #   distinct() %>%
    #   pull()
    # 
    # 
    # ls_distinct_old_review_id <-df_review_to_topic %>%
    #   select(review_id) %>%
    #   distinct() %>%
    #   pull()
    # 
    # # Check for any deletions, and remove
    # temp <- subset(df_review_to_topic, review_id %in% ls_distinct_new_review_id)
    # 
    # # Check if topics in reviews have changed 
    # prev_review_topic_id <- temp %>%
    #   group_by(review_id, topic_id) %>%
    #   distinct(topic_id) %>%
    #   ungroup() %>%
    #   group_by(review_id)
    # 
    # new_review_topic_id <- data_from_hot %>%
    #   group_by(review_id)
    # 
    # new_review_topic_id <- subset(new_review_topic_id, topic_id %notin% prev_review_topic_id$topic_id)
    # 
    # if (nrow(new_review__topic_id) > 0){
    #   # Replicate for each student
    #   new_review__topic_rep <- do.call("rbind", replicate(nrow(df_student_id), new_review_topic_id, simplify = FALSE))
    # }
    # 
    # 
    # # replicate student_id the for as many topics being added
    # student_id <- do.call("rbind", replicate(nrow(new_review_topic_id), df_student_id, simplify = FALSE)) %>%
    #   arrange(student_id)
    # 
    # df_new_review_data <- new_review_rep %>%
    #   mutate(student_id = student_id$student_id
    #          , grade = rep("NA", nrow(new_review_rep)))
    # 
    # 
    # temp <- rbind(temp, df_new_review_data) %>%
    #   arrange(review_id, topic_id)
    #                            
    # # Check for any new reviews
    # new_review_data <- subset(data_from_hot, review_id %notin% ls_distinct_old_review_id)
    # if(nrow(new_review_data) > 0){
    #   # Add in any new reviews with student grades as NA
    #   df_student_id <- r$df_student %>%
    #     select(student_id) 
    #   
    #   # replicate review_id and topic_id for as many students that are in the class
    #   new_review_rep <- do.call("rbind", replicate(nrow(df_student_id), new_review_data, simplify = FALSE))
    #   
    #   # replicate student_id the for as many topics being added
    #   student_id <- do.call("rbind", replicate(nrow(new_review_data), df_student_id, simplify = FALSE)) %>%
    #     arrange(student_id)
    #   
    #   df_new_review_data <- new_review_rep %>%
    #     mutate(student_id = student_id$student_id
    #            , grade = rep("NA", nrow(new_review_rep)))
    #   
    #   
    #   temp <- rbind(temp, df_new_review_data) %>%
    #     arrange(review_id, topic_id)
    # }
    
    
    
  })
}
