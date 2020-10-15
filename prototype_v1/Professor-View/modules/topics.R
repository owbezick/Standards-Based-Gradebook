topics_UI <- function(id){
  box(width = 12, title = "Review Table", status = "primary"
      , "Right click to add a row to the table."
      , actionBttn(NS(id, "save"), "Save")
      , rHandsontableOutput(NS(id, "topicTable"))
  )
}

topics_server <- function(id, r){
  moduleServer(id, function(input,output,session){
    # course_table <- reactive({
    #  
    #   df_review <- r$df_review
    #   df_review_to_topic <- r$df_review_to_topic
    #   # Make table headers
    #   df_topic <- r$df_topic
    #   df_topic <- df_topic %>%
    #     mutate(name = paste("Topic", id))
    #   
    #   # Populate table using review_to_topic table
    #   df_review_to_topic <- df_review_to_topic %>%
    #     group_by(review_id, topic_id) %>%
    #     summarise() %>%
    #     left_join(df_review, by = c("review_id" = "id"))
    #   
    #   df <- data.frame(
    #     matrix(
    #       ncol = length(df_topic$id) + 2
    #       , nrow = length(unique(df_review_to_topic$review_id))
    #     )
    #     , row.names = r$review$id
    #     , stringsAsFactors = T
    #   )
    #   
    #   colnames(df) <- c("Review", "Date", df_topic$name)
    #   
    #   for (exam_id in unique(df_review_to_topic$review_id)){
    #     current_exam <- filter(df_review_to_topic, review_id == exam_id)
    #     ls_topics_in_current_exam <- current_exam$topic_id
    #     date <- current_exam[1, "date"] %>% pull()
    #     df[exam_id, "Date"] = as.character(date)
    #     df[exam_id, "Review"] = paste("Review", current_exam[1, "review_id"])
    #     for (id in ls_topics_in_current_exam){
    #       df[exam_id, paste("Topic", id)] = TRUE
    #     }
    #   }
    #   df
    # })
    
  
    
    output$topicTable <- renderRHandsontable({
      df <- r$df_review_table
      df <- df %>%
        mutate(`Review Date` = ymd(`Review Date`))
      rhandsontable(
        df
        , rowHeaders = NULL
        ) %>%
        hot_context_menu(allowComments = T)
    })
    
    observeEvent(input$save, {
      df <- hot_to_r(input$topicTable)
      r$df_review_table <- df
      sheet_write(
        ss =  "https://docs.google.com/spreadsheets/d/1xIC4pGhnnodwxqopHa45KRSHIVcOTxFSfJSEGPbQH20/edit#gid=2102408290"
        , data = df
        , sheet = "review_table"
      )
      showNotification("Saved to remote.")
    })
    
  })
}
