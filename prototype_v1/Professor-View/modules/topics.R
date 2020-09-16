topics_UI <- function(id){
  box(width = 12, title = "Course Topics", status = "primary"
    , formattableOutput(NS(id, "topicTable"))
  )
}

# Add and edit topics here?
topics_server <- function(id, r){
  moduleServer(id, function(input,output,session){
    output$topicTable <- renderFormattable({
     # browser()
      
      df_exam_topics <- r$df_exam_to_topic %>%
        select(exam_id, topic_id)
      
      exam_ids <- unique(df_exam_topics[["exam_id"]]) # Get number of unique exam_id s
      
      ls_exam3_topics <- as.list(df_exam_topics %>%
                          filter(exam_id == 3) %>%
                          select(topic_id))
      
      
      topic_df <- r$df_topic %>%
        mutate(name = paste("Topic", id))
      ls_topic_table_columns <- c(topic_df$name)
      ls_topic_table_columns <- prepend(ls_topic_table_columns, "Date")
      ls_topic_table_columns <- prepend(ls_topic_table_columns, "Review")
      df <- data.frame(matrix(ncol = length(ls_topic_table_columns), nrow = length(exam_ids)), row.names = exam_ids)
      colnames(df) <- ls_topic_table_columns
      
      formattable(df)
    })
    
  })
}
