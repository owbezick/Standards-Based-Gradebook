topics_UI <- function(id){
  box(width = 12, title = "Course Topics", status = "primary"
    , formattableOutput(NS(id, "topicTable"))
  )
}

# Add and edit topics here?
topics_server <- function(id, r){
  moduleServer(id, function(input,output,session){
    output$topicTable <- renderFormattable({
      topic_df <- r$df_topic %>%
        mutate(name = paste("Topic", id))
      ls_topic_table_columns <- c(topic_df$name)
      ls_topic_table_columns <- prepend(ls_topic_table_columns, "Date")
      ls_topic_table_columns <- prepend(ls_topic_table_columns, "Review")
      df <- data.frame(matrix(ncol = length(ls_topic_table_columns), nrow = 1))
      colnames(df) <- ls_topic_table_columns
      formattable(df)
    })
  })
}