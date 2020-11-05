
# Course information button module ----
add_topic_button_UI <- function(id) {
  showModal(
    modalDialog(title = "Edit Topics", size = "l"
                , column(width = 12
                         , fluidRow(
                           rHandsontableOutput(NS(id, "edit_topics"))
                         )
                         , br()
                         , fluidRow(
                           column(width = 6
                                  , actionBttn(
                                    inputId = NS(id,"save_edits")
                                    , label = "Save Edits"
                                    , style = "material-flat"
                                    , block = T
                                  )
                           )
                           , column(width = 6
                                    , actionBttn(
                                      inputId = NS(id, "close_edits")
                                      , label = "Close"
                                      , style = "material-flat"
                                      , block = T
                                    )
                           )
                         )
                         
                )
                , footer = fluidRow()
    )
  )
}

add_topic_button_Server <- function(id, r){
  moduleServer(id, function(input,output,session){
    
    # Topic Table Output ----
    output$edit_topics <- renderRHandsontable({
      df <- r$df_topic %>%
        arrange(topic_id) %>%
        select(`Topic ID` = topic_id, Description = description)
      
      rownames(df) <- NULL
      
      rhandsontable(
        df
        , rowHeaders = NULL
        , stretchH = 'all'
      )%>%
        hot_col(col = "Topic ID", type = "numeric")
    })
    
    # BTN Save ----
    observeEvent(input$save_edits, {
      req(input$edit_topics)
      edit_topics_df <- hot_to_r(input$edit_topics) %>%
        select(topic_id = `Topic ID`, description = Description)
      # Save to df_topic
      if ( length(unique(edit_topics_df$topic_id)) != length(edit_topics_df$topic_id) ){
        showNotification("Please ensure all topic ID's are unique!", type = "warning")
      } else{
        # df_review_table
        # Remove deleted topics
        current_topics <- colnames(r$df_review_table)[5:ncol(r$df_review_table)]
        drops <- subset(current_topics, current_topics != paste("Topic", edit_topics_df$topic_id))
        r$df_review_table <- r$df_review_table[ ,!(names(r$df_review_table) %in% drops)]
        
        # New topics to Include
        current_topics <- colnames(r$df_review_table)[5:ncol(r$df_review_table)]
        df_topic_wide <- edit_topics_df %>%
          mutate(topic_name = paste("Topic", topic_id)) %>%
          filter(topic_name %notin% current_topics) %>%
          mutate(include = FALSE) %>%
          arrange(topic_id) %>%
          pivot_wider(id_cols = c(topic_id)
                      , names_from = topic_name
                      , values_from = include) %>%
          select(-c("topic_id")) %>%
          rownames_to_column() 
        
        # Add new topics - join by rownumber 
        new_review_table <- left_join(rownames_to_column(r$df_review_table), df_topic_wide) %>%
          select(-c(rowname))
        
        test <- new_review_table %>%
          pivot_longer(cols = c(5:ncol(new_review_table))) %>%
          filter(value == "TRUE")
        
        if(nrow(test) == 0){
          showNotification("Please ensure there is a topic selected for each exam!", type = "warning")
        } else{
          r$df_topic <- edit_topics_df
          
          r$df_review_table <- new_review_table 
          # Save review_grades
          save_df_review_grades()
          removeModal()
          showNotification("Saved in session.")
        }
        
      }
      
    })
    
    observeEvent(input$close_edits, {
      removeModal()
    })
  })
}