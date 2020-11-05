
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
        mutate(id = as.character(`topic_id`)) %>%
        select(id, Description = description)
      
      # Sort df by id 
      df <- df[base::order(df$id),]
      
      rhandsontable(
        df
        , rowHeaders = NULL
        , stretchH = 'all'
      )
    })
    
    observeEvent(input$save_edits, {
      req(input$edit_topics)
      
      # Save to df_topic
      if (length(unique(hot_to_r(input$edit_topics)$id)) != length(hot_to_r(input$edit_topics)$id)){
        showNotification("Please ensure all topic ID's are unique!", type = "warning")
      } else{
      r$df_topic <- hot_to_r(input$edit_topics) %>%
        select(topic_id = id, description = Description)
      
      # Save review_table
      df_topic_wide <- r$df_topic %>%
        mutate(include = FALSE) %>%
        pivot_wider(id_cols = c(topic_id), names_from = topic_id, names_prefix = "Topic ", values_from = include)
      
      new_review_table <- left_join(r$df_review_table, df_topic_wide)
      new_review_table[is.na(new_review_table)] = FALSE
      r$df_review_table <- new_review_table
      
      # Save review_grades
      save_df_review_grades()
      }
      showNotification("Saved in session.")
    })
    
    observeEvent(input$close_edits, {
      removeModal()
    })
  })
}