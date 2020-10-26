
# Course information button module ----
add_topic_button_UI <- function(id) {
  showModal(
    modalDialog(title = "Edit Topics", size = "l"
                , tabBox(width = 12
                         , tabPanel("Edit Topic Descriptions"
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
                         , tabPanel("Add Topics"
                                    , fluidRow(
                                      column(width = 3
                                             , tags$b("Topic Number: ")
                                             , uiOutput(NS(id, "topic_input"))
                                      )
                                      , column(width = 9
                                               , tags$b("Topic Description: ")
                                               , textAreaInput(inputId = NS(id, "topicDescription")
                                                               , label = NULL
                                               )
                                      )
                                    )
                                    , fluidRow(
                                      column(width = 6
                                             , actionBttn(
                                               inputId = NS(id,"save")
                                               , label = "Save Topic"
                                               , style = "material-flat"
                                               , block = T
                                             )
                                      )
                                      , column(width = 6
                                               , actionBttn(
                                                 inputId = NS(id, "close")
                                                 , label = "Close"
                                                 , style = "material-flat"
                                                 , block = T
                                               )
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
      df <- r$df_topic
      df <- df %>%
        mutate(id = as.character(`topic_id`)) %>%
        select(id, Description = description)
      
      rhandsontable(
        df
        , rowHeaders = NULL
        , stretchH = 'all'
        , readOnly = T
      ) %>%
        hot_col("Description", readOnly = F)
    })
    
    observeEvent(input$save_edits, {
      df <- hot_to_r(input$edit_topics) %>%
        select(topic_id = id, description = Description)
      
      removeModal()
      showNotification("Saved in session.")
    })
    
    output$topic_input <- renderUI({
      numericInput(inputId = NS(id, "topicNumber")
                   , label = NULL
                   , value = as.numeric(max(r$df_topic$topic_id)) + 1)
    })
    
    observeEvent(input$save, {
      if(input$topicNumber %in% r$df_topic$topic_id){
        showNotification("Topic already exists.")
        removeModal()
      }else{
        # Update df_topic 
        df_topic <- r$df_topic
        new_row <- tibble("topic_id" = input$topicNumber
                          , "description" = input$topicDescription
        )
        
        # Save & refresh data ----
        df_review_table <- r$df_review_table
        new_column <- c(rep(NA, nrow(df_review_table)))
        new_column_name = paste("Topic", input$topicNumber)
        df_review_table <- df_review_table %>%
          mutate(init = new_column) 
        names(df_review_table)[names(df_review_table) == "init"] <- new_column_name
        new_df <- rbind(df_topic, new_row)
        r$df_topic <- new_df
        r$df_review_table <- df_review_table
        
        # Update Inputs ----
        updateTextAreaInput(
          session = session
          , inputId = "topicDescription"
          , label = NULL
          , value = "New Description"
        )
        
        updateNumericInput(session = session
                           , inputId = "topicNumber"
                           , label = NULL
                           , value =  as.numeric(max(r$df_topic$topic_id)) + 1)
        
        showNotification("Saved in session.")
      }
      
    })
    
    observeEvent(input$close, {
      removeModal()
    })
    observeEvent(input$close_edits, {
      removeModal()
    })
  })
}