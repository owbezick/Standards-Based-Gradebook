
# Course information button module ----
add_topic_button_UI <- function(id) {
  showModal(
    modalDialog(title = "Add Topic", size = "m"
                , fluidRow(
                  column(width = 2
                         , tags$b("Topic Number: ")
                         , uiOutput(NS(id, "topic_input"))
                  )
                  , column(width = 10
                           , tags$b("Topic Description: ")
                           , textAreaInput(inputId = NS(id, "topicDescription")
                                           , label = NULL)
                  ))
                  , footer = fluidRow(
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
}

add_topic_button_Server <- function(id, r){
  moduleServer(id, function(input,output,session){
  
    output$topic_input <- renderUI({
      numericInput(inputId = NS(id, "topicNumber")
                     , label = NULL
                     , value =  max(r$df_topic$topic_id) + 1)
    })
    
    observeEvent(input$save, {
      # browser()
      if(input$topicNumber %in% r$df_topic$topic_id){
        showNotification("Topic already exists.")
        removeModal()
      }else{
        # Update df_topic 
        df_topic <- r$df_topic
        new_row <- tibble("topic_id" = input$topicNumber
                          , "description" = input$topicDescription
        )
        
        sheet_append(
          ss = "https://docs.google.com/spreadsheets/d/1xIC4pGhnnodwxqopHa45KRSHIVcOTxFSfJSEGPbQH20/edit#gid=2102408290"
          , data = new_row
          , sheet = "topic"
        )
        # Update df_review_table
        df_review_table <- r$df_review_table
        new_column <- c(rep(NA, nrow(df_review_table)))
        new_column_name = paste("Topic", input$topicNumber)
        df_review_table <- df_review_table %>%
          mutate(init = new_column) 
        names(df_review_table)[names(df_review_table) == "init"] <- new_column_name
        sheet_write(
          ss =  "https://docs.google.com/spreadsheets/d/1xIC4pGhnnodwxqopHa45KRSHIVcOTxFSfJSEGPbQH20/edit#gid=2102408290"
          , data = df_review_table
          , sheet = "review_table"
        )
        # Update reactive 
        new_df <- rbind(df_topic, new_row)
        r$df_topic <- new_df
        r$df_review_table <- df_review_table
        
        removeModal()
        showNotification("Saved to remote.")
      }


      
      
      
    })
    
    observeEvent(input$close, {
      removeModal()
    })
  })
}