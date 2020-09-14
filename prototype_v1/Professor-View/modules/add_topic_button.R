
# Course information button module ----
add_topic_button_UI <- function(id) {
  showModal(
    modalDialog(title = "Add Topic", size = "m"
                , fluidRow(box(width = 12, status = "primary"
                               , column(width = 2
                                        , tags$b("Topic Number: ")
                                        , numericInput(inputId = NS(id, "topicNumber")
                                                       , label = NULL
                                                       , value = 0)
                                        
                               )
                               , column(width = 10
                                        , tags$b("Topic Description: ")
                                        , textAreaInput(inputId = NS(id, "topicDescription")
                                                        , label = NULL)
                               )
                               , footer = fluidRow(actionBttn(inputId = NS(id,"save"), label = "Save Topic", block = T)
                                                   , actionBttn(inputId = NS(id, "close"), label = "Close", block = T))
                )
                )
    )
  )
}

add_topic_button_Server <- function(id, r){
  moduleServer(id, function(input,output,session){
  
    observeEvent(input$save, {
      df_topic <- r$df_topic
      new_row <- tibble("id" = input$topicNumber
                       , "description" = input$topicDescription
      )
      
      # Write to sheet ----
      if(input$topicNumber %in% df_topic$id){
        showNotification("Topic already exists.")
        removeModal()
      }else{
        sheet_append(
          ss = "https://docs.google.com/spreadsheets/d/1xIC4pGhnnodwxqopHa45KRSHIVcOTxFSfJSEGPbQH20/edit#gid=2102408290"
          , data = new_row
          , sheet = "topic"
        )
        showNotification("Saved to remote.")
      }

      # Update reactive ----
      new_df <- rbind(df_topic, new_row)
      r$df_topic <- new_df
      
      removeModal()
    })
    observeEvent(input$close, {
      removeModal()
    })
  })
}