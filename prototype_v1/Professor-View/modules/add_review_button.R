
# Course information button module ----
add_review_button_UI <- function(id) {
  showModal(
    modalDialog(title = "Add Review", size = "l"
                , fluidRow(
                  column(width = 6
                         , numericInput(inputId = NS(id, "reviewNumber")
                                        , label = "Review Number: "
                                        , value = 0)
                         , br()
                         , textAreaInput(inputId = NS(id, "reviewDescription")
                                         , label = "Review Description: ")
                         , br()
                         , tags$b()
                         , dateInput(inputId = NS(id, "reviewDate")
                                     , label = "Date: ")
                  )
                  , column(width = 6
                           , uiOutput(NS(id, "topics"))
                  )
                )
                , footer = fluidRow(
                  column(width = 6
                         , actionBttn(
                           inputId = NS(id,"save")
                           , label = "Save Review"
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

add_review_button_Server <- function(id, r){
  moduleServer(id, function(input,output,session){
    output$topics <-  renderUI({
      checkboxGroupInput(
        inputId = NS(id, "addReviewTopics")
        , label = "Select Topics: "
        , choices = r$df_topic$id
      )
    })
    
    observeEvent(input$save, {
      df_review <- r$df_review
      new_row <- tibble("id" = input$reviewNumber
                        , "date" = input$reviewDate
                        , "description" = input$reviewDescription
                        
      )
      
      # Write to sheet ----
      if(input$reviewNumber %in% df_review$id){
        showNotification("Review number already exists.")
        removeModal()
      }else{
        sheet_append(
          ss = "https://docs.google.com/spreadsheets/d/1xIC4pGhnnodwxqopHa45KRSHIVcOTxFSfJSEGPbQH20/edit#gid=2102408290"
          , data = new_row
          , sheet = "review"
        )      # Update reactive ----
        new_df <- rbind(df_review, new_row)
        r$df_review <- new_df
        
        #TODO: write to exam to topic sheet
        removeModal()
        showNotification("Saved to remote.")
      }
    })
    
    observeEvent(input$close, {
      removeModal()
    })
    
  })
}