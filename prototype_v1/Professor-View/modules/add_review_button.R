
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
                  )
                  , column(width = 6
                           , tags$b()
                           , dateInput(inputId = NS(id, "reviewDate")
                                       , label = "Date: ")
                           , br()
                           , checkboxGroupInput(
                             inputId = NS(id, "addReviewTopics")
                             , label = "Select Topics: "
                             , choices = r$df_topics$id
                           )
                  )
                )
    )
  )
}

add_review_button_Server <- function(id, r){
  moduleServer(id, function(input,output,session){
    observeEvent(input$save, {
      browser()
      df_review <- r$df_review
      new_row <- tibble("id" = input$reviewNumber
                        , "date" = input$homeworkDateAssigned
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