
# Course information button module ----
add_homework_button_UI <- function(id) {
  showModal(
    modalDialog(title = "Add Homework", size = "l"
                , fluidRow(
                  column(width = 6
                         , numericInput(inputId = NS(id, "homeworkNumber")
                                        , label = "Homework Number: "
                                        , value = 0)
                         , br()
                         , textAreaInput(inputId = NS(id, "homeworkDescription")
                                         , label = "Homework Description: ")
                  )
                  , column(width = 6
                           , dateInput(inputId = NS(id, "homeworkDateDue")
                                       , label = "Date Due: ")
                           , br()
                           , dateInput(inputId = NS(id, "homeworkDateAssigned")
                                       , label = "Date Assigned: ")
                  )
                )
                  , footer = fluidRow(
                   column(width = 6
                           , actionBttn(
                             inputId = NS(id,"save"), label = "Save Homework", block = T
                           )
                  )
                  , column(width = 6
                           , actionBttn(
                             inputId = NS(id, "close"), label = "Close", block = T
                           )
                  )
                )
    )
  )
}

add_homework_button_Server <- function(id, r){
  moduleServer(id, function(input,output,session){
    observeEvent(input$save, {
      df_homework <- r$df_homework
      new_row <- tibble("id" = input$homeworkNumber
                        , "description" = input$homeworkDescription
                        , "date_assigned" = input$homeworkDateAssigned
                        , "date_due" = input$homeworkDateDue
      )
      
      # Write to sheet ----
      if(input$homeworkNumber %in% df_homework$id){
        showNotification("Homework already exists.")
        removeModal()
      }else{
        sheet_append(
          ss = "https://docs.google.com/spreadsheets/d/1xIC4pGhnnodwxqopHa45KRSHIVcOTxFSfJSEGPbQH20/edit#gid=2102408290"
          , data = new_row
          , sheet = "homework"
        )      # Update reactive ----
        new_df <- rbind(df_homework, new_row)
        r$df_homework <- new_df
        removeModal()
        showNotification("Saved to remote.")
      }
    })
    
    observeEvent(input$close, {
      removeModal()
    })
    
  })
}