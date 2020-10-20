
# Course information button module ----
add_homework_button_UI <- function(id) {
  showModal(
    modalDialog(title = "Add Homework", size = "l"
                , fluidRow(
                  column(width = 6
                         , uiOutput(NS(id, "idPicker"))
                         , br()
                         , textAreaInput(inputId = NS(id, "homeworkDescription")
                                         , label = "Homework Description: ")
                  )
                  , column(width = 6
                           , div(class = "date_inputs", id = "date-inputs"
                             , dateInput(inputId = NS(id, "homeworkDateDue")
                                         , label = "Date Due: ")
                             , br()
                             , dateInput(inputId = NS(id, "homeworkDateAssigned")
                                         , label = "Date Assigned: ")
                           )
                  )
                )
                  , footer = fluidRow(
                   column(width = 6
                           , actionBttn(
                             inputId = NS(id,"save")
                             , label = "Save Homework"
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

add_homework_button_Server <- function(id, r){
  moduleServer(id, function(input,output,session){
    
    output$idPicker <- renderUI({
       numericInput(inputId = NS(id, "homeworkNumber")
                     , label = "Homework Number: "
                     , value = max(r$df_homework$id) + 1)
    })
    observeEvent(input$save, {
      df_homework <- r$df_homework
      # Write to sheet ----
      if(input$homeworkNumber %in% df_homework$id){
        showNotification("Homework already exists.")
        removeModal()
      }else{
        # DF Homework
        new_row <- tibble("id" = input$homeworkNumber
                          , "description" = input$homeworkDescription
                          , "date_assigned" = input$homeworkDateAssigned
                          , "date_due" = input$homeworkDateDue
        )
        sheet_append(
          ss = "https://docs.google.com/spreadsheets/d/1xIC4pGhnnodwxqopHa45KRSHIVcOTxFSfJSEGPbQH20/edit#gid=2102408290"
          , data = new_row
          , sheet = "homework"
        )
        # Update reactive ----
        new_df <- rbind(df_homework, new_row)
        r$df_homework <- new_df
        
        # DF Homework Table
        df_homework_table <- r$df_homework_table
        new_column <- c(rep("NA", nrow(df_homework_table)))
        new_column_name = paste("Homework", input$homeworkNumber)
        df_homework_table <- df_homework_table %>%
          mutate(init = new_column) 
        names(df_homework_table)[names(df_homework_table) == "init"] <- new_column_name
        
        # refresh and save data
        r$df_homework_table <- df_homework_table
        sheet_write(
          ss =  "https://docs.google.com/spreadsheets/d/1xIC4pGhnnodwxqopHa45KRSHIVcOTxFSfJSEGPbQH20/edit#gid=2102408290"
          , data = df_homework_table
          , sheet = "homework_table"
        )
       
        showNotification("Saved to remote.")
        removeModal()
      }

      
    })
    
    observeEvent(input$close, {
      removeModal()
    })
    
  })
}