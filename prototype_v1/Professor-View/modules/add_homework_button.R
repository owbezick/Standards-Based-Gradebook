
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
                                 , dateInput(inputId = NS(id, "homeworkDateAssigned")
                                             , label = "Date Assigned: ")
                                 , br()
                                 , dateInput(inputId = NS(id, "homeworkDateDue")
                                             , label = "Date Due: ")
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

# Add homework server ----
add_homework_button_Server <- function(id, r){
  moduleServer(id, function(input,output,session){
    # Picker UI ----
    output$idPicker <- renderUI({
      numericInput(inputId = NS(id, "homeworkNumber")
                   , label = "Homework Number: "
                   , value = max(r$df_homework$id) + 1)
    })
    # BTN Save homework ----
    observeEvent(input$save, {
      df_homework <- r$df_homework
      # Check if HW id exists
      if(input$homeworkNumber %in% df_homework$id){
        showNotification("Homework ID already exists.", type = "error")
        updateNumericInput(
          session = session
          , inputId = "homeworkNumber"
          , label = "Homework Number: "
          , value = max(r$df_homework$id) + 1
        )
        
      }else{
        # Save to df_homework ----
        new_row <- tibble("id" = input$homeworkNumber
                          , "description" = input$homeworkDescription
                          , "date_assigned" = input$homeworkDateAssigned
                          , "date_due" = input$homeworkDateDue
        )
        # Save and refresh 
        # sheet_append(
        #   ss = "https://docs.google.com/spreadsheets/d/1xIC4pGhnnodwxqopHa45KRSHIVcOTxFSfJSEGPbQH20/edit#gid=2102408290"
        #   , data = new_row
        #   , sheet = "homework"
        # )
        new_df <- rbind(df_homework, new_row)
        r$df_homework <- new_df
        
        
        # Save to df_homework_grades ----
        df_homework_grades <- r$df_homework_grades
        new_column <- c(rep("NA", nrow(df_homework_grades)))
        new_column_name = paste("Homework", input$homeworkNumber)
        df_homework_grades <- df_homework_grades %>%
          mutate(init = new_column) 
        names(df_homework_grades)[names(df_homework_grades) == "init"] <- new_column_name
        
        # Save and refresh 
        r$df_homework_grades <- df_homework_grades
        # sheet_write(
        #   ss =  "https://docs.google.com/spreadsheets/d/1xIC4pGhnnodwxqopHa45KRSHIVcOTxFSfJSEGPbQH20/edit#gid=2102408290"
        #   , data = df_homework_grades
        #   , sheet = "homework_grades"
        # )
        # Show notification & Update Inputs ----
        updateNumericInput(
          session = session
          , inputId = "homeworkNumber"
          , label = "Homework Number: "
          , value = max(r$df_homework$id) + 1
        )
        updateDateInput(
          session = session
          , inputId = "homeworkDateAssigned"
          , label = "Date Assigned: "
          , value = Sys.Date()
        )
        updateDateInput(
          session = session
          , inputId = "homeworkDateDue"
          , label = "Date Due: "
          , value = Sys.Date()
        )
        updateTextAreaInput(session = session
                            ,inputId = "homeworkDescription"
                            , label = "Homework Description: "
                            , value = " ")
          
        showNotification("Saved to remote.")
        
      }
      
      
    })
    
    observeEvent(input$close, {
      removeModal()
    })
    
  })
}