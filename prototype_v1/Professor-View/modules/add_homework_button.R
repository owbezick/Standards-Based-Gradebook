
# Course information button module ----
add_homework_button_UI <- function(id) {
  showModal(
    modalDialog(title = "Edit Homeworks", size = "l"
                , fluidRow(
                    column(width = 12
                  # column(width = 6
                  #        , uiOutput(NS(id, "idPicker"))
                  #        , br()
                  #        , textAreaInput(inputId = NS(id, "homeworkDescription")
                  #                        , label = "Homework Description: ")
                  # )
                  # , column(width = 6
                  #          , div(class = "date_inputs", id = "date-inputs"
                  #                , dateInput(inputId = NS(id, "homeworkDateAssigned")
                  #                            , label = "Date Assigned: ")
                  #                , br()
                  #                , dateInput(inputId = NS(id, "homeworkDateDue")
                  #                            , label = "Date Due: ")
                  #          )
                  # )
                    , rHandsontableOutput(NS(id, "homework_table"))
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
    
    output$homework_table <- renderRHandsontable({
      #browser()
      df_homework <- r$df_homework %>%
        mutate(id = as.character(id)
               , Description = description
               , `Date Assigned` = ymd(date_assigned)
               , `Due Date` = ymd(date_due)) %>%
      select(id, Description, `Date Assigned`, `Due Date`)
      
      #If there are no homeworks, add row for prof to edit initial one
      if (nrow(df_homework) < 1){
        df_homework[nrow(df_homework) + 1,] <- list(as.character(nrow(df_homework) + 1, length = 1)
                                                    , "sample description"
                                                    , ymd(Sys.Date())
                                                    , ymd(Sys.Date()))
      }
      
     
      rhandsontable(
        df_homework
        , rowHeaders = NULL
        , stretchH = 'all'
        , readOnly = F
      ) %>%
        hot_col("id", readOnly = T)
        
    })
    # Picker UI ----
    output$idPicker <- renderUI({
      numericInput(inputId = NS(id, "homeworkNumber")
                   , label = "Homework Number: "
                   , value = max(r$df_homework$id) + 1)
    })
    observeEvent(input$save, {
      req(input$homework_table)
      df_new <- hot_to_r(input$homework_table) %>%
        select(id
               , description = Description
               , date_assigned = `Date Assigned`
               , date_due = `Due Date`)
      
      #Fill in first row ids
      df_new <- df_new %>%
        mutate(id = 1:nrow(df_new)) #Wont work if removing?
      
      #Update reactive
      r$df_homework <- df_new
      
      #Close modal
      removeModal()
      showNotification("Saved in session.")
      
    })
    
    
    # BTN Save homework ----
    # observeEvent(input$save, {
    #   df_homework <- r$df_homework
    #   # Check if HW id exists
    #   if(input$homeworkNumber %in% df_homework$id){
    #     showNotification("Homework ID already exists.", type = "error")
    #     updateNumericInput(
    #       session = session
    #       , inputId = "homeworkNumber"
    #       , label = "Homework Number: "
    #       , value = max(r$df_homework$id) + 1
    #     )
    #     
    #   }else{
    #     # Save to df_homework ----
    #     new_row <- tibble("id" = input$homeworkNumber
    #                       , "description" = input$homeworkDescription
    #                       , "date_assigned" = input$homeworkDateAssigned
    #                       , "date_due" = input$homeworkDateDue
    #     )
    #     # Save and refresh 
    #     new_df <- rbind(df_homework, new_row)
    #     r$df_homework <- new_df
    #     
    #     
    #     # Save to df_homework_grades ----
    #     df_homework_grades <- r$df_homework_grades
    #     new_column <- c(rep("NA", nrow(df_homework_grades)))
    #     new_column_name = paste("Homework", input$homeworkNumber)
    #     df_homework_grades <- df_homework_grades %>%
    #       mutate(init = new_column) 
    #     names(df_homework_grades)[names(df_homework_grades) == "init"] <- new_column_name
    #     
    #     # Save and refresh 
    #     r$df_homework_grades <- df_homework_grades 
    #     
    #     # Show notification & Update Inputs ----
    #     updateNumericInput(
    #       session = session
    #       , inputId = "homeworkNumber"
    #       , label = "Homework Number: "
    #       , value = max(r$df_homework$id) + 1
    #     )
    #     updateDateInput(
    #       session = session
    #       , inputId = "homeworkDateAssigned"
    #       , label = "Date Assigned: "
    #       , value = Sys.Date()
    #     )
    #     updateDateInput(
    #       session = session
    #       , inputId = "homeworkDateDue"
    #       , label = "Date Due: "
    #       , value = Sys.Date()
    #     )
    #     updateTextAreaInput(session = session
    #                         ,inputId = "homeworkDescription"
    #                         , label = "Homework Description: "
    #                         , value = " ")
    #       
    #     showNotification("Saved in session.")
    #     
    #   }
    #   
    #   
    # })
    
    observeEvent(input$close, {
      removeModal()
    })
    
  })
}