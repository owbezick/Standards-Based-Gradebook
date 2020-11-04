#  UI-----
add_homework_button_UI <- function(id) {
  showModal(
    modalDialog(title = "Edit Homeworks", size = "l"
                , fluidRow(
                    column(width = 12
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

# Server ----
add_homework_button_Server <- function(id, r){
  moduleServer(id, function(input,output,session){
    
    output$homework_table <- renderRHandsontable({

      df_homework <- r$df_homework %>%
        mutate(id = as.character(id)
               , Description = description
               , `Date Assigned` = ymd(date_assigned)
               , `Due Date` = ymd(date_due)) %>%
      select(id, Description, `Date Assigned`, `Due Date`)
      
     
      rhandsontable(
        df_homework
        , rowHeaders = NULL
        , stretchH = 'all'
        , readOnly = F
      )
    })
    
    # Picker UI ----
    output$idPicker <- renderUI({
      max_id <- max(r$df_homework$id)
      if(max_id == -Inf){
        value = 1
      } else {
        value = max_id + 1
      }
      numericInput(inputId = NS(id, "homeworkNumber")
                   , label = "Homework Number: "
                   , value = max_id)
    })
    # Saving ----
    observeEvent(input$save, {
      req(input$homework_table)
      # Save to df_homework
      r$df_homework <- hot_to_r(input$homework_table) %>%
        select(id
               , description = Description
               , date_assigned = `Date Assigned`
               , date_due = `Due Date`)
      
      # Function assumes that r$df_homework has been refreshed
      save_df_homework_grades()
      
      #Close modal
      removeModal()
      showNotification("Saved in session.")
      
    })
    
    
    observeEvent(input$close, {
      removeModal()
    })
    
  })
}