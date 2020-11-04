
# Course information button module ----
edit_roster_button_UI <- function(id) {
  showModal(
    modalDialog(title = "Roster", size = "l", easyClose = T
                , fluidRow(
                  column(width = 12
                         , rHandsontableOutput(NS(id, "roster_table"))
                         
                         # , tabsetPanel(
                         #   tabPanel(
                         #     # Current Roster ----
                         #     title = "Current Roster"
                         #     , br()
                         #     , box(width = 12, status = "primary"
                         #           , formattableOutput(NS(id,"rosterList"))
                         #     )
                         #     , br()
                         #     , fluidRow(
                         #       # column(width = 6
                         #       #        , actionBttn(
                         #       #          inputId = NS(id,"editSave")
                         #       #          , label = "Save"
                         #       #          , style = "material-flat"
                         #       #          , block = T
                         #       #        )
                         #       # )
                         #       column(width = 12
                         #                , actionBttn(
                         #                  inputId = NS(id,"editClose")
                         #                  , label = "Close"
                         #                  , style = "material-flat"
                         #                  , block = T
                         #                )
                         #       )
                         #       
                         #     )
                         #   )
                         #   , tabPanel(
                         #     # Add to roster ----
                         #     title = "Add to Roster"
                         #     , br()
                         #     , fluidRow(
                         #       column(width = 6
                         #              , box(width = 12, status = "primary"
                         #                    , numericInput(
                         #                      inputId = NS(id, "addID")
                         #                      , label = "Student ID: "
                         #                      , value = 801000000
                         #                    )
                         #              )   
                         #       )
                         #       , column(width = 6
                         #                , box(width = 12, status = "primary"
                         #                      , textInput(
                         #                        inputId = NS(id, "addName")
                         #                        , label = "Student Name: "
                         #                      )
                         #                )
                         #       )
                         #     )
                         #     , fluidRow(
                         #       column(width = 6
                         #              , actionBttn(
                         #                inputId = NS(id,"addSave")
                         #                , label = "Add Student"
                         #                , style = "material-flat"
                         #                , block = T
                         #              )
                         #       )
                         #       , column(width = 6
                         #                , actionBttn(
                         #                  inputId = NS(id,"addClose")
                         #                  , label = "Close"
                         #                  , style = "material-flat"
                         #                  , block = T
                         #                )
                         #       )
                         #     )
                         #   )
                         #   , tabPanel(title = "Remove from Roster"
                         #              # Remove from roster ----
                         #              , br()
                         #              , box(width = 12, status = "primary"
                         #                    , uiOutput(NS(id, "removeFromRoster"))
                         #              )
                         #              , fluidRow(
                         #                column(width = 12
                         #                       , actionBttn(
                         #                         inputId = NS(id, "removeSave")
                         #                         , label = "Remove Students"
                         #                         , style = "material-flat"
                         #                         , block = T
                         #                       )
                         #                )
                         #              )
                         #   )
                         # )
                  )
                )
                , footer = fluidRow(
                  column(width = 6
                         , actionBttn(
                           inputId = NS(id,"save")
                           , label = "Save Roster"
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

edit_roster_button_Server <- function(id, r){
  moduleServer(id, function(input,output,session){
    # UI ----
    output$removeFromRoster <-  renderUI({
      checkboxGroupInput(
        inputId = NS(id, "removeFromRoster")
        , label = "Select Students to Remove: "
        , choices = r$df_student$name
      )
    })
    
    output$rosterList <- renderFormattable({
      df <- r$df_student %>%
        mutate(student_id = as.character(student_id)) %>%
        rename(`Student ID` = student_id
               , Name = name
              )
      formattable(df)
    })
    
    output$roster_table <- renderRHandsontable({
      df_roster <- r$df_student %>%
        select(`Student ID` = student_id
               , Name = name)
      
      rhandsontable(
        df_roster
        , rowHeaders = NULL
        , stretchH = 'all'
      )
      
    })
    
    observeEvent(input$save, {
      req(input$roster_table)
      if (length(unique(hot_to_r(input$roster_table)$`Student ID`)) != length(hot_to_r(input$roster_table)$`Student ID`)){
        showNotification("Ensure that all student ID's are unique!", type = "warning")
      } else{
        
        r$df_student <- hot_to_r(input$roster_table) %>%
          select(student_id = `Student ID`, name = Name)
        
        save_df_homework_grades()
        save_df_review_grades()
        
        showNotification("Saved in session.")
        
        removeModal()
      }
    })
    
    observeEvent(input$close, {
      removeModal()
    })
  })
}