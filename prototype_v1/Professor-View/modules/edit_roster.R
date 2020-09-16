
# Course information button module ----
edit_roster_button_UI <- function(id) {
  showModal(
    modalDialog(title = "Roster", size = "l"
                , fluidRow(tabsetPanel(
                  tabPanel(
                    title = "Current Roster"
                    , br()
                    , box(width = 12, status = "primary"
                      , formattableOutput(NS(id,"rosterList"))
                    )
                  )
                  , tabPanel(
                    title = "Add to Roster"
                    , br()
                    , column(width = 4
                             , box(width = 12, status = "primary"
                                   , tags$b("Student ID")
                                   , numericInput(inputId = NS(id, "addID")
                                                   , label = NULL
                                                   , value = 801000000)
                             )   
                    )
                    , column(width = 4
                       , box(width = 12, status = "primary"
                         , tags$b("Student Name")
                         , textAreaInput(inputId = NS(id, "addName")
                                         , label = NULL)
                             )
                    )
                   , column(width = 4
                        , box(width = 12, status = "primary"
                          , tags$b("Student Email")
                          , textAreaInput(inputId = NS(id, "addEmail")
                                          , label = NULL)
                            )
                        , actionBttn(
                          inputId = NS(id,"addSave")
                          , label = "Add Students"
                          , style = "material-flat"
                          , block = T
                        )
                   )
                  )
                  , tabPanel(title = "Remove from Roster"
                    , br()
                    , box(width = 12, status = "primary"
                      , uiOutput(NS(id, "removeFromRoster"))
                    )
                    , column(width = 4)
                    , column(width = 4)
                    , column(width = 4
                      , actionBttn(
                        inputId = NS(id, "removeSave")
                        , label = "Remove Students"
                        , style = "material-flat"
                        , block = T
                      )
                    )
                  )
                )
                )
                , footer = fluidRow(
                  column(width = 12
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
    
     output$removeFromRoster <-  renderUI({
       checkboxGroupInput(
         inputId = NS(id, "removeFromRoster")
         , label = "Select Students to Remove: "
         , choices = r$df_student$name
       )
     })
     
     output$rosterList <- renderFormattable({
       columns = c("ID", "Name", "Email")
       formattable(data.frame(
        ID = format(r$df_student$id, scientific=F)
         , Name = r$df_student$name
         , Email = r$df_student$email)
       )
     })
     
    
     observeEvent(input$removeSave, {
       ls_removed_names <- input$removeFromRoster
       
       df_students <- r$df_student
       df_new <- subset(df_students, !(name %in% ls_removed_names))
       
       # Write to sheet ----
       if(length(ls_removed_names) == 0) {
         showNotification("No students selected to remove.")
         removeModal()
       }else{
         sheet_write(
           ss = "https://docs.google.com/spreadsheets/d/1xIC4pGhnnodwxqopHa45KRSHIVcOTxFSfJSEGPbQH20/editgid=2102408290"
           , data = df_new
           , sheet = "student"
         ) 
         
         # Update Reactives ----
         r$df_student <- df_new
         removeModal()
         showNotification("Saved to remote.")
       }
     })
     
     
     observeEvent(input$addSave, {
       df_prev_student <- r$df_student
       new_row <- tibble("id" = input$addID
                         , "name" = input$addName
                         , "email" = input$addEmail
       )
       
        #Write to sheet ----
       if(input$addID %in% df_student$id){
         showNotification("Student ID number already exists.")
         removeModal()
       }else{
         sheet_append(
           ss = "https://docs.google.com/spreadsheets/d/1xIC4pGhnnodwxqopHa45KRSHIVcOTxFSfJSEGPbQH20/editgid=2102408290"
           , data = new_row
           , sheet = "student"
         ) 
         
         #      Update reactive ----
         new_df <- rbind(r$df_student, new_row)
         r$df_student <- new_df
         
         removeModal()
         showNotification("Saved to remote.")
       }
     })
     
     observeEvent(input$close, {
       removeModal()
     })
    
  })
}