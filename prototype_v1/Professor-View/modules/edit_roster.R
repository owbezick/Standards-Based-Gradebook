
# Course information button module ----
edit_roster_button_UI <- function(id) {
  showModal(
    modalDialog(title = "Roster", size = "l", footer = NULL, easyClose = T
                , fluidRow(
                  column(width = 12
                         , tabsetPanel(
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
                             , fluidRow(
                               column(width = 4
                                      , box(width = 12, status = "primary"
                                            , numericInput(
                                              inputId = NS(id, "addID")
                                              , label = "Student ID: "
                                              , value = 801000000
                                            )
                                      )   
                               )
                               , column(width = 4
                                        , box(width = 12, status = "primary"
                                              , textInput(
                                                inputId = NS(id, "addName")
                                                , label = "Student Name: "
                                              )
                                        )
                               )
                               , column(width = 4
                                        , box(width = 12, status = "primary"
                                              , textInput(
                                                inputId = NS(id, "addEmail")
                                                , label = "Student Email: "
                                              )
                                        )
                               )
                             )
                             , fluidRow(
                               column(width = 12
                                      , actionBttn(
                                        inputId = NS(id,"addSave")
                                        , label = "Add Student"
                                        , style = "material-flat"
                                        , block = T
                                      )
                               )
                             )
                           )
                           , tabPanel(title = "Remove from Roster"
                                      , br()
                                      , box(width = 12, status = "primary"
                                            , uiOutput(NS(id, "removeFromRoster"))
                                      )
                                      , fluidRow(
                                        column(width = 12
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
      columns = c("ID", "Name", "Email")
      formattable(data.frame(
        ID = format(r$df_student$student_id, scientific=F)
        , Name = r$df_student$name
        , Email = r$df_student$email)
      )
    })
    
    # Remove Student Save ----
    observeEvent(input$removeSave, {
      ls_removed_names <- input$removeFromRoster
      
  
      if(length(ls_removed_names) == 0) {
        showNotification("No students selected to remove.")
        removeModal()
      }else{
        # student sheet
        df_students <- r$df_student
        df_new_students <- subset(df_students, !(name %in% ls_removed_names))
        
        sheet_write(
          ss = "https://docs.google.com/spreadsheets/d/1xIC4pGhnnodwxqopHa45KRSHIVcOTxFSfJSEGPbQH20/editgid=2102408290"
          , data = df_new_students
          , sheet = "student"
        ) 
        
        # homework sheet 
        homework_table <- r$df_homework_table
        temp <- subset(homework_table, !(`Student Name` %in% ls_removed_names))
        r$df_homework_table <- temp
        sheet_write(
          ss = "https://docs.google.com/spreadsheets/d/1xIC4pGhnnodwxqopHa45KRSHIVcOTxFSfJSEGPbQH20/editgid=2102408290"
          , data = temp
          , sheet = "homework_table"
        ) 
        
        # review sheet
        df_review_to_topic <- r$df_review_to_topic %>%
          left_join(r$df_student) %>%
          select(-c(email))
        
        temp <- subset(df_review_to_topic, !(name %in% ls_removed_names)) %>%
          select(-c(name))
        
        r$df_review_to_topic <- temp
        sheet_write(
          ss = "https://docs.google.com/spreadsheets/d/1xIC4pGhnnodwxqopHa45KRSHIVcOTxFSfJSEGPbQH20/editgid=2102408290"
          , data = temp
          , sheet = "review_to_topic"
        ) 
        showNotification("Saved to remote.")
        removeModal()
      }
    })
    
    # Add Student Save ----
    observeEvent(input$addSave, {
      df_prev_student <- r$df_student
      new_row <- tibble("student_id" = input$addID
                        , "name" = input$addName
                        , "email" = input$addEmail
      )
      
      if(input$addID %in% r$df_student$student_id){
        showNotification("Student ID number already exists.")
        removeModal()
      }else{
        sheet_append(
          ss = "https://docs.google.com/spreadsheets/d/1xIC4pGhnnodwxqopHa45KRSHIVcOTxFSfJSEGPbQH20/editgid=2102408290"
          , data = new_row
          , sheet = "student"
        ) 
        
        new_df <- rbind(r$df_student, new_row)
        r$df_student <- new_df
        
        # homework_table sheet
        homework_table <- r$df_homework_table
        new_row <- c(input$addName, rep(NA, ncol(r$df_homework_table) - 1))
        temp <-rbind(homework_table, new_row)
        r$df_homework_table <- temp
        sheet_write(
          ss = "https://docs.google.com/spreadsheets/d/1xIC4pGhnnodwxqopHa45KRSHIVcOTxFSfJSEGPbQH20/editgid=2102408290"
          , data = temp
          , sheet = "homework_table"
        ) 
      
        # review to topic 
        a_student_id <-  r$df_student[1,1] %>%
          pull()
        
        review_to_topic <- r$df_review_to_topic
        new_data <- review_to_topic %>%
          filter(student_id == a_student_id) %>%
          mutate(student_id = input$addID
                 , grade = "NA")
        
        temp <- rbind(review_to_topic, new_data) %>%
          arrange(review_id, topic_id)
        
        r$df_review_to_topic <- temp
        sheet_write(
          ss = "https://docs.google.com/spreadsheets/d/1xIC4pGhnnodwxqopHa45KRSHIVcOTxFSfJSEGPbQH20/editgid=2102408290"
          , data = temp
          , sheet = "review_to_topic"
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