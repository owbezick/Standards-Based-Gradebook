
# Course information button module ----
edit_roster_button_UI <- function(id) {
  showModal(
    modalDialog(title = "Roster", size = "l", footer = NULL, easyClose = T
                , fluidRow(
                  column(width = 12
                         , tabsetPanel(
                           tabPanel(
                             # Current Roster ----
                             title = "Current Roster"
                             , br()
                             , box(width = 12, status = "primary"
                                   , formattableOutput(NS(id,"rosterList"))
                             )
                             , br()
                             , fluidRow(
                               # column(width = 6
                               #        , actionBttn(
                               #          inputId = NS(id,"editSave")
                               #          , label = "Save"
                               #          , style = "material-flat"
                               #          , block = T
                               #        )
                               # )
                               column(width = 12
                                        , actionBttn(
                                          inputId = NS(id,"editClose")
                                          , label = "Close"
                                          , style = "material-flat"
                                          , block = T
                                        )
                               )
                               
                             )
                           )
                           , tabPanel(
                             # Add to roster ----
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
                               column(width = 6
                                      , actionBttn(
                                        inputId = NS(id,"addSave")
                                        , label = "Add Student"
                                        , style = "material-flat"
                                        , block = T
                                      )
                               )
                               , column(width = 6
                                        , actionBttn(
                                          inputId = NS(id,"addClose")
                                          , label = "Close"
                                          , style = "material-flat"
                                          , block = T
                                        )
                               )
                             )
                           )
                           , tabPanel(title = "Remove from Roster"
                                      # Remove from roster ----
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
      df <- r$df_student %>%
        mutate(student_id = as.character(student_id)) %>%
        rename(`Student ID` = student_id
               , Name = name
               , Email = email)
      formattable(df)
    })
    # TODO: save edits
    # # BTN: Save edits ----
    # observeEvent(input$editSave, {
    #   # save to df_student ----
    #   temp <- hot_to_r(input$rosterList) %>%
    #     rename(student_id = `Student ID`
    #            , name = Name
    #            , email = Email) %>%
    #     mutate(student_id = as.numeric(student_id))
    #   r$df_student <- temp
    #   sheet_write(
    #     ss = "https://docs.google.com/spreadsheets/d/1xIC4pGhnnodwxqopHa45KRSHIVcOTxFSfJSEGPbQH20/editgid=2102408290"
    #     , data = temp
    #     , sheet = "student"
    #   ) 
    #   # save to homework_grades ----
    #   # save to review_grades ----
    # })
    # 
    # BTN Add Student Save ----
    observeEvent(input$addSave, {
      df_prev_student <- r$df_student
      new_row <- tibble("student_id" = input$addID
                        , "name" = input$addName
                        , "email" = input$addEmail
      )
      
      # CATCH:student id already exists ----
      if(input$addID %in% r$df_student$student_id){
        updateNumericInput(
          session = session
          , inputId = "addID"
          , label = "Student ID: "
          , value = 801000000
        )
        showNotification("Student ID number already exists.", type = "error")
        
      }else{
        sheet_append(
          ss = "https://docs.google.com/spreadsheets/d/1xIC4pGhnnodwxqopHa45KRSHIVcOTxFSfJSEGPbQH20/editgid=2102408290"
          , data = new_row
          , sheet = "student"
        ) 
        
        new_df <- rbind(r$df_student, new_row)
        r$df_student <- new_df
        
        # Save in homework_grades Sheet ----
        homework_grades <- r$df_homework_grades
        
        # CATCH: adding students when there are no homeworks ----
        if (ncol(homework_grades) == 1){
          temp <- tibble("Student Name" = input$addName)
          sheet_append(
            ss = "https://docs.google.com/spreadsheets/d/1xIC4pGhnnodwxqopHa45KRSHIVcOTxFSfJSEGPbQH20/editgid=2102408290"
            , data = temp
            , sheet = "homework_grades"
          ) 
          
        } else{
          temp <- r$df_homework_grades[1,] %>%
            mutate(`Student Name` = input$addName) 
          temp[1,2:ncol(temp)] <- "NA"
          sheet_append(
            ss = "https://docs.google.com/spreadsheets/d/1xIC4pGhnnodwxqopHa45KRSHIVcOTxFSfJSEGPbQH20/editgid=2102408290"
            , data = temp
            , sheet = "homework_grades"
          ) 
        }
        temp <-rbind(homework_grades, temp)
        r$df_homework_grades <- temp
        
        
        # Save to review_grades sheet ----
        # catch for adding students when there are no reviews
        if (nrow(r$df_review_grades) != 0){
          a_student_id <-  r$df_student[1,1] %>%
            pull()
          
          review_to_topic <- r$df_review_grades
          new_data <- review_to_topic %>%
            filter(student_id == a_student_id) %>%
            mutate(student_id = input$addID
                   , grade = "NA")
          
          temp <- rbind(review_to_topic, new_data) %>%
            arrange(review_id, topic_id)
          
          r$df_review_grades <- temp
          sheet_write(
            ss = "https://docs.google.com/spreadsheets/d/1xIC4pGhnnodwxqopHa45KRSHIVcOTxFSfJSEGPbQH20/editgid=2102408290"
            , data = temp
            , sheet = "review_grades"
          ) 
        }
        
        # Update Inputs & show notification ----
        updateNumericInput(
          session = session
          , inputId = "addID"
          , label = "Student ID: "
          , value = 801000000
        )
        
        updateTextInput(
          session = session
          , inputId = "addName"
          , value = "New Name"
          , placeholder = "New Name"
          , label = "Student Name: "
        )
        
        updateTextInput(
          session = session
          , inputId = "addEmail"
          , value = "New Email"
          , placeholder = "New Email"
          , label = "Student Email: "
        )
        showNotification("Saved to remote.")
      }
    })
    
    # BTN Remove Student Save ----
    observeEvent(input$removeSave, {
      ls_removed_names <- input$removeFromRoster
      if(length(ls_removed_names) == 0) {
        showNotification("No students selected to remove.")
        removeModal()
      }else{
        # save student sheet ----
        df_students <- r$df_student
        df_new_students <- subset(df_students, !(name %in% ls_removed_names))
        sheet_write(
          ss = "https://docs.google.com/spreadsheets/d/1xIC4pGhnnodwxqopHa45KRSHIVcOTxFSfJSEGPbQH20/editgid=2102408290"
          , data = df_new_students
          , sheet = "student"
        ) 
        r$df_student <- df_new_students
        
        # save homework_grades sheet ----
        df_homework_grades <- r$df_homework_grades
        temp <- subset(df_homework_grades, !(`Student Name` %in% ls_removed_names))
        r$df_homework_grades <- temp
        sheet_write(
          ss = "https://docs.google.com/spreadsheets/d/1xIC4pGhnnodwxqopHa45KRSHIVcOTxFSfJSEGPbQH20/editgid=2102408290"
          , data = temp
          , sheet = "homework_grades"
        ) 
        
        # save review sheet ----
        df_review_grades <- r$df_review_grades 
        if (ncol(df_review_grades) == 0){
          
        } else{
          df_review_to_topic <- df_review_grades %>%
            left_join(df_students) %>%
            select(-c(email))
          
          temp <- subset(df_review_to_topic, !(name %in% ls_removed_names)) %>%
            select(-c(name))
          
          r$df_review_to_topic <- temp
          sheet_write(
            ss = "https://docs.google.com/spreadsheets/d/1xIC4pGhnnodwxqopHa45KRSHIVcOTxFSfJSEGPbQH20/editgid=2102408290"
            , data = temp
            , sheet = "review_grades"
          ) 
        }
        
        showNotification("Saved to remote.")
      }
    })
    
    # BTN closing -----
    observeEvent(input$addClose, {
      removeModal()
    })
    observeEvent(input$rosterClose, {
      removeModal()
    })
    observeEvent(input$editClose, {
      removeModal()
    })
  })
}