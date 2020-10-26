wizardUI <- function(id, r){
  df <- r$df_course_info
  tabBox(width = 12
         , tabPanel(title = "Add Course Info"
                    , fluidRow(
                      column(width = 6
                             , tags$b("Course Location: ")
                             , textInput(inputId = NS(id,"location")
                                         , label = NULL, value = df$location)
                             , br()
                             , tags$b("Meeting times: ")
                             , textInput(inputId = NS(id,"meeting_times")
                                         , label = NULL, value = df$meeting_times)
                             , br()
                             , tags$b("Office hours: ")
                             , textInput(inputId = NS(id,"office_hours")
                                         , label = NULL, value = df$office_hours)
                      )
                      , column(width = 6
                               , column(width = 6
                                        , tags$b("Link URL: ")
                                        , textInput(inputId = NS(id,"link1_url")
                                                    , label = NULL, value = df$link1_url)
                                        , br()
                                        , tags$b("Link URL: ")
                                        , textInput(inputId = NS(id,"link2_url")
                                                    , label = NULL, value = df$link2_url)
                                        , br()
                                        , tags$b("Link URL: ")
                                        , textInput(inputId = NS(id,"link3_url")
                                                    , label = NULL, value = df$link3_url)
                               )
                               , column(width = 6
                                        , tags$b("Link Description: ")
                                        , textInput(inputId = NS(id,"link1_text")
                                                    , label = NULL, value = df$link1_text)
                                        , br()
                                        , tags$b("Link Description: ")
                                        , textInput(inputId = NS(id,"link2_text")
                                                    , label = NULL, value = df$link2_text)
                                        , br()
                                        , tags$b("Link Description: ")
                                        , textInput(inputId = NS(id,"link3_text")
                                                    , label = NULL, value = df$link3_text)
                               )
                      )
                    )
                    , fluidRow(
                      column(width = 12
                             , actionBttn(inputId = NS(id,"saveCourseInfo")
                                          , label = "Add Course Information"
                                          , style = "material-flat"
                                          , block = T
                             )
                      )
                    )
                    )
         , tabPanel(title = "Add Roster"
                    , fluidRow(
                      column(width = 6
                             , box(width = 12, status = "primary"
                                   , numericInput(
                                     inputId = NS(id, "addID")
                                     , label = "Student ID: "
                                     , value = 801000000
                                   )
                             )   
                      )
                      , column(width = 6
                               , box(width = 12, status = "primary"
                                     , textInput(
                                       inputId = NS(id, "addName")
                                       , label = "Student Name: "
                                     )
                               )
                      )
                    )
                    , fluidRow(
                      column(width = 12
                             , actionBttn(
                               inputId = NS(id,"addStudent")
                               , label = "Add Student"
                               , style = "material-flat"
                               , block = T
                             )
                      )
                    )
         )
         , tabPanel(title = "Add Homeworks"
                    , fluidRow(
                      column(width = 6
                             , uiOutput(NS(id, "homework_idPicker"))
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
                    , fluidRow(
                      column(width = 12
                             , actionBttn(
                               inputId = NS(id,"saveHomework")
                               , label = "Add Homework"
                               , style = "material-flat"
                               , block = T
                             )
                      )
                    )
                    )
         , tabPanel(title = "Add Topics"
                    , fluidRow(
                      column(width = 3
                             , tags$b("Topic Number: ")
                             , uiOutput(NS(id, "topic_input"))
                      )
                      , column(width = 9
                               , tags$b("Topic Description: ")
                               , textAreaInput(inputId = NS(id, "topicDescription")
                                               , label = NULL
                               )
                      )
                    )
                    , fluidRow(
                      column(width = 12
                             , actionBttn(
                               inputId = NS(id,"saveTopic")
                               , label = "Save Topic"
                               , style = "material-flat"
                               , block = T
                             )
                      )
                    )
                    )
         , tabPanel(title = "Add Reviews"
                    , fluidRow(
                      column(width = 6
                             , uiOutput(NS(id, "review_idPicker"))

                             , br()
                             , textAreaInput(inputId = NS(id, "reviewName")
                                             , label = "Review Name: ")
                             , br()
                             , div(class = "date_inputs", id = "date-inputs"
                                   , dateInput(inputId = NS(id, "reviewDate")
                                               , label = "Review Date: ")
                             )
                      )
                      , column(width = 6
                               , uiOutput(NS(id, "review_topics"))
                      )
                    )
                    , fluidRow(
                      column(width = 12
                             , actionBttn(
                               inputId = NS(id,"saveReview")
                               , label = "Add Review"
                               , style = "material-flat"
                               , block = T
                             )
                      )
                    )
                    )
  )
}

wizard_server <- function(id, r) {
  moduleServer(id, function(input, output, session){
    
    output$topic_input <- renderUI({
      numericInput(inputId = NS(id, "topicNumber")
                   , label = NULL
                   , value = as.numeric(max(r$df_topic$topic_id)) + 1)
    })
    
    observeEvent(input$saveTopic, {
      if(input$topicNumber %in% r$df_topic$topic_id){
        showNotification("Topic already exists.")
        removeModal()
      }else{
        df_topic <- r$df_topic
        new_row <- tibble("topic_id" = input$topicNumber
                          , "description" = input$topicDescription
        )
        df_review_table <- r$df_review_table
        new_column <- c(rep(NA, nrow(df_review_table)))
        new_column_name = paste("Topic", input$topicNumber)
        df_review_table <- df_review_table %>%
          mutate(init = new_column) 
        names(df_review_table)[names(df_review_table) == "init"] <- new_column_name
        new_df <- rbind(df_topic, new_row)
        r$df_topic <- new_df
        r$df_review_table <- df_review_table
      
        updateTextAreaInput(
          session = session
          , inputId = "topicDescription"
          , label = NULL
          , value = "New Description"
        )
        
        updateNumericInput(session = session
                           , inputId = "topicNumber"
                           , label = NULL
                           , value =  as.numeric(max(r$df_topic$topic_id)) + 1)
        
        showNotification("Saved in session.")
      }
      
    })

    output$review_idPicker <- renderUI({
      numericInput(inputId = NS(id, "reviewNumber")
                   , label = "Review Number: "
                   , value = max(r$df_review$id) + 1)
    })
    
    # Review topic selector 
    output$review_topics <- renderUI({
      choices <- r$df_topic$topic_id
      checkboxGroupInput("topics", "Topics", choices = choices)
    })
    
    # homework id picker
    output$homework_idPicker <- renderUI({
      numericInput(inputId = NS(id, "homeworkNumber")
                   , label = "Homework Number: "
                   , value = max(r$df_homework$id) + 1)
    })
    # BTN Save homework ----
    observeEvent(input$saveHomework, {
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
        new_row <- tibble("id" = input$homeworkNumber
                          , "description" = input$homeworkDescription
                          , "date_assigned" = input$homeworkDateAssigned
                          , "date_due" = input$homeworkDateDue
        )
        # Save and refresh 
        new_df <- rbind(df_homework, new_row)
        r$df_homework <- new_df
        
        df_homework_grades <- r$df_homework_grades
        new_column <- c(rep("NA", nrow(df_homework_grades)))
        new_column_name = paste("Homework", input$homeworkNumber)
        df_homework_grades <- df_homework_grades %>%
          mutate(init = new_column) 
        names(df_homework_grades)[names(df_homework_grades) == "init"] <- new_column_name
        
        # Save and refresh 
        r$df_homework_grades <- df_homework_grades 

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
                            , inputId = "homeworkDescription"
                            , label = "Homework Description: "
                            , value = " ")
        
        showNotification("Saved in session.")
        
      }
    })
    # BTN Add Student Save ----
    observeEvent(input$addStudent, {
      df_prev_student <- r$df_student
      new_row <- tibble("student_id" = input$addID
                        , "name" = input$addName
      )
      
      if(input$addID %in% r$df_student$student_id){
        updateNumericInput(
          session = session
          , inputId = "addID"
          , label = "Student ID: "
          , value = 801000000
        )
        showNotification("Student ID number already exists.", type = "error")
        
      }else{
        new_df <- rbind(r$df_student, new_row)
        r$df_student <- new_df
        homework_grades <- r$df_homework_grades

        if (ncol(homework_grades) == 1){
          temp <- tibble("Student Name" = input$addName)
        } else if (nrow(homework_grades) == 0){
          temp <- r$df_homework_grades[1,] %>%
            mutate(`Student Name` = input$addName) %>%
            mutate_at(.vars = c(2:ncol(r$df_homework_grades[1,])), .funs = as.character)
          temp[1,2:ncol(temp)] <- "NA"
        } else{
          temp <- r$df_homework_grades[1,] %>%
            mutate(`Student Name` = input$addName) 
          temp[1,2:ncol(temp)] <- "NA"
        }
        temp <-rbind(homework_grades, temp)
        r$df_homework_grades <- temp
        
        # Should not be the case if starting with an initial review (for the time being)
        if (nrow(r$df_review_table) != 0){
          a_student_id <-  r$df_student[1,1] %>%
            pull()
          
          review_grades <- r$df_review_grades
          # If first student being added to review_grades
          if (nrow(review_grades) == 0){
            df_review <- r$df_review_table %>%
              pivot_longer(cols = (4:ncol(r$df_review_table))) %>%
              filter(value == "TRUE")
            
            review_ids <- df_review %>%
              select(`Review ID`) %>%
              distinct() %>%
              pull()
            
            topics <- str_split_fixed(df_review$name, " ", n = 2)[,2]
            
            temp <- tibble(review_id = df_review$`Review ID`
                           , topic_id = topics
                           , student_id = rep(input$addID, length(topics))
                           , grade = rep("NA", length(topics)))
            
            r$df_review_grades <- temp
          } else{
            new_data <- review_grades %>%
              filter(student_id == a_student_id) %>%
              mutate(student_id = input$addID
                     , grade = "NA")
            
            temp <- rbind(review_grades, new_data) %>%
              arrange(review_id, topic_id)
            
            r$df_review_grades <- temp
          }
        }
        
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
        showNotification("Saved in session.")
      }
    })
    
    # BTN Save course info
    observeEvent(input$saveCourseInfo, {
      new_df <- tibble("location" = input$location
                       ,"meeting_times" = input$meeting_times
                       , "office_hours" = input$office_hours
                       , "link1_url" = input$link1_url
                       , "link2_url" = input$link2_url
                       , "link3_url" = input$link3_url
                       , "link1_text" = input$link1_text
                       , "link2_text" = input$link2_text
                       , "link3_text" = input$link3_text
      )
      r$df_course_info <- new_df
      showNotification("Saved in session.")
    })
    
  })
  
}