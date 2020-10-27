wizardUI <- function(id){
  box(width = 12
      , tabsetPanel(id = NS(id, "wizard"), type = "hidden"
                    # Course info UI ----
                    , tabPanel(title = "Add Course Info", value = "courseInfo"
                               , "Course Information"
                               , br()
                               , fluidRow(
                                 box(width = 6, status = "primary"
                                     , tags$b("Course Location: ")
                                     , textInput(inputId = NS(id,"location")
                                                 , label = NULL
                                                 , placeholder = "Class location")
                                     , br()
                                     , tags$b("Meeting times: ")
                                     , textInput(inputId = NS(id,"meeting_times")
                                                 , label = NULL
                                                 , placeholder = "Class meeting times")
                                     , br()
                                     , tags$b("Office hours: ")
                                     , textInput(inputId = NS(id,"office_hours")
                                                 , label = NULL
                                                 , placeholder = "Office hours schedule")
                                 )
                                 , box(width = 6, status = "primary"
                                       , column(width = 6
                                                , tags$b("Link URL: ")
                                                , textInput(inputId = NS(id,"link1_url")
                                                            , label = NULL
                                                            , placeholder = "Link URL")
                                                , br()
                                                , tags$b("Link URL: ")
                                                , textInput(inputId = NS(id,"link2_url")
                                                            , label = NULL
                                                            , placeholder = "Link URL")
                                                , br()
                                                , tags$b("Link URL: ")
                                                , textInput(inputId = NS(id,"link3_url")
                                                            , label = NULL
                                                            , placeholder = "Link URL")
                                       )
                                       , column(width = 6
                                                , tags$b("Link Description: ")
                                                , textInput(inputId = NS(id,"link1_text")
                                                            , label = NULL
                                                            , placeholder = "Link text")
                                                , br()
                                                , tags$b("Link Description: ")
                                                , textInput(inputId = NS(id,"link2_text")
                                                            , label = NULL
                                                            , placeholder = "Link text")
                                                , br()
                                                , tags$b("Link Description: ")
                                                , textInput(inputId = NS(id,"link3_text")
                                                            , label = NULL
                                                            , placeholder = "Link text")
                                       )
                                 )
                               )
                               # Buttons ----
                               , fluidRow(
                                 column(width = 4)
                                 , column(width = 4
                                          , actionBttn(inputId = NS(id,"saveCourseInfo")
                                                       , label = "Save"
                                                       , style = "material-flat"
                                                       , block = T
                                          )
                                 )
                                 , column(width = 4
                                          , actionBttn(
                                            inputId = NS(id,"toRoster")
                                            , label = "Next"
                                            , style = "material-flat"
                                            , block = T
                                          )
                                 )
                               )
                    )
                    # Roster UI ----
                    , tabPanel(value = "roster", title = "Add Roster"
                               , "Roster"
                               , br()
                               , fluidRow(
                                 box(width = 6, status = "primary"
                                     , numericInput(
                                       inputId = NS(id, "addID")
                                       , label = "Student ID: "
                                       , value = 801000000
                                     )   
                                 )
                                 , column(width = 6, status = "primary"
                                          , box(width = 12, status = "primary"
                                                , textInput(
                                                  inputId = NS(id, "addName")
                                                  , label = "Student Name: "
                                                )
                                          )
                                 )
                               )
                               # Buttons ----
                               , fluidRow(
                                 column(width = 4
                                        , actionBttn(
                                          inputId = NS(id,"toCourseInfo")
                                          , label = "Back"
                                          , style = "material-flat"
                                          , block = T
                                        )
                                 )
                                 , column(width = 4
                                          , actionBttn(
                                            inputId = NS(id,"addStudent")
                                            , label = "Save"
                                            , style = "material-flat"
                                            , block = T
                                          )
                                 )
                                 ,    column(width = 4
                                             , actionBttn(
                                               inputId = NS(id,"toHomework")
                                               , label = "Next"
                                               , style = "material-flat"
                                               , block = T
                                             )
                                 )
                               )
                    )
                    # Homework UI ----
                    , tabPanel(value = "homework", title = "Add Homeworks"
                               , "Homework"
                               , br()
                               , fluidRow(
                                 box(width = 6, status = "primary"
                                     , uiOutput(NS(id, "homework_idPicker"))
                                     , br()
                                     , textAreaInput(inputId = NS(id, "homeworkDescription")
                                                     , label = "Homework Description: ")
                                 )
                                 , box(width = 6, status = "primary"
                                       , div(class = "date_inputs", id = "date-inputs"
                                             , dateInput(inputId = NS(id, "homeworkDateAssigned")
                                                         , label = "Date Assigned: ")
                                             , br()
                                             , dateInput(inputId = NS(id, "homeworkDateDue")
                                                         , label = "Date Due: ")
                                       )
                                 )
                               )
                               # Buttons ----
                               , fluidRow(
                                 column(width = 4
                                        , actionBttn(
                                          inputId = NS(id,"backToRoster")
                                          , label = "Back"
                                          , style = "material-flat"
                                          , block = T
                                        )
                                 )
                                 , column(width = 4
                                          , actionBttn(
                                            inputId = NS(id,"saveHomework")
                                            , label = "Save"
                                            , style = "material-flat"
                                            , block = T
                                          )
                                 )
                                 , column(width = 4
                                          , actionBttn(
                                            inputId = NS(id,"toTopics")
                                            , label = "Next"
                                            , style = "material-flat"
                                            , block = T
                                          )
                                 )
                               )
                    )
                    # Topics UI ----
                    , tabPanel(value = "topics", title = "Add Topics"
                               , "Topics"
                               , br()
                               , fluidRow(
                                 box(width = 2, status = "primary"
                                     , tags$b("Topic Number: ")
                                     , uiOutput(NS(id, "topic_input"))
                                 )
                                 , box(width = 10, status = "primary"
                                       , tags$b("Topic Description: ")
                                       , textAreaInput(inputId = NS(id, "topicDescription")
                                                       , label = NULL
                                       )
                                 )
                               )
                               # Buttons ----
                               , fluidRow(
                                 column(width = 4
                                        , actionBttn(
                                          inputId = NS(id,"backToHomework")
                                          , label = "Back"
                                          , style = "material-flat"
                                          , block = T
                                        )
                                 )
                                 , column(width = 4
                                          , actionBttn(
                                            inputId = NS(id,"saveTopic")
                                            , label = "Save"
                                            , style = "material-flat"
                                            , block = T
                                          )
                                 )
                                 , column(width = 4
                                          , actionBttn(
                                            inputId = NS(id,"toReview")
                                            , label = "Next"
                                            , style = "material-flat"
                                            , block = T
                                          )
                                 )
                               )
                    )
                    # Review UI ----
                    , tabPanel(value = "review", title = "Add Reviews"
                               , "Reviews"
                               , br()
                               , fluidRow(
                                 box(width = 6, status = "primary"
                                     , fluidRow(
                                       column(width = 4
                                              , uiOutput(NS(id, "review_idPicker"))
                                              , br()
                                       )
                                       , column(width = 8
                                                , textInput(inputId = NS(id, "reviewName")
                                                            , label = "Review Name: ")
                                                , br()
                                       )
                                     ) 
                                     , fluidRow(
                                       column(width = 6
                                              , div(class = "date_inputs", id = "date-inputs"
                                                    , dateInput(inputId = NS(id, "reviewStartDate")
                                                                , label = "Review Start Date: ")
                                              )
                                       )
                                       , column(width = 6
                                                , div(class = "date_inputs", id = "date-inputs"
                                                      , dateInput(inputId = NS(id, "reviewEndDate")
                                                                  , label = "Review End Date: ")
                                                )
                                       )
                                     )
                                 )
                                 , box(width = 6, status = "primary"
                                       , uiOutput(NS(id, "review_topics"))
                                 )
                               )
                               # Buttons ----
                               , fluidRow(
                                 column(width = 4
                                        , actionBttn(
                                          inputId = NS(id,"backToTopics")
                                          , label = "Back"
                                          , style = "material-flat"
                                          , block = T
                                        )
                                 )
                                 , column(width = 4
                                          , actionBttn(
                                            inputId = NS(id,"saveReview")
                                            , label = "Save"
                                            , style = "material-flat"
                                            , block = T
                                          )
                                 )
                                 , column(width = 4
                                          , actionBttn(
                                            inputId = NS(id,"closeWizard")
                                            , label = "Close Wizard"
                                            , style = "material-flat"
                                            , block = T
                                          )
                                 )
                               )
                    )
      )
  )
}

wizard_server <- function(id, r, parent_session) {
  
  `%notin%` <- Negate(`%in%`)
  
  moduleServer(id, function(input, output, session){
    # Next Buttons ----
    observeEvent(input$toRoster, {
      updateTabsetPanel(session, "wizard", selected = "roster")
    })
    
    observeEvent(input$toHomework, {
      if (nrow(r$df_student) > 0){
        updateTabsetPanel(session, "wizard", selected = "homework")
      }else{
        showNotification("Please add at least one student!")
      }
    })
    
    observeEvent(input$toTopics, {
    
        updateTabsetPanel(session, "wizard", selected = "topics")
     
    })
    
    observeEvent(input$toReview, {
      if (nrow(r$df_topic) > 0){
      updateTabsetPanel(session, "wizard", selected = "review")
      }else{
        showNotification("Please add at least one topic!")
      }
    })
    
    # Back Buttons ----
    observeEvent(input$toCourseInfo, {
      updateTabsetPanel(session, "wizard", selected = "courseInfo")
    })
    
    observeEvent(input$backToRoster, {
      updateTabsetPanel(session, "wizard", selected = "roster")
    })
    
    observeEvent(input$backToHomework, {
      updateTabsetPanel(session, "wizard", selected = "homework")
    })
    
    observeEvent(input$backToTopics, {
      updateTabsetPanel(session, "wizard", selected = "topics")
    })
    
    # Close Wizard Button ----
    observeEvent(input$closeWizard, {
      removeModal()
      
    })
    # Save Course info ----
    observeEvent(input$saveCourseInfo, {
      course_info <- tibble("location" = input$location
                            ,"meeting_times" = input$meeting_times
                            , "office_hours" = input$office_hours
                            , "link1_url" = input$link1_url
                            , "link2_url" = input$link2_url
                            , "link3_url" = input$link3_url
                            , "link1_text" = input$link1_text
                            , "link2_text" = input$link2_text
                            , "link3_text" = input$link3_text
      )
      
      r$df_course_info <- course_info
      showNotification("Saved in session.")
    })
    
    # Roster ----
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
    
    # Homework ----
    # homework id picker
    output$homework_idPicker <- renderUI({
      max_id <- max(r$df_homework$id)
      if(max_id == -Inf){
        value = 1
      } else {
        value = max_id + 1
      }
      numericInput(inputId = NS(id, "homeworkNumber")
                   , label = "Homework Number: "
                   , value = value)
    })
    # Save homework
    observeEvent(input$saveHomework, {
      df_homework <- r$df_homework
      # Check if HW id exists
      if(input$homeworkNumber %in% df_homework$id){
        showNotification("Homework ID already exists.", type = "error")
        updateNumericInput(
          session = session
          , inputId = "homeworkNumber"
          , label = "Homework Number: "
          , value =  max(r$df_homework$id) + 1
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
    
    # Topics ----
    output$topic_input <- renderUI({
      if(is.na(max(r$df_topic$topic_id))){
        value = 1
      } else {
        value = as.numeric(max(r$df_topic$topic_id)) + 1
      }
      numericInput(inputId = NS(id, "topicNumber")
                   , label = NULL
                   , value =  + 1)
    })
    
    observeEvent(input$saveTopic, {
      if(input$topicNumber %in% r$df_topic$topic_id){
        showNotification("Topic ID already exists.")
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
    
    # Reviews ----
    output$review_idPicker <- renderUI({
      max_id <- max(r$df_review$id)
      if(max_id == -Inf){
        value = 1
      } else {
        value = max_id + 1
      }
      numericInput(inputId = NS(id, "reviewNumber")
                   , label = "Review Number: "
                   , value = value)
    })
    
    # Review topic selector 
    output$review_topics <- renderUI({
      choices <- r$df_topic$topic_id
      checkboxGroupInput(inputId = NS(id, "topics"), label = "Topics", choices = choices)
    })
    
    # Save review
    observeEvent(input$saveReview, {
      ls_topics <- input$topics
      topic <- tibble("topic_id" = ls_topics
                      , topic = rep("Topic", length(ls_topics)
                      ))
      topic_names <- topic %>%
        mutate(name = paste(topic, topic_id)) %>%
        select(name) %>%
        pull()
      
      # Review Table
      review_table <- tibble(`Review Name` = input$reviewName
                             , `Review ID` = input$reviewNumber
                             , `Review Start Date` = input$reviewStartDate
                             , `Review End Date` = input$reviewEndDate)
      
      review_table <- cbind(review_table, setNames( lapply(topic_names, function(x) x=TRUE), topic_names) )
      r$df_review_table <- review_table
      
      # Review Grades
      df_review_grades <- r$df_review_grades
      
      df_student <- r$df_student %>%
        select(student_id)
      
      data_from_hot <- review_table %>%
        pivot_longer(cols = c(5:ncol(review_table))) %>%
        na.omit()
      
      review_topic_id_hot <- data_from_hot %>%
        filter(value == "TRUE")
      
      review_topic_id_hot <- review_topic_id_hot %>%
        mutate(topic_id = str_split_fixed(review_topic_id_hot$name, " ", 2)[,2]) %>%
        select(review_id = `Review ID`, topic_id)
      
      number_of_topics <- nrow(review_topic_id_hot)
      # replicate review_id and topic_id for as many students that are in the class
      new_review_topic_rep <- do.call("rbind", replicate(nrow(df_student), review_topic_id_hot, simplify = FALSE))
      
      # replicate student_id the for as many topics being added
      student_id <- do.call("rbind", replicate(nrow(review_topic_id_hot), df_student, simplify = FALSE)) %>%
        arrange(student_id)
      
      df_review_grades <- df_review_grades %>%
        filter(grade != "NA") %>%
        mutate(
          filter_id = paste(review_id, topic_id, student_id)
        )
      
      df_new_review_data <- new_review_topic_rep %>%
        mutate(student_id = student_id$student_id
               , grade = rep("NA", nrow(new_review_topic_rep))
               , filter_id = paste(review_id, topic_id, student_id)) %>%
        filter(filter_id %notin% df_review_grades$filter_id) %>%
        rbind(df_review_grades) %>%
        arrange(review_id, topic_id) %>%
        select(-c(filter_id))
      
      
      # Refresh and save data ----
      r$df_review_grades <- df_new_review_data
      showNotification("Saved to remote.")
      removeModal()
    })
  }) # End module server ----
  
}