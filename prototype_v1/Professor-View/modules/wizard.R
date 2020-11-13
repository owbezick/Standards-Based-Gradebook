wizardUI <- function(id, doneButton){
  box(width = 12
      , tabsetPanel(id = NS(id, "wizard"), type = "hidden"
                    # Course info UI ----
                    , tabPanel(title = "Add Course Info", value = "courseInfo"
                               , "Course Information"
                               , br()
                               , fluidRow(
                                 box(width = 6, status = "primary", id = NS(id, "course_info_box")
                                     , fluidRow(column(width = 6
                                                       , textInput(
                                                         NS(id, "type")
                                                         , "Information Type"
                                                         , placeholder = "Class Location"
                                                       )
                                     )
                                     , column(width = 6
                                              , textInput(
                                                NS(id, "value")
                                                , "Information"
                                                , placeholder = "CHAM 1234"
                                              )
                                     )
                                     )
                                     , fluidRow(column(width = 12,actionBttn(NS(id, "saveCourseInput"), "Save")))
                                     
                                 )
                                 , box(width = 6, status = "primary", id = NS(id, "web_link_box")
                                       , fluidRow(column(width = 6
                                                         , textInput(
                                                           NS(id, "link_d")
                                                           , "Link Description"
                                                           , placeholder = "Class Moodle Page"
                                                         )
                                       )
                                       , column(width = 6
                                                , textInput(
                                                  NS(id, "link")
                                                  , "Link URL"
                                                  , placeholder = "moodle.com"
                                                )
                                       )
                                       )
                                       , fluidRow(
                                         column(width = 12
                                                , actionBttn(NS(id, "saveLinkInput"), "Save")
                                         )
                                       )
                                 )
                               )
                               # Buttons ----
                               , fluidRow(
                                 column(width = 6
                                        , actionBttn(
                                          NS(id, "skip")
                                          , "Skip to Mid-Semester View"
                                          , style = "material-flat"
                                          , block = T
                                        )
                                 )
                                 , column(width = 6
                                          , actionBttn(
                                            inputId = NS(id,"toGradeScale")
                                            , label = "Next"
                                            , style = "material-flat"
                                            , block = T
                                          )
                                 )
                               )
                    )
                    # Grade Scale ----
                    , tabPanel(title = "Edit Grade Scale", value = "gradeScale"
                               , "Grade Scale"
                               , br()
                               , fluidRow(
                                 column(width = 12
                                        , rHandsontableOutput(NS(id, "grade_scale_table"))
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
                                          inputId = NS(id,"saveGradeScale")
                                          , label = "Save Scale"
                                          , style = "material-flat"
                                          , block = T
                                        )
                                 )
                                 , column(width = 4
                                          , actionBttn(
                                            inputId = NS(id, "toRoster")
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
                                                  , value = "Student Name"
                                                )
                                          )
                                 )
                               )
                               # Buttons ----
                               , fluidRow(
                                 column(width = 4
                                        , actionBttn(
                                          inputId = NS(id,"backToGradeScale")
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
                                     , textInput(inputId = NS(id, "homeworkDescription")
                                                 , label = "Homework Description: "
                                                 , value = "Homework 1")
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
                                       , textInput(inputId = NS(id, "topicDescription")
                                                   , label = NULL
                                                   , value = "Topic 1"
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
                                                            , label = "Review Name: "
                                                            , value = "Review 1")
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
                                          , doneButton
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
    observeEvent(input$skip, {
      r$df_student <- readRDS("data/df_student.RDS")
      r$df_course_info <- readRDS("data/df_course_info.RDS")
      r$df_links <- readRDS("data/df_links.RDS")
      r$df_homework <- readRDS("data/df_homework.RDS")
      r$df_homework_grades <- readRDS("data/df_homework_grades.RDS")
      r$df_topic <- readRDS("data/df_topic.RDS")
      r$df_review_table <- readRDS("data/df_review_table.RDS")
      r$df_review_grades <-  readRDS("data/df_review_grades.RDS")
      removeModal()
      showNotification("Welcome!")
    })
    
    observeEvent(input$toGradeScale, {
      if (nrow(r$df_course_info) > 0 && nrow(r$df_links) > 0 ){
        updateTabsetPanel(session, "wizard", selected = "gradeScale")
      } else{
        showNotification("Please add at least one course information and link!", type = "error")
      }
    })
    
    observeEvent(input$toRoster, {
        updateTabsetPanel(session, "wizard", selected = "roster")
    })
    
    observeEvent(input$toHomework, {
      if (nrow(r$df_student) > 0){
        updateTabsetPanel(session, "wizard", selected = "homework")
      }else{
        showNotification("Please add at least one student!", type = "error")
      }
    })
    
    observeEvent(input$toTopics, {
      if (nrow(r$df_homework) > 0){
        updateTabsetPanel(session, "wizard", selected = "topics")
      }else{
        showNotification("Please add at least one homework!", type = "error")
      }
      
      
    })
    
    observeEvent(input$toReview, {
      if (nrow(r$df_topic) > 0){
        updateTabsetPanel(session, "wizard", selected = "review")
      }else{
        showNotification("Please add at least one topic!", type = "error")
      }
    })
    
    # Back Buttons ----
    observeEvent(input$toCourseInfo, {
      updateTabsetPanel(session, "wizard", selected = "courseInfo")
    })
    
    observeEvent(input$backToGradeScale, {
      updateTabsetPanel(session, "wizard", selected = "gradeScale")
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
    
    
    # Save Course Input ----
    observeEvent(input$saveCourseInput, {
      type <- input$type
      value <- input$value
      
      # Save
      r$df_course_info <- rbind(r$df_course_info
                                , tibble("Type" = c(type)
                                         , "Value" = c(value)))
      
      # Insert preview to UI
      insertUI(
        selector = paste0("#", NS(id, "course_info_box"))
        , where = "beforeBegin"
        , ui = box(title = NULL, width = 12
                   , fluidRow(
                     column(width = 12
                            , tags$b(paste0(type, ":"))
                            , value
                     )
                   )
        )
      )
      
      # Update inputs
      updateTextInput(
        session = session
        , inputId = "value"
        , value = "New"
      )
      updateTextInput(
        session = session
        , inputId = "type"
        , value = "New"
      )
    })
    
    # Save Weblink ----
    observeEvent(input$saveLinkInput, {
      type <- input$link_d
      value <- input$link
      
      # Save
      r$df_links <- rbind(r$df_links
                          , tibble("Link Description" = c(type)
                                   , "Link URL" = c(value)))
      
      # Insert preview to UI
      insertUI(
        selector = paste0("#", NS(id, "web_link_box"))
        , where = "beforeBegin"
        , ui = box(title = NULL, width = 12
                   , fluidRow(
                     column(width = 12
                            , div(tags$a(
                              type
                              , href =  value
                            )
                            )
                     )
                   )
        )
      )
      
      # Update inputs
      updateTextInput(
        session = session
        , inputId = "link_d"
        , value = "New"
      )
      updateTextInput(
        session = session
        , inputId = "link"
        , value = "New"
      )
    })
    
    # Grade Scale ----
    output$grade_scale_table <- renderRHandsontable({
      df_grade_scale <- r$df_grade_scale %>%
        select(`Level` = level, Title = title)
      
      rhandsontable(
        df_grade_scale
        , rowHeaders = NULL
        , stretchH = 'all'
        , readOnly = F
      ) %>%
        hot_col(col = c("Level"), type = "numeric")
    })
    
    observeEvent(input$saveGradeScale, {
      grade_scale <- hot_to_r(input$grade_scale_table)
      r$df_grade_scale <- grade_scale %>%
        select(level = Level, title = Title)
      
      showNotification("Saved in session.")
    })
    
    # Roster ----
    observeEvent(input$addStudent, {
      # Check to see if ID already exists
      if(input$addID %in% r$df_student$student_id){
        showNotification("Student ID number already exists.", type = "error")
        updateNumericInput(
          session = session
          , inputId = "addID"
          , label = "Student ID: "
          , value = 801000000
        )
      } else if (input$addName == "") {
        showNotification("Please include student name!", type = "warning")
      }
      else{
        # Save to df_student
        r$df_student <- rbind(r$df_student
                              , tibble(
                                "student_id" = input$addID
                                , "name" = input$addName
                              )
        )
        
        # Save to df_homework_grades If there are homeworks present, add in name and "NA" for grades
        if (ncol(r$df_homework_grades) > 1){
          temp <- r$df_homework_grades[1,]
          new_row <- temp %>%
            mutate(`Student Name` = input$addName) # Add in name
          new_row[1,2:ncol(new_row)] <- "NA"
          r$df_homework_grades <- rbind(r$df_homework_grades, new_row)
        }
        
        # Save to df_review_grades
        # Check to see if there are reviews
        if (nrow(r$df_review_table) != 0){
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
          } else{ # There are already students in the table
            a_student_id <-r$df_student[1,1] %>%
              pull()
            
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
          , value = "Student Name"
          , label = "Student Name: "
        )
        showNotification("Saved in session.")
      }
    })
    
    # Homework ----
    # homework id picker
    output$homework_idPicker <- renderUI({
      max_id <- suppressWarnings(max(r$df_homework$id))
      if(max_id == -Inf){
        value = 1
      } else {
        value = max_id + 1
      }
      numericInput(inputId = NS(id, "homeworkNumber")
                   , label = "Homework Number: "
                   , value = value)
    })
    
    # Save homework ----
    observeEvent(input$saveHomework, {
      
      # Check if HW id exists
      if (input$homeworkNumber %in% r$df_homework$id){
        showNotification("Homework ID already exists.", type = "error")
        updateNumericInput(
          session = session
          , inputId = "homeworkNumber"
          , label = "Homework Number: "
          , value =  max(r$df_homework$id) + 1
        )
      }else if (input$homeworkDateAssigned > input$homeworkDateDue) {
        showNotification("Assignment start date is after assignment end date.", type = "warning")
      }
      else{
        # Save to df_homework
        r$df_homework <- rbind(r$df_homework
                               , tibble(
                                 "id" = input$homeworkNumber
                                 , "description" = input$homeworkDescription
                                 , "date_assigned" = input$homeworkDateAssigned
                                 , "date_due" = input$homeworkDateDue
                               )
        )
        
        # Function assumes that r$df_homework has been refreshed
        save_df_homework_grades()
        
        # Update Inputs
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
        
        updateTextInput(session = session
                        , inputId = "homeworkDescription"
                        , label = "Homework Description: "
                        , value = paste("Homework", max(r$df_homework$id) + 1))
        
        showNotification("Saved in session.")
        
      }
    })
    
    # Topics ----
    output$topic_input <- renderUI({
      if(is.na(suppressWarnings(max(r$df_topic$topic_id)))){
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
        # Save to df_topic
        r$df_topic <- rbind(r$df_topic
                            , tibble("topic_id" = input$topicNumber
                                     , "description" = input$topicDescription
                            )
        )
        
        # Save to df_review_table
        new_column <- c(rep(NA, nrow(r$df_review_table )))
        new_column_name = paste("Topic", input$topicNumber)
        r$df_review_table  <- r$df_review_table  %>%
          mutate(init = new_column)
        names(r$df_review_table )[names(r$df_review_table ) == "init"] <- new_column_name
        
        updateTextInput(
          session = session
          , inputId = "topicDescription"
          , label = NULL
          , value = paste("Topic", max(r$df_topic$topic_id) + 1)
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
      max_id <- suppressWarnings(max(r$df_review$id))
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
      checkboxGroupInput(inputId = NS(id, "topics")
                         , label = "Topics"
                         , choices = choices
                         , selected = choices)
    })
    
    # Save review
    observeEvent(input$saveReview, {
   
      # Check if review ID exists:
      if (input$reviewNumber %in% r$df_review_grades$review_id){
        showNotification("Review ID already exists.", type = "error")
      } else if (is.na(input$topics)) {
        showNotification("Please add a topic", type = "error")
      }
      else if (input$reviewStartDate > input$reviewEndDate){
        showNotification("Assignment start date is after assignment end date.", type = "warning")
      }
      else{
        
        # Save to df_review_table ----
        selected_topics <- paste(
          rep("Topic", length(input$topics))
          , input$topics
        )
        topics <- unlist(colnames(r$df_review_table[,5:(ncol(r$df_review_table))]))
        if (is.null(topics)) {
          topic_df <- matrix(TRUE, ncol = length(selected_topics), nrow = 1)
          as_tibble(topic_df)
          colnames(topic_df) <- selected_topics
        }else{
          topic_df <- tibble(Topics = topics) %>%
            mutate(
              value = case_when(Topics %in% selected_topics ~ TRUE)
            ) %>%
            pivot_wider(names_from = Topics, values_from = value)
          
        }
        
        temp <- tibble(
          `Review Name` = input$reviewName
          ,`Review ID` = input$reviewNumber
          , `Review Start Date` = input$reviewStartDate
          , `Review End Date` = input$reviewEndDate) %>%
          cbind(topic_df)
        
        r$df_review_table <- rbind(r$df_review_table
                                   , temp
        )
        
        # Save to df_review_grades ----
        save_df_review_grades()
        
        # Update Inputs
        updateNumericInput(
          session = session
          , inputId = "reviewNumber"
          , label = "Review Number: "
          , value = max(r$df_review_grades$review_id) + 1
        )
        updateDateInput(
          session = session
          , inputId = "reviewStartDate"
          , label = "Date Assigned: "
          , value = Sys.Date()
        )
        updateDateInput(
          session = session
          , inputId = "reviewEndDate"
          , label = "Date Due: "
          , value = Sys.Date()
        )
        updateTextInput(session = session
                        , inputId = "reviewName"
                        , label = "Review Name "
                        , value = paste("Review", max(r$df_review_grades$review_id) + 1))
        showNotification("Saved in session")
      }
      
    })
    
  }) # End module server ----
  
}
