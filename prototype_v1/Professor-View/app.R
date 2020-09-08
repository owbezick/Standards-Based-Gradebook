# Mastery Gradebook application - Professor View
# Author: Owen Bezick

# Source Libraries
source("libraries.R", local = TRUE)
source("data_intake.R", local = TRUE)
source("utils.R", local = TRUE)

# Define UI
ui <- dashboardPage(skin = "black"
                    , dashboardHeader(title = "Professor View" 
                                      , tags$li(class = "dropdown", tags$img(height = "40px", src='davidson_logo_white.png', hspace = "4", vspace ="4")
                                      ))
                    # Sidebar ----
                    , dashboardSidebar( 
                        sidebarMenu(
                            menuItem(tabName = "home", text = "Home", icon = icon("home"))
                            , menuItem(tabName ="viewGrades", text = "View Grades", icon = icon("chalkboard")
                                       , menuSubItem(tabName = "examGrades", text = "View Exam Grades")
                                       , menuSubItem(tabName = "homeworkGrades", text = "View Homework Grades")
                            )
                            , menuItem(tabName ="editGrades", text = "Edit Grades", icon = icon("chalkboard-teacher")
                                       , menuSubItem(tabName = "editReviewGrades", text = "Edit Exam Grades")
                                       , menuSubItem(tabName = "editHomeworkGrades", text = "Edit Homework Grades")
                            )
                        )
                    )
                    , dashboardBody(
                        # Custom CSS Formating ----
                        tags$head(
                            tags$style(
                                HTML('
                          .skin-red .main-header .logo {
                            background-color: #d73925;
                            color: #000;
                            border-bottom: 0 solid transparent;
                          }
                          .skin-black .main-header .navbar {
                            background-color:#1f2d33;
                          }
                          .skin-black .main-header .navbar>.sidebar-toggle {
                          color: #eee;
                          }
                          .box.box-danger {
                          border-top-color: #ac1a2f;
                          }
                          .small-box.bg-red{ 
                          background-color: #ac1a2f !important; color: #eee !important; 
                          }
                          .skin-black .main-header>.logo {
                          background-color: #ac1a2f;
                          color: #ffffff;
                          border-bottom: 0 solid transparent;
                          border-right: 1px solid #eee;
                          }
                          .main-header .logo {
                          display: block;
                          float: left;
                          height: 50px;
                          font-size: 17px;
                          line-height: 50px;
                          text-align: center;
                          width: 230px;
                          font-family: "Rubik",Helvetica,Arial,Lucida,sans-serif;
                          padding: 0 15px;
                          font-weight: 300;
                          overflow: hidden;
                          }
                          .h3 {
                          font-family: "Rubik",Helvetica,Arial,Lucida,sans-serif;
                          }
                          .body {
                          font-family: "Rubik",Helvetica,Arial,Lucida,sans-serif;
                          }
                          .nav-tabs-custom>.nav-tabs>li.active {
                            border-top-color: #ac1a2f;
                          }
                            .bttn-fill.bttn-danger {
                            background: #ac1a2f;
                            color: #fff;
                            }
                            ')
                            )
                        )
                        , tabItems(
                            # Home Tab ----
                            tabItem(
                                tabName = "home"
                                , HTML("<center><h1> Mastery Gradebook Dashboard </h1></center>")
                                , column(width = 3
                                         ,box(width = 12, status = "danger"
                                              , tags$head(tags$style(HTML(".small-box {height: 85px}")))
                                              , valueBoxOutput("total_exams", width = 12)
                                              , valueBoxOutput("total_homeworks", width = 12)
                                         )
                                )
                                , column(width = 9
                                         , fluidRow(box(width = 12, status = "danger", title = "Assignment Schedule"
                                                        , HTML("Select an assignment for details")
                                                        , timevisOutput("gantt")
                                         )
                                         )
                                )
                            )
                            # View Exams ----
                            , tabItem(
                                tabName = "examGrades"
                                ,fluidRow(
                                    box(width = 12, title = "Filter:", status = "danger" 
                                        ,column(width = 4
                                                ,uiOutput("examStudentPicker")
                                        )
                                        , column(width = 4
                                                 ,uiOutput("examPicker")
                                        )
                                        , column(width = 4
                                                 ,uiOutput("topicPicker")
                                        )
                                    )
                                )
                                , fluidRow(
                                    tabBox(title = "Exam Data", width = 12
                                           , tabPanel(title = "All Grades", width = 12
                                               , fluidRow(
                                                   box(width = 6, status = "danger", title = "All Topic Grades"
                                                            , DTOutput("totalExamGrades")
                                               )
                                               , box(width = 6, stauts = "danger", title = "Top Grades", status = "danger"
                                                     , echarts4rOutput("gradeBar")
                                               )
                                               )
                                           )
                                           , tabPanel(title = "By Question", width = 12
                                                      , fluidRow(
                                                          box(width = 6, status = "danger", title = "Topic Masteries"
                                                              , DTOutput("byQuestionDT")
                                                          )
                                                          , box(width = 6, stauts = "danger", title = "Topic Masteries", status = "danger"
                                                                , echarts4rOutput("questionBar")
                                                          )
                                                      )
                                                      )
                                           , tabPanel(title = "By Student", width = 12
                                                      , fluidRow(
                                                          box(width = 6, status = "danger", title = "Student Masteries"
                                                              , DTOutput("byStudentDT")
                                                          )
                                                          , box(width = 6, stauts = "danger", title = "Student Masteries", status = "danger"
                                                                , echarts4rOutput("studentBar")
                                                          )
                                                      )
                                                      )
                                    )
                                )
                            )
                            # View Homeworks ----
                            , tabItem(
                                tabName = "homeworkGrades"
                                , fluidRow(
                                    box(width = 12, title = "Filter:", status = "danger" 
                                        ,column(width = 6
                                                , uiOutput("hwStudentPicker")
                                        )
                                        , column(width = 6
                                                 ,uiOutput("hwPicker")
                                        )
                                    )
                                )
                                , fluidRow(
                                    box(width = 6, status = "danger",  title = "Homework Grades", height = "550"
                                        , DTOutput("homeworkGradeTable")
                                    )
                                    , box(width = 6, status = "danger", title = "Homework Averages", height = "550"
                                          , echarts4rOutput("avgHomeworkGraph")
                                    )
                                )
                            )
                            # Edit Exams ---- 
                            , tabItem(
                                tabName = "editReviewGrades"
                                , fluidRow(
                                    column(width = 6
                                           , actionBttn(inputId = "addExam", label = "Add Exam", style = "fill", color = "danger", block = T)
                                           )
                                    , column(width = 6
                                             , actionBttn(inputId = "editExam", label = "Edit Exam", style = "fill", color = "danger", block = T)
                                             )
                                )
                                , fluidRow(
                                    box(width = 12, status = "danger", title = "Edit Exam Grades"
                                        , column(width = 12
                                                 , DTOutput("edit_exam_dt")
                                        )
                                    )
                                )
                            )
                            # Edit Homeworks ----
                            , tabItem(
                                tabName = "editHomeworkGrades"
                                , fluidRow(
                                    column(width = 6
                                           , actionBttn(inputId = "addHW", label = "Add Homework", style = "fill", color = "danger", block = T)
                                    )
                                    , column(width = 6
                                             , actionBttn(inputId = "editHW", label = "Edit Homework", style = "fill", color = "danger", block = T)
                                    )
                                )
                                , fluidRow(
                                    box(width = 12, status = "danger", title = "Edit Homework Grades"
                                        , DTOutput("editHomeworkGrades")
                                    )
                                )
                            )
                            
                        )
                    )
)


# Define server logic 
server <- function(input, output) {
    output$total_exams <- renderValueBox({
        value <- max(reactive$exam_def$exam_id)
        valueBox(value, subtitle = "Exams", color= "red")
    })
    output$total_homeworks <- renderValueBox({
        value <- reactive$homework_def %>%
            nrow()
        valueBox(value, subtitle = "Homeworks", color= "red")
    })
    
    # Schedule ----
    output$gantt <- renderTimevis({
        exams <- reactive$exam_def %>%
            mutate(content = paste("Exam", exam_id)) %>%
            mutate(id = paste0("E", exam_id)) %>%
            select(content = content, start = date, id = id)
        homeworks <- reactive$homework_def %>%
            mutate(content = paste("Homework", homework_id))%>%
            mutate(id = paste0("H", homework_id)) %>%
            select(content = content, start = date, id = id)
        
        assignments <- rbind(exams,homeworks)
        timevis(assignments)
    })
    
    output$schedule <- renderUI({ 
        req(is$auth)
        fluidRow(column(width = 12
                        , box(width = 12, title = "Assignment Schedule", status = "danger"
                              , HTML("Select an assignment for details")
                              , timevisOutput("gantt")
                        )
        )
        )
    })
    
    observeEvent(input$gantt_selected,{
        input <- input$gantt_selected
        assignemnt_type <- substr(input,1,1)
        assignment_id <- as.numeric(substr(input, 2,2))
        if(assignemnt_type == "H"){
            df <- homework_def %>%
                filter(homework_id == assignment_id)
            
            showModal(
                modalDialog(title = "Homework Details"
                            , box(width = 12, status = "danger"
                                  , column(width = 12
                                           , fluidRow(
                                               column(width = 12
                                                      , HTML("<b> Homework ID: </b>")
                                                      , df$homework_id
                                               )
                                           )
                                           , fluidRow(
                                               column(width = 12
                                                      , HTML("<b> Homework Description: </b>")
                                                      , df$description
                                               )
                                           )
                                           , fluidRow(
                                               column(width = 12
                                                      , HTML("<b> Homework Date: </b>")
                                                      , df$date
                                               )
                                           )
                                  )
                            )
                )
            )
        } else{
            df <- exam_def %>%
                filter(exam_id == assignment_id)
            showModal(
                modalDialog(title = "Exam Details", footer = NULL, easyClose = T
                            , box(width = 12, status = "danger"
                                  , column(width = 12
                                           , fluidRow(
                                               column(width = 12
                                                      , HTML("<b> Exam ID: </b>")
                                                      , df$exam_id
                                               )
                                           )
                                           , fluidRow(
                                               column(width = 12
                                                      , HTML("<b> Topics Covered: </b>")
                                               )
                                               , column(width = 6
                                                        , HTML("<b> First: </b>")
                                                        , df$first_topic)
                                               , column(width = 6
                                                        , HTML("<b> Last: </b>")
                                                        , df$last_topic)
                                           )
                                           , fluidRow(
                                               column(width = 12
                                                      , HTML("<b> Exam Date: </b>")
                                                      , df$date
                                               )
                                           )
                                  )
                            )
                )
            )
        }
    })
    
    # View Exam Server ---- 
    # List of students by ID
    ls_studentsR <- reactive({
        df <- exam_grades()
        df %>% distinct(firstLast) %>% pull()
    })
    
    
    # List of exams by id
    ls_examsR <- reactive({
        df <- exam_grades()
        df %>% distinct(exam_id) %>% 
            pull()
    })
    # List of topic numbers
    ls_topicsR <- reactive({
        df <- exam_grades()
        ls_topics <- df %>% distinct(topic_id) %>% 
            pull()
        ls_topics <- sort(ls_topics, decreasing = F)
    })
    
    # Student Picker
    output$examStudentPicker <- renderUI({
        pickerInput("examStudentPicker"
                    ,"Student"
                    , choices = ls_studentsR()
                    , selected = ls_studentsR()
                    , multiple = TRUE
                    , options = list(
                        `actions-box` = TRUE,
                        `deselect-all-text` = "Deselect All",
                        `select-all-text` = "Select All")
        )
    })
    
    # Exam Picker
    output$examPicker <- renderUI({
        pickerInput("examPicker"
                    ,"Exam by ID"
                    , choices = ls_examsR()
                    , selected = ls_examsR()
                    , multiple = TRUE
                    , options = list(
                        `actions-box` = TRUE,
                        `deselect-all-text` = "Deselect All",
                        `select-all-text` = "Select All")
        )
    })
    
    # Topic Picker
    output$topicPicker <- renderUI({
        pickerInput("topicPicker"
                    ,"Topic by ID"
                    , choices = ls_topicsR()
                    , selected = ls_topicsR()
                    , multiple = TRUE
                    , options = list(
                        `actions-box` = TRUE,
                        `deselect-all-text` = "Deselect All",
                        `select-all-text` = "Select All")
        )
    })
    
    filtered_exam_data <- reactive({
        req(input$examStudentPicker, input$examPicker, input$topicPicker)
        df <- exam_grades()
        df <- df %>%
            filter(exam_id %in% input$examPicker, firstLast %in% input$examStudentPicker, topic_id %in% input$topicPicker)
    })
    
    # Total Grades ----
    # DT Exam Grade Output 
    output$totalExamGrades <- renderDT({
        df <- filtered_exam_data() %>%
            select( Name = firstLast, `Exam ID` = exam_id, Topic = topic_id, Grade = grade)
        datatable(df, rownames = FALSE)
    })
    
    # Total Grades Chart-- visualizes review data from the class
    output$gradeBar <- renderEcharts4r({
        df <- filtered_exam_data() 
        df <- df %>%
            group_by(firstLast, topic_id) %>%
            summarise(grade = grade_max(grade)) %>%
            select(grade) %>%
            filter(grade != "NA") %>%
            count(grade) %>%
            ungroup()
        
        apprentice <- df %>%
            filter(grade == "A") %>%
            summarise(total = sum(n)) %>%
            pull()
        journey <- df %>%
            filter(grade == "J") %>%
            summarise(total = sum(n)) %>%
            pull()
        master <- df %>%
            filter(grade == "M") %>%
            summarise(total = sum(n)) %>%
            pull()
        
        graph_df <- tibble(A = c(0 + as.numeric(apprentice[1]))
                           , J = c(0 + as.numeric(journey[1]))
                           , M = c(0 + as.numeric(master[1]))
                           , chart = c(""))
        graph_df %>%
            e_chart(chart) %>%
            e_bar("A", name = "Apprentice") %>%
            e_bar("J", name = "Journeyman")  %>%
            e_bar("M", name = "Master") %>%
            e_theme("dark") %>%
            e_tooltip() %>%
            e_legend(bottom = 0)
    })
    
    # By Question ----
    output$byQuestionDT <- renderDT({
        df <- filtered_exam_data() 
        df <- df %>%
            group_by(topic_id) %>%
            count(grade) %>%
            ungroup() %>%
            filter(grade =="M") %>%
            select(`Topic ID` = topic_id, `Mastery Count` = n)
        datatable(df, rownames = FALSE)
    })
    # Total Grades Chart-- visualizes review data from the class
    output$questionBar <- renderEcharts4r({
        df <- filtered_exam_data() 
        df <- df %>%
            group_by(topic_id) %>%
            count(grade) %>%
            ungroup() %>%
            filter(grade =="M")
        
        df %>%
            e_chart(topic_id) %>%
            e_bar("n", name = "Total Mastered") %>%
            e_tooltip() %>%
            e_legend(show = F) %>%
            e_theme("dark")
    })
    # By Student ----
    output$byStudentDT <- renderDT({

        df <- filtered_exam_data() 
        df <- df %>%
            group_by(firstLast, topic_id) %>%
            summarise(grade = grade_max(grade)) %>%
            count(grade) %>%
            ungroup() %>%
            filter(grade =="M") %>%
            select(Name = firstLast, `Total Mastered` = n)
        datatable(df, rownames = FALSE)
    })
    # Total Grades Chart-- visualizes review data from the class
    output$studentBar <- renderEcharts4r({
        df <- filtered_exam_data() 
        df <- df %>%
            group_by(firstLast, topic_id) %>%
            summarise(grade = grade_max(grade)) %>%
            count(grade) %>%
            ungroup() %>%
            filter(grade =="M") 
        
        df %>%
            e_chart(firstLast) %>%
            e_bar("n", name = "Total Mastered") %>%
            e_tooltip() %>%
            e_legend(show = F) %>%
            e_theme("dark")
    })
    
    # View Homeworks Server -----
    # List of students by firstLast
    ls_studentsHW <- reactive({
        df <- homework_grades()
        df %>% distinct(firstLast) %>% pull()
    })
    
    #List from homework
    ls_homeworksHW <- reactive({
        df <- homework_grades()
        df %>% distinct(homework_id) %>% pull()
    })
    
    # Student Picker
    output$hwStudentPicker <- renderUI({
        pickerInput("hwStudentPicker"
                    ,"Student"
                    , choices = ls_studentsHW()
                    , selected = ls_studentsHW()
                    , multiple = TRUE
                    , options = list(
                        `actions-box` = TRUE,
                        `deselect-all-text` = "Deselect All",
                        `select-all-text` = "Select All")
        )
    })
    
    # Homework Picker
    output$hwPicker <- renderUI({
        pickerInput("hwPicker"
                    ,"Homework by ID"
                    , choices = ls_homeworksHW()
                    , selected = ls_homeworksHW()
                    , multiple = TRUE
                    , options = list(
                        `actions-box` = TRUE,
                        `deselect-all-text` = "Deselect All",
                        `select-all-text` = "Select All")
        )
    })
    
    # homwork data
    output$homeworkGradeTable <- renderDT({
        req(input$hwStudentPicker, input$hwPicker)
        df <- homework_grades()
        df  <- df %>% 
            filter(firstLast %in% input$hwStudentPicker) %>%
            filter(homework_id %in% input$hwPicker) %>%
            select(Name = firstLast, `Homework ID` = homework_id, Grade= grade)
        
        datatable(df, rownames = FALSE)
    })
    
    # HW AVG Data
    hwAvg <- reactive({
        req(input$hwStudentPicker, input$hwPicker)
        df <- homework_grades()
        df  <- df %>% 
            filter(firstLast %in% input$hwStudentPicker) %>%
            filter(homework_id %in% input$hwPicker) %>%
            group_by(firstLast) %>%
            mutate(homeworkAvg = mean(grade)/100)
    })
    
    # AVG hw graph
    output$avgHomeworkGraph <- renderEcharts4r({
        df <- hwAvg()
        df %>%
            e_chart(last) %>%
            e_scatter(homeworkAvg, symbol_size = 10) %>%
            e_theme("dark") %>%
            e_tooltip(formatter = e_tooltip_item_formatter(
                style = c("percent"),
                digits = 2
            )
            ) %>%
            e_x_axis(axisLabel = list(interval = 0, rotate = 45)) %>%
            e_y_axis(formatter = e_axis_formatter(
                style = c("percent"),
                digits = 2,
            )
            ) %>%
            e_legend(show = F)
    })
    
    # Edit Exam Server ----
    output$edit_exam_dt <- renderDT({
        df <- exam_grades() %>%
            select(Name = firstLast, `Exam Id` = exam_id, Topic = topic_id, Grade = grade)
        datatable(df, rownames = FALSE
                  , selection = list(mode = 'single', target = 'row')
                  , filter = 'top', caption = "Click a Row to Edit")
    })
    
    # Creates interactive box to input changes
    observeEvent(input$edit_exam_dt_rows_selected,{
        rowNumber <- input$edit_exam_dt_rows_selected
        df <- exam_grades()
        rowData <- df[rowNumber, ]
        showModal(
            modalDialog(title = "Edit Grade", easyClose = T
                        ,box(width = 12, status = "danger"
                             , HTML("<b> Name: </b>")
                             , renderText(paste(rowData$firstLast))
                             , HTML("<b> Topic ID: </b>")
                             , renderText(rowData$topic_id)
                             , pickerInput("grade", "Grade:", choices = c("M", "J", "A", "NA")
                                           , selected = as.character(rowData$grade))
                        )
                        , footer = fluidRow(
                            column(width = 6
                                   , actionBttn("gradeSave"
                                                , "Save"
                                                , icon = icon("save")
                                                , style = "material-flat"
                                                , block = T
                                   )
                            )
                            , column(width = 6
                                     , actionBttn("gradeDismiss"
                                                  , "Dismiss"
                                                  , icon = icon("close")
                                                  , style = "material-flat"
                                                  , block = T)
                            )
                        )
            )
        )
    })
    
    # When the "Grade Dismiss" button is pressed
    observeEvent(input$gradeDismiss,{
        removeModal()
    })
    
    #When the "Save Grade" button is pressed
    observeEvent(input$gradeSave,{
        rowNumber <- input$edit_exam_dt_rows_selected
        df <- exam_grades()
        rowData <- df[rowNumber, ]
        topic_id <- rowData$topic_id
        newGrade <- as.character(input$grade)
        exam_id <- rowData[1, 3]
        
        df <- student_def %>%
            filter(firstLast == rowData$firstLast)
        
        student_id <- df$student_id
        
        # Write to Database
        sql_query <- paste0("update Shiny.dbo.exam_grade set grade = '", newGrade, "' where (topic_id = ", topic_id, " and student_id = ", student_id, " and exam_id = ", exam_id, ")")
        dbExecute(con, sql_query)
        
        # Background App Refresh
        sql_query <- 'Select * from Shiny.dbo.exam_grade'
        df_examGrades <- dbGetQuery(con, sql_query)
        reactive$exam_grade <- df_examGrades
        
        showNotification("Changes Saved to Remote Database.", type = c("message"), duration = 3)
        removeModal()
    })
    
    
    # Add Exam
    observeEvent(input$addExam, {
        showModal(
            modalDialog(title = "Add an Exam",  easyClose = T
                        , box(width = 12, status = "danger", title = "Exam Information"
                              , fluidRow(
                                  column(width = 6
                                         , numericInput(inputId = "add_exam_id", label = "Review ID", value = 1 + max(exam_def$exam_id))
                                         , numericInput(inputId = "add_exam_first", label = "First Topic", value = 1 + max(exam_def$first_topic))
                                  )
                                  , column(width = 6
                                           , dateInput(inputId = "add_exam_date", label = "Date Assigned", value = 1 + max(exam_def$date))
                                           , numericInput(inputId = "add_exam_last", label = "Last Topic", value = 1 + max(exam_def$last_topic))
                                  )
                              )
                        )
                        , footer = fluidRow(
                            column(width = 6
                                   , actionBttn("saveExam"
                                                , "Save"
                                                , icon = icon("save")
                                                , style = "material-flat"
                                                , color = "danger"
                                                , block = T
                                   )
                            )
                            , column(width = 6
                                     , actionBttn("dissmissExam"
                                                  , "Dismiss"
                                                  , icon = icon("close")
                                                  , style = "material-flat"
                                                  , color = "danger"
                                                  , block = T)
                            )
                        )
            )
        )
    })
    # Dismiss
    observeEvent(input$dissmissExam,{
        removeModal()
    })
    
    # Saves the new assignment
    observeEvent(input$saveExam, {
        con <- db_connect()
        row <- data_frame(exam_id = c(as.numeric(input$add_exam_id))
                          , first_topic = c(as.character(input$add_exam_first))
                          , last_topic = c(as.character(input$add_exam_last))
                          , date = c(as.character(input$add_exam_date)))
        
        
        query <- sqlAppendTable(con, "shiny.dbo.exam_def", quotes(row), row.names = FALSE)
        
        query_character <- as.character(query)
        noDouble <- gsub('"',"",query)
        noNew <- gsub('\n'," ",noDouble)
        dbSendQuery(con, noNew)
        
        # Background App Refresh
        sql_query <- 'Select * from Shiny.dbo.exam_def'
        df_examdef <- dbGetQuery(con, sql_query)
        reactive$exam_def <- df_examdef
        
        
        first_topic <- as.numeric(input$add_exam_first)
        last_topic <- as.numeric(input$add_exam_last)
        times <- last_topic - first_topic + 1
        # Add in new grades as NA
        ls_student_id <- reactive$homework_grade %>%
            select(student_id) %>%
            distinct() %>%
            pull()
        
        # Add exam to grades as NA
        student_id <- c()
        exam_id <- c()
        grade <- c()
        topic_id <- c()
        for(i in 1:length(ls_student_id)){
            student_id = append(student_id, c(rep(ls_student_id[i], times)))
            exam_id = append(exam_id, c(rep(as.numeric(input$add_exam_id), times)))
            grade = append(grade, c(rep("NA", times)))
            topic_id = append(topic_id, c(seq(first_topic, last_topic)))
        }
        
        new_grade_df <- data_frame(
            student_id = student_id
            , exam_id = exam_id
            , grade = grade
            , topic_id = topic_id
        )
        
        for(i in 1:nrow(new_grade_df)){
            row <- quotes(new_grade_df[i,])
            query <- sqlAppendTable(con, "Shiny.dbo.exam_grade", row, row.names = FALSE)
            query_character <- as.character(query)
            noDouble <- gsub('"',"",query)
            noNew <- gsub('\n'," ",noDouble)
            dbSendQuery(con, noNew)
        }
        
        sql_query <- 'Select * from Shiny.dbo.exam_grade'
        df_homework_grade <- dbGetQuery(con, sql_query)
        reactive$homework_grade <- df_homework_grade
        
        showNotification(paste0("Exam added as id: ", as.character(input$add_exam_id), " with grade NA"))
        # Background App Refresh
        sql_query <- 'Select * from Shiny.dbo.exam_grade'
        df_examgrade <- dbGetQuery(con, sql_query)
        reactive$exam_grade <- df_examgrade
        
        removeModal()
        
    })
    
    # Edit Homework Server ----
    #Data Table -- builds homeworks data table
    output$editHomeworkGrades <- renderDT({
        df <- homework_grades() %>%
            select(Name = firstLast,`Homework Id` = homework_id, Grade = grade)
        datatable(df, rownames = FALSE, selection = list(mode = 'single', target = 'row'), filter = 'top', caption = "Click a Row to Edit")
    })
    
    
    # Edit Review Grades 
    observeEvent(input$editHomeworkGrades_rows_selected,{
        
        rowNumber <- input$editHomeworkGrades_rows_selected
        df <- homework_grades()
        rowData <- df[rowNumber, ]
        showModal(
            modalDialog(title = "Edit Grade", easyClose = T
                        ,box(width = 12, status = "danger"
                             , HTML("<b> Name: </b>")
                             , renderText(rowData$firstLast)
                             , HTML("<b> Homework ID: </b>")
                             , renderText(rowData[1,3])
                             , numericInput("hwGrade", "Grade:",  value = as.numeric(rowData$grade), max = 100)
                        )
                        , footer = fluidRow(
                            column(width = 6
                                   , actionBttn("hwgradeSave"
                                                , "Save"
                                                , icon = icon("save")
                                                , style = "material-flat"
                                                , block = T
                                                , color = "danger"
                                   )
                            )
                            , column(width = 6
                                     , actionBttn("hwgradeDismiss"
                                                  , "Dismiss"
                                                  , icon = icon("close")
                                                  , style = "material-flat"
                                                  , color = "danger"
                                                  , block = T)
                            )
                        )
            )
        )
    })
    
    # Dismiss
    observeEvent(input$hwgradeDismiss,{
        removeModal()
    })
    #When the "Save Grade" button is pressed
    observeEvent(input$hwgradeSave,{
        rowNumber <- input$editHomeworkGrades_rows_selected
        df <- homework_grades()
        rowData <- df[rowNumber, ]
        newGrade <- as.character(input$hwGrade)
        hw_ID <- rowData[1, 3]
        
        df <- student_def %>%
            filter(firstLast == rowData$firstLast)
        
        student_id <- df$student_id
        
        # Write to Database
        sql_query <- paste0("update Shiny.dbo.homework_grade set grade = '", newGrade, "' where (homework_id = ", hw_ID, " and student_id = ", student_id, ")")
        dbExecute(con, sql_query)
        
        # Background App Refresh
        sql_query <- 'Select * from Shiny.dbo.homework_grade'
        df_homeworkGrades <- dbGetQuery(con, sql_query)
        reactive$homework_grade <- df_homeworkGrades
        
        showNotification("Changes Saved to Remote Database.", type = c("message"), duration = 3)
        removeModal()
    })
    
    
    #  Add HW Button press --- implements interface for professor to add homeworks
    observeEvent(input$addHW, {
        df_homeworks <- homework_grades()
        showModal(
            modalDialog(title = "Add a Homework",  easyClose = T
                        , box(width = 12, status = "danger", title = "Homework Information"
                              , fluidRow(
                                  column(width = 6
                                         , numericInput(inputId = "hwAddID", label = "Homework ID", value = 1 + max(df_homeworks$homework_id))
                                         , dateInput(inputId = "addHWStartDate", label = "Date", value = df_homeworks$date[1])
                                  )
                                  , column(width = 12
                                           , textAreaInput(inputId = "hwAddDesc", label = " Homework Description", value = "Description"))
                              )
                        )
                        , footer = fluidRow(
                            column(width = 6
                                   , actionBttn("saveHW"
                                                , "Save"
                                                , icon = icon("save")
                                                , style = "material-flat"
                                                , block = T
                                                , color = "danger"
                                   )
                            )
                            , column(width = 6
                                     , actionBttn("hwAddDismiss"
                                                  , "Dismiss"
                                                  , icon = icon("close")
                                                  , style = "material-flat"
                                                  , color = "danger"
                                                  , block = T)
                                     
                            )
                        )
            )
        )
    })
    # Dismiss
    observeEvent(input$hwAddDismiss,{
        removeModal()
    })
    
    # Inputs the changes to the database
    observeEvent(input$saveHW, {
        con <- db_connect()
        row <- data_frame(homework_id = c(as.numeric(input$hwAddID))
                          , description = c(as.character(input$hwAddDesc))
                          , date = c(as.character(input$addHWStartDate))
        )
        quotes <- quotes(row)[1,]
        query <- sqlAppendTable(con, "shiny.dbo.homework_def", quotes(row)[1,], row.names = FALSE)
        query_character <- as.character(query)
        noDouble <- gsub('"',"",query)
        noNew <- gsub('\n'," ",noDouble)
        dbSendQuery(con, noNew)
        
        #Background App refresh
        sql_query <- 'Select * from Shiny.dbo.homework_def'
        df_homework_def <- dbGetQuery(con, sql_query)
        reactive$homework_def <- df_homework_def
        
        # Add homework grades as 0
        ls_student_id <- reactive$homework_grade %>%
            select(student_id) %>%
            distinct() %>%
            pull()
        
        new_grade_df <- data_frame(
            student_id = ls_student_id
            , homework_id = rep(as.numeric(input$hwAddID), length(ls_student_id))
            , grade = rep(0,length(ls_student_id) )
        )
        
        for(i in 1:nrow(new_grade_df)){
            row <- quotes(new_grade_df[i,])
            query <- sqlAppendTable(con, "Shiny.dbo.homework_grade", row, row.names = FALSE)
            query_character <- as.character(query)
            noDouble <- gsub('"',"",query)
            noNew <- gsub('\n'," ",noDouble)
            dbSendQuery(con, noNew)
        }
        
        sql_query <- 'Select * from Shiny.dbo.homework_grade'
        df_homework_grade <- dbGetQuery(con, sql_query)
        reactive$homework_grade <- df_homework_grade
        
        showNotification(paste0("Homework added as id: ", as.character(input$hwAddID), "with grade of 0."))
        removeModal()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)