# Mastery Gradebook application - Student View
# Author: Owen Bezick


# Source Libraries
source("libraries.R", local = TRUE)
source("data_intake.R", local = TRUE)
source("utils.R", local = TRUE)

# Define UI
ui <- dashboardPage(skin = "black"
  , dashboardHeader(title = "Student View"
                    , tags$li(class = "dropdown", tags$img(height = "40px", src='davidson_logo_white.png', hspace = "4", vspace ="4"))
  )
  , dashboardSidebar(
    sidebarMenu(
      menuItem(tabName = "home", text = "Home", icon = icon("home")) 
      , menuItem(tabName = "viewGrades", text = "View Grades", icon = icon("check-double"))
    )
  )
  , dashboardBody( 
    # Custom CSS Formating ----
    tags$head(
      tags$style(
        HTML('
                          .skin-black .main-header .logo {
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
                            ')
      )
    )
    , tabItems(
      tabItem(
        tabName = "home"
        , HTML("<center><h1> Mastery Gradebook Dashboard </h1></center>")
        , column(width = 12
                 , fluidRow(uiOutput("student_profile"), uiOutput("prof_profile") 
                 )
        )
        , uiOutput("schedule")
        , uiOutput("authModal")
      )
      , tabItem(
        tabName = "viewGrades"
        , tags$style(".small-box.bg-red { background-color: #da766e !important; color: #000000!important; }")
        , tags$style(".small-box.bg-navy { background-color: #ac1a2f !important; color: #ffffff !important; }")
        , tags$style(".small-box.bg-yellow { background-color: #84a4aa !important; color: #000000 !important; }")
        , tags$style(".small-box.bg-green { background-color:	#e4a78f !important; color: #000000 !important; }")
        , fluidRow(
          tabBox(width = 12
                   , tabPanel(title = "Exam Grades"
                              , box(width = 12, status = "danger"
                                    , fluidRow(
                                      column(width = 3
                                             , valueBoxOutput("topics", width = 12)
                                      )
                                      ,column(width = 3
                                              , valueBoxOutput("mastery", width = 12)
                                      )
                                      , column(width = 3
                                               , valueBoxOutput("journey", width = 12)
                                      )
                                      , column(width = 3
                                               , valueBoxOutput("apprentice", width = 12)
                                      )
                                    )
                              )
                              , fluidRow(
                                column(width = 12
                                       , box(width = 6, status = "danger", title = "All Exam Grades", height = "550"
                                             , DTOutput("exam_grades_dt")
                                       )
                                       , box(width = 6, status = "danger", title = "Top Grades",  height = "550"
                                             , echarts4rOutput("gradeBar")
                                       )
                                )
                              )
                   )
                   , tabPanel(title = "Homework Grades"
                              , fluidRow(
                                column(width = 6
                                       , box(width = 12, status = "danger", height = "100%"
                                             , valueBoxOutput("homework_average", width = 12)
                                       )
                                       , box(width = 12, status = "danger", title = "All Homework Grades"
                                             , DTOutput("homework_grades_dt")
                                       )
                                )
                                , column(width = 6
                                         , box(width = 12, status = "danger", title = "Plotted Homework Grades", height = "100%"
                                               , echarts4rOutput("homeworkScatter")
                                         )
                                )
                              )
                   )
          )
        )
      )
    )
  )
)



# Define server logic 
server <- function(input, output) {
  # Authentication ---- 
  output$authModal <- renderUI({
    showModal(
      modalDialog(title = "Authentication", easyClose = F, footer = actionButton(inputId = "auth_save", label = "Continue")
                  , numericInput(inputId = "student_id"
                                 , label = "Enter your Student ID:"
                                 , value = 0)
      )
    )
  })
  
  # Pulls input information
  is <- reactiveValues(auth = F)
  auth_student_id <- reactive(input$student_id)
  
  # List of valid student idss
  ls_student_id <- student_def %>%
    distinct(student_id) %>% pull()
  
  # Verifies user input
  observeEvent(input$auth_save, {
    if (input$student_id %in% ls_student_id){
      name <- student_def %>%
        filter(student_id == input$student_id) %>% 
        select(first) %>%
        pull()
      showNotification(paste0("Welcome, ",name, "!"), type = "message")
      is$auth <- T
      removeModal()
    } else {
      showNotification(paste(as.character(input$student_id), "not found. Please try again."), type = "error")
    }
  })
  
  # Creates student data frames
  exam_grades <- reactive({
    req(is$auth)
    current_student <- auth_student_id()
    exam_grade <- exam_grade %>%
      filter(student_id == current_student)
  })
  
  homework_grades <- reactive({
    req(is$auth)
    current_student  <- auth_student_id()
    homework_grade <- homework_grade %>%
      filter(student_id == current_student)
  })
  
  # Student Profile ----
  output$student_profile <- renderUI({
    req(is$auth) # requires authentication for viewing
    df <- student_def %>%
      filter(student_id == auth_student_id())
    box(width= 6, title = "Student Information", status = "danger"
        , column(width = 6
                 , fluidRow(
                   img(src= paste0(as.character(df$student_id), ".jpg"))
                 )
        )
        , column(width = 6
                 , fluidRow(
                   HTML(paste0("<b>", paste(df$first, df$last), "</b>"))
                 )
                 , br()
                 , fluidRow(
                   HTML("<b> Student ID: </b>")
                   , df$student_id
                 )
                 , br()
                 , fluidRow(
                   HTML("<b> Email: </b>")
                   , tolower(paste0(substr(df$first, 0, 2), df$last, "@davidson.edu"))
                 )
                 
        )
    )
  })
  
  # Professor Profile ----
  output$prof_profile <- renderUI({
    req(is$auth) # requires authentication for viewing
    df <- student_def %>%
      filter(student_id == auth_student_id())
    
    box(width= 6, title = "Professor Information", status = "danger"
        , column(width = 6
                 , fluidRow(
                   img(src= "mascot.jpg")
                 )
        )
        , column(width = 6
                 , fluidRow(
                   HTML("<b> Dr. Professorson </b>"),
                 )
                 , br()
                 , fluidRow(
                   HTML("<b> Email: </b>"),
                   HTML("drprofessorson@davidson.edu")
                 )
                 , br()
                 , fluidRow(
                   HTML("<b> Office Hours: </b>")
                   , br()
                   , HTML("MWF: 9:30- 11")
                   , br()
                   , HTML("TTh: 1:40-3:00")
                 )
        )
    )
  })
  
  # Schedule ----
  output$gantt <- renderTimevis({
    exams <- exam_def %>%
      mutate(content = paste("Exam", exam_id)) %>%
      mutate(id = paste0("E", exam_id)) %>%
      select(content = content, start = date, id = id)
    homeworks <- homework_def %>%
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
  # Exam Grades ----
  output$exam_grades_dt <- renderDT({
    df <- exam_grades() %>%
      select(`Exam ID` = exam_id, `Topic ID` = topic_id, Grade = grade)
    datatable(df, rownames = F)
  })
  
  output$gradeBar <- renderEcharts4r({
    df <- exam_grades() %>%
      group_by(topic_id) %>%
      summarise(grade = grade_max(grade)) %>%
      select(grade) %>%
      filter(grade != "NA") %>%
      count(grade)
    
    apprentice <- df %>%
      filter(grade == "A") %>%
      pull()
    journey <-df %>%
      filter(grade == "J") %>%
      pull()
    master <- df %>%
      filter(grade == "M") %>%
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
  
  output$mastery <- renderValueBox({
    value <- exam_grades() %>%
      group_by(topic_id) %>%
      summarise(grade = grade_max(grade)) %>%
      filter(grade == "M") %>%
      count() %>%
      pull()
    
    valueBox(value = value, subtitle = "Mastered", color = "green")
  })
  
  output$journey<- renderValueBox({
    value <- exam_grades() %>%
      group_by(topic_id) %>%
      summarise(grade = grade_max(grade)) %>%
      filter(grade == "J") %>%
      count() %>%
      pull()
    valueBox(value = value, subtitle = "Journeyman", color = "yellow")
  })
  
  output$apprentice <- renderValueBox({
    value <- exam_grades() %>%
      group_by(topic_id) %>%
      summarise(grade = grade_max(grade)) %>%
      filter(grade == "A") %>%
      count() %>%
      pull()
    valueBox(value = value, subtitle = "Apprentice", color = "red")
  })
  
  output$topics <- renderValueBox({
    value <- exam_grades() %>%
      select(topic_id) %>%
      distinct() %>%
      nrow()
    
    valueBox(value = value, subtitle = "Topics Covered", color = "navy")
  })
  
  # Homework Grades ----
  output$homework_grades_dt <- renderDT({
    df <- homework_grades()
    df <- df %>%
      select(`Homework ID` = homework_id, Grade= grade)
    datatable(df, rownames = FALSE)
  })
  
  output$homeworkScatter <- renderEcharts4r({
    df <- homework_grades()
    df <- df %>%
      mutate(homework_id = as.character(homework_id))
    df %>%
      e_chart(homework_id) %>%
      e_scatter(grade, symbol_size = 15) %>%
      e_tooltip() %>%
      e_theme('dark') %>%
      e_legend(show=F)
  })
  
  output$homework_average <- renderValueBox({
    value <- homework_grades() %>%
      summarise(avg = mean(grade)) %>% pull()
    valueBox(paste0(as.character(value), "%"), "Homework Average", color = "navy")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)