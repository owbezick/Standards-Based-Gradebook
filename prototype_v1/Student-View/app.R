# Mastery Gradebook application - Student View
# Author: Owen Bezick & Calvin Spencer

# Source Libraries
source("utils/libraries.R", local = TRUE)
source("data/data_intake.R", local = TRUE)
source("utils/utils.R", local = TRUE)

source("modules/course_info.R")
source("modules/course_calendar.R")
source("modules/view_calendar_item.R")
source("modules/grade.R")
source("modules/topics.R")

# Define UI
ui <- dashboardPage(
  skin = "black"
  , dashboardHeader(
    title = "Student View" 
    , tags$li(class = "dropdown"
              , tags$img(
                height = "40px"
                , src= 'davidson_logo_white.png'
                , hspace = "4"
                , vspace = "4"
              )
    )
  )
  , dashboardSidebar( 
    sidebarMenu(
      menuItem(
        tabName = "home"
        , text = "Home"
        , icon = icon("home")
      )
      , menuItem(
        tabName ="grades"
        , text = "Grades"
        , icon = icon("chalkboard")
      )
    )
  )
  , dashboardBody(
    includeCSS("utils/style.css")
    , uiOutput("authModal")
    , tabItems(
      tabItem(
        tabName = "home"
        , fluidRow(
          course_info_UI("courseinfo")
        )
        , fluidRow(
          course_calendar_UI("course_calendar")
        )
        , fluidRow(
          topics_UI("topics")
        )
      )
      , tabItem(
        tabName = "grades"
        , tabBox(title = "Grades", width = 12
                 , homework_UI("homework")
                 , review_UI("review")
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
                                 , value = 801000000)
      )
    )
  })
  
  # Pulls input information
  r$is <- reactiveValues(auth = F)
  r$auth_student_id <- reactive(input$student_id)
  
  # List of valid student idss
  ls_student_id <- reactive({
    r$df_student %>%
    distinct(student_id) %>% 
    mutate(student_id = format(student_id, scientific = F)) %>%
    pull()
  })
  
  # Verifies user input
  observeEvent(input$auth_save, {
    if (r$auth_student_id() %in% ls_student_id()){
      name <- r$df_student %>%
        filter(student_id == r$auth_student_id()) %>% 
        select(name) %>%
        pull()
      showNotification(paste0("Welcome, ", name, "!"), type = "message")
      r$is$auth <- T
      removeModal()
    } else {
      showNotification(paste(as.character(input$student_id), "not found. Please try again."), type = "error")
    }
  })
  
  
  course_calendar_server("course_calendar", r)
  view_calendar_item_server("calendar_item", r)
  topics_server("topics", r)
  course_info_server("courseinfo", r)
  homework_server("homework", r)
  review_server("review", r)
}

# Run the application 
shinyApp(ui = ui, server = server)