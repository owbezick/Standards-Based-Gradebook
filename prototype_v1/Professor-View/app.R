# Mastery Gradebook application - Professor View
# Author: Owen Bezick & Calvin Spencer
source("utils/libraries.R")
source("data/data_intake.R")
source("modules/wizard.R")
source("modules/course_info.R")
source("modules/add_homework_button.R")
source("modules/add_topic_button.R")
source("modules/edit_and_add.R")
source("modules/course_calendar.R")
source("modules/course_information_button.R")
source("modules/edit_roster.R")
source("modules/topics.R")
source("modules/view_calendar_item.R")
source("modules/grade.R")


ui <- dashboardPage(
    skin = "black"
    , dashboardHeader(
        title = "Professor View" 
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
        , uiOutput("dataWizard")
        , tabItems(
            tabItem(
                # Home tab UI ----
                tabName = "home"
                , fluidRow(
                    course_info_UI("courseinfo")
                    , edit_and_add_UI("edit_and_add")
                )
                , fluidRow(
                    course_calendar_UI("course_calendar")
                )
                , fluidRow(
                    topics_UI("topics")
                )
            )
            , tabItem(
                # Grade tab UI ----
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
    # Data Wizard ----
    output$dataWizard <- renderUI({
        showModal(
            modalDialog(title = "Initial Data Input"
                        , size = "l"
                        , footer = "Note: more data can be added and edited later in the app."
                        , easyClose = F
                        , br()
                        , fluidRow(
                            wizardUI("dataWizard", r) 
                        )
            )
        )
    })
    observeEvent(input$closeWizard, {
        removeModal()
    })
    
    wizard_server("dataWizard", r)
    
    # Other server calls ----
    course_calendar_server("course_calendar", r)
    course_info_server("courseinfo", r)
    edit_and_add_server("edit_and_add", r)
    topics_server("topics", r)
    view_calendar_hw_Server("calendar_hw", r)
    view_calendar_review_Server("calendar_review", r)
    homework_server("homework", r)
    review_server("review", r)
}

# Run the application 
shinyApp(ui = ui, server = server)