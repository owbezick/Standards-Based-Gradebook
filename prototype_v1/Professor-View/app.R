# Mastery Gradebook application - Professor View
# Author: Owen Bezick & Calvin Spencer
source("utils/libraries.R")
source("utils/utils.R")
source("data/data_intake.R")
source("modules/wizard.R")
source("modules/course_info.R")
source("modules/course_calendar.R")
source("modules/edit_reviews.R")
source("modules/edit_course_info.R")
source("modules/edit_and_add.R")
source("modules/edit_homework.R")
source("modules/edit_topic.R")
source("modules/edit_roster.R")
source("modules/edit_grades.R")
source("modules/view_calendar_item.R")


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
        # Initial Data Set Up ----
        , uiOutput("wizard")
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
                # Grades tab UI ----
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
server <- function(input, output, session) {
    # Data Wizard ----
    output$wizard <- renderUI({
         showModal(modalDialog(title = "Initial Data Input"
                      , size = "l"
                      , footer = "Note: more data can be added and edited later in the app."
                      , easyClose = F
                      , fluidRow(
                          wizardUI("dataWizard"
                                   ,  actionBttn(
                                       inputId = "closeWizard"
                                       , label = "Close Wizard"
                                       , style = "material-flat"
                                       , block = T
                                   ) 
                          ) 
                      )
        )
         )
    })
    
    wizard_server("dataWizard", r)
    
    observeEvent(input$closeWizard, {
        if(nrow(r$df_review_grades) > 0){
            removeModal()
        }else{
            showNotification("Please add a review!", type = "error")
        }
    })
    
    
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