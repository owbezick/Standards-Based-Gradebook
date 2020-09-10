# Mastery Gradebook application - Professor View
# Author: Owen Bezick & Calvin Spencer

# Why do we need to still source these? I thought shiny 1.15 takes care of that 
# automatically...

# Can we just move over to creating a package?
source("modules/course_info.R")
source("modules/add_homework_button.R")
source("modules/add_review_button.R")
source("modules/add_topic_button.R")
source("modules/edit_and_add.R")
source("modules/course_calendar.R")
source("modules/course_information_button.R")
source("modules/topics.R")
source("data/data_intake.R")
source("utils/libraries.R")
# Add the 120 character limit by going to Rstudio -> preferences -> code -> display -> show margin

# Essentially how "factored out" do we want the code to be?
# Do we wanna pull out dashboard headers, sidebar, and tab items?

# We should also look into using the demo package (i forget the name for it) to explain how to use it

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
        , tabItems(
            tabItem(
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
                tabName = "grades"
                , box(width = 12
                      , tabsetPanel(
                          tabPanel(title = "Homeworks"
                                   , box(width = 12, title = "View by student or homework button/select"
                                         , "Data table of information"
                                         , "Graph of information"
                                         , "ways to edit")
                          )
                          , tabPanel(title = "Reviews"
                                     , box(width = 12, title = "View by student or review or select topics button/select"
                                           , "Data table of information"
                                           , "Graph of information"
                                           , "ways to edit")
                          )
                      )
                )
            )
        )
    )
)


# Define server logic 
server <- function(input, output) {
    course_calendar_server("course_calendar", df_homework_data, df_review_data)
    course_info_server("courseinfo", df_course_info)
    edit_and_add_server("edit_and_add")
}

# Run the application 
shinyApp(ui = ui, server = server)