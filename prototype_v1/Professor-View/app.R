# Mastery Gradebook application - Professor View
# Author: Owen Bezick & Calvin Spencer

# Why do we need to still source these? I thought shiny 1.15 takes care of that 
# automatically

# Can we just move over to creating a package?
source("modules/course_info.R")
source("modules/edit_and_add.R")
source("data/data_intake.R")
source("utils/libraries.R")
# Add the 80 character limit by going to code -> display -> show margin
# We can start with 80 but depending on how we want to structure apps
# we can potentially move to 120 now that you have a monitor as well.

# Essentially how "factored out" do we want the code to be?

# We should also look into using the demo package (i forget the name for it)

ui <- dashboardPage(
    skin = "black"
    # do we wanna pull this out? 
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
    # do we wanna pull this out? 
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
            # do we wanna pull this out? 
            tabItem(
                tabName = "home"
                , fluidRow(
                    course_info_UI("courseinfo")
                    , edit_and_add_UI("edit_and_add")
                )
                , fluidRow(
                    box(width = 12, status = "primary", title = "Course Calendar"
                        , timevisOutput("course_schedule")
                    )
                )
            )
            # do we wanna pull this out? 
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
    #TODO: make module for calendar section
    output$course_schedule <- renderTimevis({
        homework_id <- unique(df_homework_data$homework_id)
        homework_date <- unique(df_homework_data$homework_date)
        review_id <- unique(df_review_data$review_id)
        review_date <- unique(df_review_data$review_date)
        df_timevis <- tibble(
            content = c("Homework 1", "Review 1")
            , start = c(homework_date, review_date)
        )
        timevis(df_timevis)
    })

    course_info_server("courseinfo", df_course_info)
    edit_and_add_server("edit_and_add")
}

# Run the application 
shinyApp(ui = ui, server = server)