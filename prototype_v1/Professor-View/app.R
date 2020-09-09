# Mastery Gradebook application - Professor View
# Author: Owen Bezick

source("courseinfo.R")
source("data_intake.R")
source("edit_and_add.R")

# Define UI
ui <- dashboardPage(skin = "black"
                    , dashboardHeader(title = "Professor View" 
                                      , tags$li(class = "dropdown"
                                                , tags$img(height = "40px"
                                                           , src= 'davidson_logo_white.png'
                                                           , hspace = "4"
                                                           , vspace = "4")
                                      )
                    )
                    # Sidebar ----
                    , dashboardSidebar( 
                        sidebarMenu(
                            menuItem(tabName = "home", text = "Home", icon = icon("home")
                                     )
                            , menuItem(tabName ="grades", text = "Grades", icon = icon("chalkboard")
                                       )
                        )
                    )
                    , dashboardBody(
                        includeCSS("style.css")
                        , tabItems(
                            # Home Tab ----
                            tabItem(
                                tabName = "home"
                                , fluidRow(
                                    courseinfoUI("courseinfo")
                                    , edit_and_add_UI("edit&add")
                                )
                                , fluidRow(
                                    box(width = 12, title = "Course Calendar", status = "primary"
                                        , timevisOutput("course_schedule")
                                    )
                                    
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
    # Course info page ----
    #TODO: action button modules
    
    # Schedule ----
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
    
    # course info ----
    courseinfoServer("courseinfo", df_course_info)
}

# Run the application 
shinyApp(ui = ui, server = server)