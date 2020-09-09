# Mastery Gradebook application - Professor View
# Author: Owen Bezick


source("courseInfo.R", local = TRUE)
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
                            menuItem(tabName = "home", text = "Home", icon = icon("home"))
                            , menuItem(tabName ="grades", text = "Grades", icon = icon("chalkboard"))
                        )
                    )
                    , dashboardBody(
                        tabItems(
                            # Home Tab ----
                            tabItem(
                                tabName = "home"
                                , fluidRow(
                                    courseinfoUI("courseinfo")
                                    , box(width = 3, title = "Edit or Add!"
                                          , actionBttn(inputId = "courseInfo", label = "Course Information", style = "material-flat")
                                          , actionBttn(inputId = "addHomework", label = "Add Homework", style = "material-flat")
                                          , actionBttn(inputId = "addReview", label = "Add Review", style = "material-flat")
                                    )
                                )
                                , fluidRow(
                                    box(width = 12, title = "Course Calendar"
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
    
    courseinfoServer("courseinfo", df_course_info)
}

# Run the application 
shinyApp(ui = ui, server = server)