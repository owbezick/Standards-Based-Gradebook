# Edit and add module ----
edit_and_add_UI <- function(id){
  box(width = 4, title = "Edit & Add", status = "primary"
      , column(width = 12
               , actionBttn(inputId = NS(id, "editCourseInfo"), label = "Course Information", style = "material-flat", block = T)
               , br()
      )
      , column(width = 6
               , actionBttn(inputId = NS(id, "addHomework"), label = "Homework", style = "material-flat", block = T)
      )
      , column(width = 6
               , actionBttn(inputId = NS(id, "addReview"), label = "Review", style = "material-flat", block = T)
      )
  )
}

edit_and_add_server <- function(id){
  moduleServer(id, function(input, output, session) {
    #TODO: wrap into separate modules for each button 
    observeEvent(NS(id, input$editCourseInfo), {
      course_information_button_UI("course_info_button", df_course_info)
    }, ignoreInit = T) # Maybe set to F for first time only?
    
    
    observeEvent(NS(id, input$addHomework), {
      showModal(
        modalDialog(title = "Add Homework", size = "l")
      )
    }, ignoreInit = T)
    
    observeEvent(NS(id, input$addReview), {
      showModal(
        modalDialog(title = "Add Review", size = "l")
      )
    }, ignoreInit = T)
    
  })
}

# Course information button module ----
course_information_button_UI <- function(id, df) {
  showModal(
    modalDialog(title = "Edit Course Information", size = "l"
                , fluidRow(
                  column(width = 6
                         , tags$b("Course Location: ")
                         , textInput(inputId = "courseLocation", label = NULL, placeholder = df$location)
                         , br()
                         , tags$b("Meeting times: ")
                         , textInput(inputId = "meetingTimes", label = NULL, placeholder = df$meeting_times)
                         , br()
                         , tags$b("Office hours: ")
                         , textInput(inputId = "officeHours", label = NULL, placeholder = df$office_hours)
                  )
                  , column(width = 6
                           , column(width = 6
                                    , tags$b("Link URL: ")
                                    , textInput(inputId = "linkURL_1", label = NULL, placeholder = df$link)
                                    , br()
                                    , tags$b("Link URL: ")
                                    , textInput(inputId = "linkURL_2", label = NULL, placeholder = df$link)
                                    , br()
                                    , tags$b("Link URL: ")
                                    , textInput(inputId = "linkURL_3", label = NULL, placeholder = df$link)
                           )
                           , column(width = 6
                                    , tags$b("Link Description: ")
                                    , textInput(inputId = "linkDescription_1", label = NULL, placeholder = df$link_description)
                                    , br()
                                    , tags$b("Link Description: ")
                                    , textInput(inputId = "linkDescription_2", label = NULL, placeholder = df$link_description)
                                    , br()
                                    , tags$b("Link Description: ")
                                    , textInput(inputId = "linkDescription_3s", label = NULL, placeholder = df$link_description)
                           )
                  )
                )
    )
  )
}

course_information_button_Server <- function(id){
  moduleServer(id, function(input,output,session){
  
  })
}