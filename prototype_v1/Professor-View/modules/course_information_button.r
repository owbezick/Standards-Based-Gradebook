
# Course information button module ----
course_information_button_UI <- function(id, df) {
  showModal(
    modalDialog(title = "Edit Course Information", size = "l"
                , fluidRow(
                  column(width = 6
                         , tags$b("Course Location: ")
                         , textInput(inputId = "courseLocation"
                                     , label = NULL, placeholder = df$location)
                         , br()
                         , tags$b("Meeting times: ")
                         , textInput(inputId = "meetingTimes"
                                     , label = NULL, placeholder = df$meeting_times)
                         , br()
                         , tags$b("Office hours: ")
                         , textInput(inputId = "officeHours"
                                     , label = NULL, placeholder = df$office_hours)
                  )
                  , column(width = 6
                           , column(width = 6
                                    , tags$b("Link URL: ")
                                    , textInput(inputId = "linkURL_1"
                                                , label = NULL, placeholder = df$link)
                                    , br()
                                    , tags$b("Link URL: ")
                                    , textInput(inputId = "linkURL_2"
                                                , label = NULL, placeholder = df$link)
                                    , br()
                                    , tags$b("Link URL: ")
                                    , textInput(inputId = "linkURL_3"
                                                , label = NULL, placeholder = df$link)
                           )
                           , column(width = 6
                                    , tags$b("Link Description: ")
                                    , textInput(inputId = "linkDescription_1"
                                                , label = NULL, placeholder = df$link_description)
                                    , br()
                                    , tags$b("Link Description: ")
                                    , textInput(inputId = "linkDescription_2"
                                                , label = NULL, placeholder = df$link_description)
                                    , br()
                                    , tags$b("Link Description: ")
                                    , textInput(inputId = "linkDescription_3s"
                                                , label = NULL, placeholder = df$link_description)
                           )
                  )
                )
    )
  )
}

course_information_button_Server <- function(id){
  moduleServer(id, function(input,output,session){
    #TODO: add to data base once created ----
  })
}