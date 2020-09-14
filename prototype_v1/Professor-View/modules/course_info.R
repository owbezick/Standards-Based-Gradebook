course_info_UI <- function(id){
   box(width = 8, title = "Course Information", status = "primary"
        , column(width = 6
                 , tags$b("Course Location: ")
                 , textOutput(NS(id, "location"))
                 , br()
                 , tags$b("Meeting times: ")
                 , textOutput(NS(id, "meeting_times"))
        )
        , column(width = 6
                 , tags$b("Helpful links: ")
                 , uiOutput(NS(id, "link"))
                 , br()
                 , tags$b("Office hours: ")
                 , textOutput(NS(id, "office_hours"))
        )
  )
}

course_info_server <- function(id, r){
  moduleServer(id, function(input,output,session){
    output$location <- renderText(
      r$df_course_info$location
    )
    output$meeting_times <- renderText(
      r$df_course_info$meeting_times
    )
    output$office_hours <- renderText(
      r$df_course_info$office_hours
    )
    output$link <- renderUI(
      div(tags$a(
          r$df_course_info$link1_text
          , href =  r$df_course_info$link1_url
        )
        , br()
        , tags$a(
          r$df_course_info$link2_text
          , href =  r$df_course_info$link2_url
        )
        , br()
        , tags$a(
          r$df_course_info$link3_text
          , href =  r$df_course_info$link3_url
        )
      )
    )
  })
}