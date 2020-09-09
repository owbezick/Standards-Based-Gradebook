courseinfoServer <- function(id, df){
  moduleServer(id, function(input,output,session){
    
    output$location <- renderText(
      df$location
    )
    output$meeting_times <- renderText(
      df$meeting_times
    )
    output$office_hours <- renderText(
      df$office_hours
    )
    output$link <- renderUI(
        tags$a(df$link_description
        , href = df$link
      )
    )
    
  })
}

courseinfoUI <- function(id){
  box(width = 9, title = "Course information"
      , tags$b("Course Location: ")
      , textOutput(NS(id, "location"))
      , br()
      , tags$b("Meeting times: ")
      , textOutput(NS(id, "meeting_times"))
      , br()
      , tags$b("Office hours: ")
      , textOutput(NS(id, "office_hours"))
      , br()
      , tags$b("Helpful links: ")
      , uiOutput(NS(id, "link"))
  )
}