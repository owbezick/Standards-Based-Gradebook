course_calendar_UI <- function(id){
  box(width = 12, status = "primary", title = "Course Calendar"
      , timevisOutput(NS(id,"course_schedule"))
  )
}

# Have to rework to use with reactive data 
course_calendar_server <- function(id, r){
  moduleServer(id, function(input,output,session){
    output$course_schedule <- renderTimevis({
      df_homework <- r$df_homework
      # Start and content necessary to form the tibble
      df_timevis_homework <- r$df_homework %>%
        mutate(content = paste("Homework", id)
               , start = as.character(date_assigned)
               , id = content
               ) %>%
        select(id, start, content)
      
      timevis(df_timevis_homework)
    })
  })
}