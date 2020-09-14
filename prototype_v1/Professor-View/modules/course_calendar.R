course_calendar_UI <- function(id){
  box(width = 12, status = "primary", title = "Course Calendar"
      , timevisOutput(NS(id,"course_schedule"))
  )
}

# Have to rework to use with reactive data 
course_calendar_server <- function(id, r){
  moduleServer(id, function(input,output,session){
    browser()
    output$course_schedule <- renderTimevis({
      df_timevis <- tibble(
        content = c("Homework 1", "Review 1")
        , start = c(homework_date, review_date)
      )
      timevis(df_timevis)
    })
  })
}