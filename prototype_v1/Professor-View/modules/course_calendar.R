course_calendar_UI <- function(id){
  box(width = 12, status = "primary", title = "Course Calendar"
      , timevisOutput(NS(id,"course_schedule"))
  )
}

# Have to rework to use with reactive data 
course_calendar_server <- function(id, df_homework_data, df_review_data){
  moduleServer(id, function(input,output,session){
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
  })
}