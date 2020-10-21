course_calendar_UI <- function(id){
  box(width = 12, status = "primary", title = "Course Calendar"
      , timevisOutput(NS(id,"course_schedule"))
  )
}

# Have to rework to use with reactive data 
course_calendar_server <- function(id, r){
  moduleServer(id, function(input,output,session){
    
    observeEvent(input$course_schedule_selected, {
      # Get item id in r_# or h_# format
      item_id <- input$course_schedule_selected
      
      # split into "r" "#" or "h" "#" format
      item_id <- str_split(item_id, "_") %>%
                     unlist() 
     
      item_type <- item_id[1]
      item_id <- item_id[2]
      
      title <- paste0("Homework ", item_id)
      
      if (item_type == "r"){
        title <- paste0("Review ", item_id)

        df_item <- r$df_review_table %>%
          filter(`Review ID` == item_id)
          
      }
      else{
        df_item <- r$df_homework %>%
          filter(id == item_id) %>%
          mutate(`Homework Number` = as.integer(id)) %>%
          mutate(`Date Assigned` = date_assigned) %>%
          mutate(`Date Due` = date_due) %>%
          select(`Homework Number`, `Date Assigned`, `Date Due`)
      }
      
      #browser()
      r$df_cal_item <- df_item
      view_calendar_item_UI("calendar_item", title)
      
      
    }, ignoreInit = T)
    
    output$course_schedule <- renderTimevis({
      browser()
      
      df_timevis_homework <-  r$df_homework %>%
        mutate(content = paste("Homework", id)
               , start = as.character(date_assigned)
               , end = as.character(date_due)
               , id = paste0("h_", id)
               , group = "homework"
               , className = "homework"
               ) %>%
        select(id, start, end, content, group, className)
      
      df_timevis_review <- r$df_review_table %>%
        mutate(content = `Review Name`
               , start = as.character(`Review Date`)
               , end = NA
               , id = paste0("r_", `Review ID`)
               , group = "review"
               , className = "review"
        ) %>%
        select(id, start, end, content, group, className)
      
      groups <- data.frame(
        id = c("review", "homework")
        , content = c("Reviews", "Homework")
        , className = c("review","homework")
      )
      df_timevis <- rbind(df_timevis_homework, df_timevis_review)
      
      timevis(df_timevis
              , fit = TRUE
              , groups
              , showZoom = FALSE
              , options = list(
                  zoomable = FALSE
                , horizontalScroll = TRUE # not working
                , moveable = TRUE)
              )
    })
  })
}