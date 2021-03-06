course_calendar_UI <- function(id){
  box(width = 12, status = "primary", title = "Course Calendar"
      , timevisOutput(NS(id,"course_schedule"))
  )
}

# Have to rework to use with reactive data 
course_calendar_server <- function(id, r){
  moduleServer(id, function(input,output,session){
    
    # Course Schedule output ----
    output$course_schedule <- renderTimevis({
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
               , start = as.character(`Review Start Date`)
               , end =  as.character(`Review End Date`)
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
      )
    })
    
    # Course Schedule Selected ----
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
        r$cal_item <- df_item
        view_calendar_review_UI("calendar_review", title)
      }
      else{
        df_item <- r$df_homework %>%
          filter(id == item_id) %>%
          mutate(`Homework Number` = as.integer(id)) %>%
          mutate(`Date Assigned` = date_assigned) %>%
          mutate(`Date Due` = date_due) %>%
          select(`Homework Number`, Description = description, `Date Assigned`, `Date Due`)
        r$cal_item <- df_item
        view_calendar_hw_UI("calendar_hw", title)
      }
      
    }, ignoreInit = T)
    
    
  })
}