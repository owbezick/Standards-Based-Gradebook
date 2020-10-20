homework_UI <- function(id) {
  tabPanel(title = "Homework"
           , fluidRow(
             column(width = 12
                    , box(width = 12, status = "primary"
                          , rHandsontableOutput(NS(id,"homework_grades"))
                    )
             )
             , column(width = 12
                      , box(width = 12, status = "primary"
                            , echarts4rOutput(NS(id,"homework_grade_bar"), height = "300px")
                      )
             )
           )
  )
}

homework_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    df_filtered_homework_grades <- reactive({
      req(r$is$auth)
      student_name <- r$df_student %>%
        filter(student_id == r$auth_student_id()) %>%
        select(name) %>%
        pull()
      
      homework_grade <- r$df_homework_table %>%
        filter(`Student Name` == student_name)
      
      cols <- c(2:ncol(homework_grade))
      
      homework_grade[,cols] <- lapply(homework_grade[,cols], as.double)
      
      df <- homework_grade
    })
    
    output$homework_grade_bar <- renderEcharts4r({
      df <- df_filtered_homework_grades() %>%
        select(-c(`Student Name`))
      
      df <- as_tibble(cbind(Homework = names(df), t(df))) %>%
        mutate(grade = as.numeric(V2)/100)
      
      
      
      df %>%
        e_chart(Homework) %>%
        e_bar(grade, barWidth = "50%") %>%
        e_legend(show = F) %>%
        e_y_axis(formatter = e_axis_formatter("percent", digits = 2)) %>%
        e_labels(position = "top", formatter  = htmlwidgets::JS("
                                    function(params){
                                      return(parseFloat(params.value[1]*100).toFixed(2) +'%');
                                    }
                                    ")) %>%
        #e_color(color = rgb(196, 18, 48, alpha = 230, max = 255)) %>%
        e_color(color = "#c41230") %>%
        e_grid(left = "15%", right = "5%", top = "10%", bottom = "10%")
    })
    
    
    output$homework_grades <- renderRHandsontable({
      req(r$is$auth)
      
      df_hw <- df_filtered_homework_grades()
      
      # TODO: Conditional formatting for cells that are actually NA vs character of NA ----
      rhandsontable(df_hw
                    , rowHeaders = NULL
                    , stretchH = 'all') 
    })
  }) #end module server
}

review_UI <- function(id) {
  tabPanel(title = "Reviews"
           , fluidRow(
             column(width = 12
                    , box(width = 12, status = "primary", title = "Review Grades"
                          , rHandsontableOutput(NS(id,"review_grades"))
                    )
                    , box(width = 12, status = "primary", title = "Topic Proficiency"
                          , column(width = 3
                                   , "Topic Attempts"
                                   , rHandsontableOutput(NS(id,"topic_proficiency"))
                          )
                          , column(width = 9
                                   , echarts4rOutput(NS(id,"topic_proficiency_bar"), height = "250px")
                          )
                    )
             )
           )
  )
}

review_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    # Creates student data frames
    df_filtered_review_to_topic <- reactive({
      req(r$is$auth)
      current_student <- r$auth_student_id()
      exam_grade <- r$df_review_to_topic %>%
        filter(student_id == current_student
               , grade != "NA")
    })
    
    df_filtered_homework_grades <- reactive({
      req(r$is$auth)
      student_name <- r$df_student %>%
        filter(student_id == r$auth_student_id()) %>%
        select(name) %>%
        pull()
      
      
      homework_grade <- r$df_homework_table %>%
        filter(`Student Name` == student_name)
    })
    
    
    
    df_remaining_attempts <- reactive({
      # Topics Completed
      df_overall_grades <- df_filtered_review_to_topic() %>% 
        group_by(grade) %>%
        tally()
      
      # Topic attempts so far
      df_attempts <- df_filtered_review_to_topic() %>% 
        group_by(topic_id) %>%
        tally() 
      
      
      # Topic attempts total
      df_total_attempts <- r$df_review_table %>%
        subset(select = -c(`Review Name`, `Review ID`, `Review Date`))
      
      df_total_attempts[is.na(df_total_attempts)] <- 0
      
      df_total_attempts <- df_total_attempts %>%
        colSums()
      
      df_total_attempts <- df_total_attempts %>%
        as.matrix() %>%
        as.data.frame()
      
      df_total_attempts$topic_id <- rownames(df_total_attempts)
      
      justNum <- function(str){
        return(str_remove_all(str, "Topic "))
      }
      
      df_total_attempts <- df_total_attempts %>%
        select(topic_id, total = V1) %>%
        mutate(topic_id = justNum(topic_id))
      
      df_remaining_attempts <- base::merge(x = df_total_attempts, y = df_attempts, by = "topic_id", all.x = TRUE) %>%
        select(topic_id, total, attempts = n)
      
      df_remaining_attempts[is.na(df_remaining_attempts)] <- 0
      
      df_remaining_attempts <- df_remaining_attempts %>%
        mutate(remaining = total - attempts)
      
      base::merge(df_remaining_attempts, r$df_topic, by = "topic_id", all.y = TRUE)
      
      return (df_remaining_attempts)
      
    }) 
    
    
    output$topic_proficiency <- renderRHandsontable({
      req(r$is$auth)
      
      df <- df_remaining_attempts() %>%
        mutate(
          Topic = topic_id, `Previous Attempts` = as.integer(attempts)
          , `Total Attempts` = as.integer(total)
          , `Remaining Attempts` = as.integer(remaining)
        ) %>%
        select(
          Topic
          , Previous = `Previous Attempts`
          , Remaining = `Remaining Attempts`
          , Total =  `Total Attempts`
          )
      rhandsontable(
        df
        , rowHeaders = NULL
      ) %>%
        hot_cols(readOnly = T)
    })
    
    output$topic_proficiency_bar <- renderEcharts4r({
      req(r$is$auth)
      
      df <- df_remaining_attempts()
      
      df <- df %>%
        mutate(Topic = topic_id
               , `Previous Attempts` = as.integer(attempts)
               , `Total Attempts` = as.integer(total)
               , `Remaining Attempts` = as.integer(remaining)) %>%
        select(Topic
               , prev = `Previous Attempts` 
               , total = `Total Attempts`
               , remain = `Remaining Attempts`)
      
      df %>%
        e_charts(Topic) %>%
        e_bar(prev
              , stack = "Topics"
              , color = "#c41230"
              , barWidth = "50%"
              , name = "Previous Attempts") %>%
        e_bar(remain
              , stack = "Topics"
              , color = "#222D32"
              , barWidth = "50%"
              , name = "Remaining Attempts") %>%
        e_legend(bottom = 'bottom') %>%
        e_grid(top = "15%", left= "10%", bottom = "20%", right = "5%")
      
      
    })
    
    
    output$review_grades <- renderRHandsontable({
      req(r$is$auth)
      grade_types <- c("NA", "NC", "Fluent", "Getting There", "Needs Work")
      df_review_to_topic <- df_filtered_review_to_topic()
      df_student <- r$df_student
      df_review_topic <- df_review_to_topic %>%
        left_join(df_student, by = "student_id") %>%
        pivot_wider(id_cols = c(review_id, topic_id)
                    , names_from = topic_id
                    , values_from = grade) %>%
        rename(`Review ID` = review_id)
      # TODO: Conditional formatting for cells that are actually NA vs character of NA ----
      rhandsontable(df_review_topic
                    , rowHeaders = NULL
                    , stretchH = 'all') %>%
        hot_col(col = "Review ID", type = "character") %>%
        hot_cols(type = "dropdown", source = grade_types, readOnly = T)
    })
  })
}