homework_UI <- function(id) {
  tabPanel(title = "Homework"
           , fluidRow(
             column(width = 12
                    , box(width = 12
                          , status = "primary"
                          , title = "Homework Grades"
                          , rHandsontableOutput(NS(id,"homework_grades"))
                    )
             )
             , column(width = 12
                      , box(width = 12, status = "primary", title = NULL
                            , echarts4rOutput(NS(id,"homework_grade_bar"), height = "300px")
                      )
             )
           )
  )
}

# Homework server ----
homework_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    # Reactive Data ----
    df_filtered_homework_grades <- reactive({
      req(r$is$auth)
      student_name <- r$df_student %>%
        filter(student_id == r$auth_student_id()) %>%
        select(name) %>%
        pull()
      
      homework_grade <- r$df_homework_grades %>%
        filter(`Student Name` == student_name)
      
      cols <- c(2:ncol(homework_grade))
      
      homework_grade[,cols] <- lapply(homework_grade[,cols], as.double)
      
      df <- homework_grade
    })
    # Average ----
    output$title <- renderText({
      avg <- df_filtered_homework_grades() %>%
        select(c(2:ncol(df_filtered_homework_grades()))) %>%
        rowMeans()
      paste0("Average Homework: ", avg, "%")
      
    })
    # Bar Chart ----
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
        e_mark_line(title = "Average Score"
                    , data = list(type = "average", name = "Average Score")
                    , animation = FALSE
                    , lineStyle = c(color = "#444")
                    , symbol = "circle"
        ) %>%
        e_color(color = "#c41230") %>%
        e_grid(left = "15%", right = "5%", top = "10%", bottom = "10%") %>%
        e_tooltip()
    })
    
    # Table ----
    output$homework_grades <- renderRHandsontable({
      req(r$is$auth)
      
      df_hw <- df_filtered_homework_grades()
      rhandsontable(df_hw
                    , rowHeaders = NULL
                    , stretchH = 'all') 
    })
  }) #end module server
}

# Review UI ----
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

# Review Server
review_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    # Review grade data -----
    df_review_grades <- reactive({
      req(r$is$auth)
      current_student <- r$auth_student_id()
      exam_grade <- r$df_review_grades %>%
        filter(student_id == current_student
               , grade != "NA")
    })
    
    
    # Calculate attempt chart ----
    df_attempts <- reactive({
      # Total attempts for each topic
      attempts_topic_total <- r$df_review_table %>%
        pivot_longer(cols = c(4:ncol(r$df_review_table))) %>%
        group_by(name) %>%
        tally(value) %>%
        rename(topic = name, total_attempts = n)
      
      topics <- str_split_fixed(attempts_topic_total$topic, " ", 2)[,2]
      attempts_topic_total <- attempts_topic_total %>%
        mutate(topic = topics) %>%
        rename(topic_id = topic)
      
      # Attempts taken for each topic 
      df_attempts <- df_review_grades() %>% 
        group_by(topic_id) %>%
        mutate(grade = TRUE) %>%
        tally() 
      
      attempts_df <- left_join(attempts_topic_total, df_attempts) %>%
        rename(attempts = n, total = total_attempts) %>%
        mutate(remaining = total - attempts)
      
    }) 
    
    # Topic table ----
    output$topic_proficiency <- renderRHandsontable({
      req(r$is$auth)
      df <- df_attempts() %>%
        mutate(
          Topic = as.integer(topic_id), `Previous Attempts` = as.integer(attempts)
          , `Total Attempts` = as.integer(total)
          , `Remaining Attempts` = as.integer(remaining)
        ) %>%
        select(
          Topic
          , Total =  `Total Attempts`
          , Previous = `Previous Attempts`
          , Remaining = `Remaining Attempts`
         
        )
      rhandsontable(
        df
        , rowHeaders = NULL
      ) %>%
        hot_cols(readOnly = T)
    })
    
    # Topic bar -----
    output$topic_proficiency_bar <- renderEcharts4r({
      req(r$is$auth)
      
      df <- df_attempts()
      browser()
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
        e_y_axis(formatter = e_axis_formatter("decimal", digits = 0))
        e_grid(top = "15%", left= "10%", bottom = "20%", right = "5%") %>%
        e_tooltip()
      
      
    })
    
    # grades table ----
    output$review_grades <- renderRHandsontable({
      req(r$is$auth)
      grade_types <- c("NA", "NC", "Fluent", "Getting There", "Needs Work")
      df_review_to_topic <- df_review_grades()
      df_student <- r$df_student
      df_review_topic <- df_review_to_topic %>%
        left_join(df_student, by = "student_id") %>%
        pivot_wider(id_cols = c(review_id, topic_id)
                    , names_from = topic_id
                    , values_from = grade
                    , names_prefix = "Topic ") %>%
        rename(`Review ID` = review_id)
      
      rhandsontable(df_review_topic
                    , rowHeaders = NULL
                    , stretchH = 'all') %>%
        hot_col(col = "Review ID", type = "character") %>%
        hot_cols(type = "dropdown", source = grade_types, readOnly = T) %>%
        hot_heatmap(cols = c(2:ncol(df_review_topic)))
    })
  })
}