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
             , column(width = 12
                      , box(width = 12, status = "primary", title = NULL
                          , rHandsontableOutput(NS(id, "homework_descriptions"))
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
        e_mark_line(title = "  Your Average HW Score"
                    , data = list(type = "average", name = "Average Score")
                    , animation = FALSE
                    , lineStyle = c(color = "#444")
                    , symbol = "circle"
                    , title_position = 'insideStartTop'
        ) %>%
        e_color(color = "#c41230") %>%
        e_grid(left = "10%", right = "5%", top = "10%", bottom = "10%") %>%
        e_tooltip()
      
    })
    
    # HW Grade Table ----
    output$homework_grades <- renderRHandsontable({
      req(r$is$auth)
      
      df_hw <- df_filtered_homework_grades()
      rhandsontable(df_hw
                    , rowHeaders = NULL
                    , stretchH = 'all'
                    , readOnly = T) 
    })
    
    # HW Description Table ----
    output$homework_descriptions <- renderRHandsontable({
      req(r$is$auth)
      
      df_hw <- r$df_homework %>%
        mutate(id = as.character(id)
               , date_due = ymd(date_due)
               , date_assigned = ymd(date_assigned)) %>%
        select(ID = id
               , `Date Assigned` = date_assigned
               , `Due Date` = date_due
               ,  Description = description)
      
      rhandsontable(df_hw
                    , rowHeaders = NULL
                    , stretchH = 'last'
                    , readOnly = T) %>%
        hot_col("ID", type = "numeric") %>%
        hot_col("Date Assigned", type = "date") %>%
        hot_col("Due Date", type = "date")
      
    })
    
  }) #end module server
}

# Review UI ----
review_UI <- function(id) {
  tabPanel(title = "Reviews"
           , fluidRow(
             column(width = 12
                    , fluidRow(
                      div(class = "metrics", id = "metrics_row"
                          , column(width = 12
                            , div(class = "fluent_metric", id = "fluent_metric"
                              , box(width = 3, status = "primary", title = "Fluent"
                                    , uiOutput(NS(id, "fluent_metric"))
                              )
                            )
                            , div(class = "progressing_metric", id = "progressing_metric"
                              , box(width = 3, status = "primary", title = "Progressing"
                                    , uiOutput(NS(id, "progressing_metric"))
                              )
                            )
                            , div(class = "nw_metric", id = "nw_metric"
                              , box(width = 3, status = "primary", title = "Needs Work"
                                    , uiOutput(NS(id, "nw_metric"))
                              )
                            )
                            , div(class = "na_metric", id = "na_metric"
                              , box(width = 3, status = "primary", title = "Not Attempted"
                                    , uiOutput(NS(id, "na_metric"))
                              )
                            )
                          )
                      )
                    )
                    , box(width = 12, status = "primary", title = "Review Grades"
                          , rHandsontableOutput(NS(id,"review_grades"))
                    )
                    , tabBox(width = 12, height = "300px"
                        , tabPanel("Topic Attempts", height = "300px"
                              , column(width = 4
                                       #, "Topic Attempts"
                                       , rHandsontableOutput(NS(id,"topic_attempts"))
                              )
                              , column(width = 8
                                       , "Remaining Topics"
                                       , echarts4rOutput(NS(id,"topic_attempts_bar"), height = "200px")
                              )
                        )
                        , tabPanel("Topic Proficiency", height = "300px"
                              , column(width = 12
                                , echarts4rOutput(NS(id,"topic_proficiency_bar"), height = "200px")
                              )
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
        pivot_longer(cols = c(5:ncol(r$df_review_table))) %>%
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
    output$topic_attempts <- renderRHandsontable({
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
    output$topic_attempts_bar <- renderEcharts4r({
      req(r$is$auth)
      
      df <- df_attempts() %>%
        rename(Topic = topic_id)
      
      #Find which topics the student is fluent in
      df_review_to_topic <- df_review_grades() %>%
        select(topic_id, review_id, grade) %>%
        mutate(count = 1) %>%
        pivot_wider(id_cols = c(topic_id, review_id), names_from = grade, values_from = count) %>%
        mutate_at(c(3:6), ~replace(., is.na(.), 0)) %>%
        group_by(topic_id) %>%
        summarise_at(.vars = vars(c(2:5)), .funs = sum) %>%
        select(Fluent, Topic = topic_id)
      
      df <- merge(x = df, y = df_review_to_topic, by = "Topic", all.x = TRUE)
        
      #Remove rows where student is fluent
      df <- df[df$Fluent == 0,]
      
      df %>%
        e_charts(Topic) %>%
        e_bar(attempts
              , stack = "Topics"
              , color = "#c41230"
              , barWidth = "50%"
              , name = "Previous Attempts") %>%
        e_bar(remaining
              , stack = "Topics"
              , color = "#222D32"
              , barWidth = "50%"
              , name = "Remaining Attempts") %>%
        e_legend(bottom = 'bottom') %>%
        e_tooltip(formatter = htmlwidgets::JS("
        function(params){
          return('value: ' +
          parseFloat((params.value[1] * 10) / 10).toFixed(1))
        }
")) %>%
        e_grid(top = "15%", left= "10%", bottom = "25%", right = "5%") %>%
        e_tooltip()
      
      
    })
    
  
    # Fluency status data ----
    df_review_scores <- reactive({
      df <- df_review_grades() %>%
        select(review_id, topic_id, grade) %>%
        mutate(count = 1) %>%
        pivot_wider(id_cols = c(review_id, topic_id), names_from = grade, values_from = count) %>%
        mutate_at(c(3:6), ~replace(., is.na(.), 0)) %>%
        group_by(review_id) %>%
        summarise_at(.vars = vars(c(2:5)), .funs = sum) %>%
        rename(NC = `Not Completed`)
    
    })
    
    df_current_status <- reactive({
      df <- df_review_grades() %>%
        select(review_id, topic_id, grade) %>%
        mutate(grade_to_int = unlist(lapply(df_review_grades()$grade, grade_to_int))) %>%
        group_by(topic_id) %>%
        summarise(grade = max(grade_to_int)) %>%
        mutate(int_to_grade = unlist(lapply(grade, int_to_grade))) %>%
        mutate(count = 1) %>%
        group_by(int_to_grade) %>%
        summarise(count = sum(count)) 
    })
    
    # Fluency stacked bar -----
    output$topic_proficiency_bar <- renderEcharts4r({
      req(r$is$auth)  
      
      #Get status of fluencies, 
      df <- df_review_scores() %>%
        select(Review = review_id, `Not Attempted` = NC, Progressing, `Needs Work`, Fluent) %>%
        mutate(Description = paste0("Review ", Review))
      
      df %>%
        group_by(Review) %>%
        e_charts(Description) %>%
        e_bar("Not Attempted"
              , stack = "Attempted"
              , color = theme$NC
              , barWidth = "25%"
              , name = "Not Attempted") %>%
        e_bar("Needs Work"
              , stack = "Attempted"
              , color = theme$`Needs Work`
              , barWidth = "25%"
              , name = "Needs Work") %>%
        e_bar(Progressing
              , stack = "Attempted"
              , color = theme$Progressing
              , barWidth = "25%"
              , name = "Progressing") %>%
        e_bar(Fluent
              , stack = "Attempted"
              , color = theme$Fluent
              , barWidth = "25%"
              , name = "Fluent") %>%
        e_legend(bottom = 'bottom') %>%
        e_grid(top = "15%", left= "5%", bottom = "25%", right = "5%") %>%
        e_tooltip()
      
      
    })
    
    # Grades table ----
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
        hot_cols(renderer = "
        function(instance, td, row, col, prop, value, cellProperties) {
          Handsontable.renderers.TextRenderer.apply(this, arguments);
          if (value == 'Fluent'){
            td.style.background = '#225ea8';
            td.style.color = 'white';
          } else if (value == 'Progressing'){
            td.style.color = 'black';
            td.style.background = '#41b6c4';
          } else if (value == 'Needs Work'){
            td.style.background = '#a1dab4';
            td.style.color = 'black';
          } else if (value == 'Not Completed'){
            td.style.background = '#dbdbad';
            td.style.color = 'black';
          }

          if (col > 1){
            if (!isNaN(value)) {
              td.style.background = 'grey';
              cellProperties.readOnly = true;
            }
          }

        }")
    })
    
    
    #Fluent Metric
    output$fluent_metric <- renderUI({
      req(r$is$auth)
      val <-  df_current_status() %>%
        filter(int_to_grade == "Fluent") %>%
        select(count) %>%
        pull()
      df <- if (is_empty(val)) {
        val <- 0
      } 
        
      paste(val)
    })
    
    #Progressing Metric
    output$progressing_metric <- renderUI({
      req(r$is$auth)
      
      val <-  df_current_status() %>%
        filter(int_to_grade == "Progressing") %>%
        select(count) %>%
        pull()
      df <- if (is_empty(val)) {
        val <- 0
      } 
      paste(val)
    })
    
    #Needs Work Metric
    output$nw_metric <- renderUI({
      req(r$is$auth)
      val <-  df_current_status() %>%
        filter(int_to_grade == "Needs Work") %>%
        select(count) %>%
        pull()
      
      df <- if (is_empty(val)) {
        val <- 0
      } 
      
      paste(val)
    })
    
    #Not Attempted Metric
    output$na_metric <- renderUI({
      req(r$is$auth)
      val <-  df_current_status() %>%
        filter(int_to_grade == "Not Attempted") %>%
        select(count) %>%
        pull()
      df <- if (is_empty(val)) {
        val <- 0
      } 
      
      paste(val)
    })
    
  })
}