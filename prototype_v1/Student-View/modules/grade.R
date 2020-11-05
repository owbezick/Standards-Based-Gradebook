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
        e_grid(top = "15%", left= "10%", bottom = "20%", right = "5%") %>%
        e_tooltip()
      
      
    })
    
    
    #TODO find a simpler way to do ALL of this
    # Fluency status data ----
    df_status <- reactive({
      df <- df_review_grades() %>%
        select(review_id, topic_id, grade)
      
      #Add column with integer grades
      df$int_grade <- apply(as.matrix(df$grade), 1, grade_to_int)
      
      #get number of unique topics
      numTopics <- r$df_topic %>%
        select(topic_id) %>%
        unique() %>%
        nrow()
      
      #get number of unique reviews
      numReviews <- df %>%
        select(review_id) %>%
        unique() %>%
        nrow()
      
      #generate tempate df with rows for status of each topic at each review
      df_template <- tibble("int_grade" = rep(NA, numTopics*numReviews))
      df_template$topic_id <- rep(1:numTopics, each = numReviews)
      df_template$review_id <- rep(1:numReviews, length.out = numTopics*numReviews)
      df_template$grade <- rep("Not Completed", each = numTopics*numReviews)
      
      #Match column types
      df$topic_id <- as.integer(df$topic_id)
      df$review_id <- as.integer(df$review_id)
      
      #Append template (filler) rows to grade df
      df_status <- df %>%
        rbind(df_template) %>%
        transform(grade_id = paste0(topic_id, "_", review_id))
      
      #Get rid of duplicate columns (the filler rows from tempate df get replaced by actual grades)
      df_status <- df_status[!duplicated(df_status[,c('grade_id')]),] 
      
      #Sort
      df_status <- df_status[order(df_status$review_id, df_status$topic_id),]
      
      #Fill in NA grades to show updated progress
      df_status <- df_status %>%
        group_by(topic_id) %>%
        fill(int_grade)
      
      #Replace NAs with 0s
      df_status$int_grade <- replace_na(df_status$int_grade, 0)
      
      #Fill in grade strings from integers
      df_status$grade <- apply(as.matrix(df_status$int_grade), 1, int_to_grade)
      
      #count each type of grade grouped by review, pivot so grades are columns
      df_status <- df_status %>%
        select(review_id, topic_id, grade) %>%
        group_by(review_id) %>%
        count(grade) %>%
        pivot_wider(names_from = grade, values_from = n, values_fill = 0)
      
      #Check if columns are missing
      if (is.null(df_status$`Not Atttempted`)) {
        df_status$`Not Attempted` <- c(as.integer(rep(0, numReviews)))
      }
      if (is.null(df_status$Fluent)) {
        df_status$Fluent <- c(as.integer(rep(0, numReviews)))
      }
      if (is.null(df_status$Progressing)) {
        df_status$Progressing <- c(as.integer(rep(0, numReviews)))
      }
      if (is.null(df_status$`Needs Work`)) {
        df_status$`Needs Work` <- c(as.integer(rep(0, numReviews)))
      }
      
      return(df_status)
      
    })
    
    # Fluency stacked bar -----
    output$topic_proficiency_bar <- renderEcharts4r({
      req(r$is$auth)
      
      
      #Get status of fluencies, 
      df <- df_status() %>%
        select(Review = review_id, `Not Attempted` = NC, Progressing, `Needs Work`, Fluent) %>%
        mutate(Description = paste0("Review ", Review))
      
      # %>%
      #   group_by(review_id, .add = FALSE) %>% 
      #   group_split()
      
      
        
      
      df %>%
        group_by(Review) %>%
        e_charts(Description) %>%
        e_bar(`Not Attempted`
              , stack = "Not Attempted"
              , color = theme$NC
              , barWidth = "25%"
              , name = "Not Attempted") %>%
        e_bar(`Needs Work`
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
      
      
            # attempted to use glue()
      # hot_cols(renderer = glue("
      #   function(instance, td, row, col, prop, value, cellProperties) {{
      #     Handsontable.renderers.TextRenderer.apply(this, arguments);
      #     if (value == 'Fluent'){{ d
      #       td.style.background = '{theme$Fluent}';
      #     }} else if (value == 'Progressing'){{ d
      #       td.style.background = '{theme$Progressing}';
      #     }} else if (value == 'Needs Work'){{
      #       td.style.background = '{theme$`Needs Work`}';
      #     }} else if (value == 'Not Completed'){{
      #       td.style.background = '{theme$NC}';
      #     }}
      # 
      #     if (col > 1){{
      #       if (!isNaN(value)) {{
      #         td.style.background = '{theme$NC}';
      #         cellProperties.readOnly = true;
      #       }}
      #     }}
      # 
      #   }}"))
    })
    
    
    #Fluent Metric
    output$fluent_metric <- renderUI({
      req(r$is$auth)
      df <- df_status()
      val <- df$Fluent[nrow(df)]
        
      paste(val)
    })
    
    #Progressing Metric
    output$progressing_metric <- renderUI({
      req(r$is$auth)
      
      df <- df_status()
      val <- df$Progressing[nrow(df)]
      
      paste(val)
    })
    
    #Needs Work Metric
    output$nw_metric <- renderUI({
      req(r$is$auth)
      df <- df_status()
      val <- df$`Needs Work`[nrow(df)]
      
      paste(val)
    })
    
    #Not Attempted Metric
    output$na_metric <- renderUI({
      req(r$is$auth)
      df <- df_status()
      val <- df$`Not Attempted`[nrow(df)]
      
      paste(val)
    })
    
  })
}