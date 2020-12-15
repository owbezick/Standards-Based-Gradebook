# Homework UI ----
homework_UI <- function(id) {
  tabPanel(title = "Homework"
           , fluidRow(
             box(width = 12, status = "primary"
                 , title = "Edit Homework Grades"
                 , rHandsontableOutput(NS(id, "homework_table"))
                 , br()
                 , actionBttn(NS(id, "save"), "Save", style = "material-flat", block = T)
             )
             , br()
             , box(width = 12, status = "primary"
                   , title = textOutput(NS(id, "title"))
                   , echarts4rOutput(NS(id,'homework_bar'))
             )
           )
  )
}

# Homework serrver ----
homework_server <- function(id, r){
  moduleServer(id, function(input, output, session){

    output$title <- renderText(
      paste0("Homework Grades (Class Averages)")
    )
    # Homework Bar Chart ----
    output$homework_bar <- renderEcharts4r({
      df <- r$df_homework_grades %>%
        select(-c(`Student Name`)) %>%
        mutate_all(.funs = as.double) %>%
        colMeans(na.rm = T) %>%
        as.matrix()


      df <- df[,1] %>%
        as.data.frame() %>%
        rename(Average = ".") %>%
        mutate(average = Average / 100) %>%
        mutate(Chart = "chart") %>%
        select(Average = average, chart = Chart)

      df$id <- rownames(df)

      chart <- df %>%
        e_chart(id) %>%
        e_bar(Average, barWidth = "50%") %>%
        e_legend(show = F) %>%
        e_y_axis(formatter = e_axis_formatter("percent", digits = 2)) %>%
        e_color(color = "#c41230") %>%
        e_grid(left = "15%", right = "5%", top = "10%", bottom = "10%") %>%
        e_labels(position = "top", formatter  = htmlwidgets::JS("
                                    function(params){
                                      return(parseFloat(params.value[1]*100).toFixed(2) +'%');
                                    }
                                    ")) %>%
        e_tooltip()
    })

    # Homework Table ----
    output$homework_table <- renderRHandsontable({
      df_homework_grades <- r$df_homework_grades
      if (ncol(df_homework_grades) == 1){
        rhandsontable(df_homework_grades
                      , rowHeaders = NULL
                      , stretchH = 'all')%>%
          hot_context_menu(allowRowEdit = FALSE)
      } else{
        rhandsontable(df_homework_grades
                      , rowHeaders = NULL
                      , stretchH = 'all') %>%
          hot_context_menu(allowRowEdit = FALSE) %>%
          hot_col(col = c(2:ncol(df_homework_grades)), type = "numeric")
      }

    })
    
    # Save homework
    observeEvent(input$save,{
      df_hot <- hot_to_r(input$homework_table)
      
      #browser()
      r$df_homework_grades  <- df_hot
      write_rds(r$df_homework_grades, "data/df_homework_grades.RDS")
      showNotification("Saved in session.")
    })

  }) #end module server
}

# Review UI ----
review_UI <- function(id) {
  tabPanel(title = "Reviews"
           , fluidRow(
             tabBox(width = 12
                    # Review by Review ----
                    , tabPanel(title = "Review Grade by Student"
                               , box(width = 12, status = "primary"
                                     , title = "Review Grade by Student"
                                     , rHandsontableOutput(NS(id, "review_table_review"), width = "100%")
                               )
                               , actionBttn(NS(id, "saveReview"), "Save", style = "material-flat", block = T)
                    )
                    # Review by Student ----
                    , tabPanel(title = "Review Grade by Topic"
                               , box(width = 12, status = "primary"
                                     , title = "Review Grade by Topic"
                                     , rHandsontableOutput(NS(id, "review_table_student"), width = "100%")
                               )
                               , actionBttn(NS(id, "saveStudent"), "Save", style = "material-flat", block = T)
                    )
                    # Summary ----
                    , tabPanel(title = "Summary"
                               , box(width = 12, status = "primary", title = "Progress Summary"
                                     , rHandsontableOutput(NS(id, "review_table_summary"), width = "100%")
                                     , br()
                                     , rHandsontableOutput(NS(id, "topic_completion"), width = "100%"))
                    )
                    
                    
             )
           )
  )
}

review_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    
    df_review_summary <- reactive({
      df_review_grades <- r$df_review_grades
      df_student <- r$df_student
      df_review <- df_review_grades %>%
        left_join(df_student, by = "student_id") %>%
        pivot_wider(id_cols = c(review_id, topic_id), names_from = name, values_from = grade) %>%
        rename(`Review ID` = review_id, `Topic ID` = topic_id)
      
      df_review_summary <- df_review %>%
        group_by(`Topic ID`) %>%
        mutate_at(c(3:(nrow(df_student) + 2)),  max_grade) %>%
        ungroup() %>%
        filter(!duplicated(`Topic ID`)) %>%
        select(-`Review ID`) %>%
        mutate(`Topic ID` = as.numeric(`Topic ID`))
      
      df_review_summary <- df_review_summary[order(df_review_summary$`Topic ID`),] %>%
        mutate(`Topic ID` = as.character(`Topic ID`))
    })
    
    
    # Review by Review ----
    output$review_table_review <- renderRHandsontable({
      df_review_grades <- r$df_review_grades
      df_student <- r$df_student
      df_review <- df_review_grades %>%
        left_join(df_student, by = "student_id") %>%
        pivot_wider(id_cols = c(review_id, topic_id), names_from = name, values_from = grade) %>%
        rename(`Review ID` = review_id, `Topic ID` = topic_id)
      df_review <- df_review %>%
        group_by(`Review ID`) %>%
        mutate(`Topic ID` = as.numeric(`Topic ID`)) %>%
        arrange(`Review ID`, `Topic ID`) %>%
        mutate(`Topic ID` = as.character(`Topic ID`))

      column_names <- names(df_review)
      student_names <- column_names[3:length(column_names)]
      reviews <- df_review %>%
        select(id = `Review ID`) %>%
        unique()
      
      #Generate borders around reviews
      borders = list()
      for (i in 1:(nrow(reviews))){
        #Find start and end rows in table
        reviewRows <- which(grepl(i, df_review$`Review ID`))
        firstRow <- reviewRows[[1]] - 1
        lastRow <- reviewRows[[length(reviewRows)]] - 1
        
        #Load borders lists
        borders[[length(borders)+1]] <- list(
          range = list(from = list(row = firstRow, col = 0)
                       , to = list(row = lastRow, col = length(student_names) + 1))
          , top = list(width = 2, color = "#222D32")
          , left = list(width = 2, color = "#222D32")
          , bottom = list(width = 2, color = "#222D32")
          , right = list(width = 2, color = "#222D32"))
      }

      grade_types <- r$df_grade_scale$title
      grade_types[length(grade_types)+1] <- "NA"
      
      
      rhandsontable(df_review
                    , rowHeaders = NULL
                    , stretchH = 'all') %>%
        hot_table(customBorders = borders) %>%
        hot_col(col = "Review ID", readOnly = T) %>%
        hot_col(col = "Topic ID", readOnly = T) %>%
        hot_cols(type = "dropdown", source = grade_types) %>%
        hot_cols(renderer = handsontable_renderer()) %>%
        hot_context_menu(allowRowEdit = FALSE)
    })
    
    # Review grade summary ----
    output$review_table_summary <- renderRHandsontable({
      df_review_summary <- df_review_summary()
      
      rhandsontable(df_review_summary
                    , rowHeaders = NULL
                    , stretchH = 'all'
                    , readOnly = T) %>%
        hot_cols(renderer = handsontable_renderer()) %>%
        hot_context_menu(allowRowEdit = FALSE)
    })
    
    output$topic_completion <- renderRHandsontable({
      completed <- r$df_grade_scale$title[1]
      df_review_summary <- df_review_summary()
      
      df_num_completed <- df_review_summary %>%
        gather(Name, value, c(2:ncol(df_review_summary))) %>%
        group_by(Name) %>%
        tally(value == completed) %>%
        ungroup() %>%
        select(`Student Name` = Name, `Topics Completed` = n)
      
      
      rhandsontable(df_num_completed
                      , rowHeaders = NULL
                      , stretchH = 'all'
                      , readOnly = T
                      , height = "100%"
                      , width = "100%") %>%
        hot_context_menu(allowRowEdit = FALSE)
      
    })
    
    # Save Review by Student ----
    observeEvent(input$saveReview,{
      r$df_review_grades <- hot_to_r(input$review_table_review) %>%
        pivot_longer(cols = c(3:ncol(hot_to_r(input$review_table_review)))) %>%
        left_join(r$df_student, by = "name") %>%
        select(review_id = `Review ID`, topic_id = `Topic ID`, student_id, grade = value)
      write_rds(r$df_review_grades, "data/df_review_grades.RDS")
      showNotification("Saved in session.")
    })

    # Review by Topic ----
    output$review_table_student <- renderRHandsontable({
      df_review_grades <- r$df_review_grades
      df_student <- r$df_student
      df_review_topic <- df_review_grades %>%
        left_join(df_student, by = "student_id") %>%
        mutate(`topic_id` = as.numeric(`topic_id`)) %>%
        arrange(`topic_id`) %>%
        mutate(`topic_id` = as.character(`topic_id`)) %>%
        pivot_wider(id_cols = c(review_id, name, topic_id)
                    , names_from = topic_id
                    , values_from = grade
                    , names_prefix = "Topic ") %>%
        rename(`Review ID` = review_id, `Student Name` = name)

      df_review_topic <- df_review_topic %>%
        group_by(`Student Name`) %>%
        arrange(`Student Name`)
      column_names <- names(df_review_topic)
      topic_names <- column_names[3:length(column_names)]
      
      grade_types <- r$df_grade_scale$title
      grade_types[length(grade_types)+1] <- "NA"

      borders = list()
      for (name in df_student$name){
        #Find start and end rows in table
        reviewRows <- which(grepl(name, df_review_topic$`Student Name`))
        firstRow <- reviewRows[[1]] - 1
        lastRow <- reviewRows[[length(reviewRows)]] - 1
        
        #Load borders lists
        borders[[length(borders)+1]] <- list(
          range = list(from = list(row = firstRow, col = 0)
                       , to = list(row = lastRow, col = length(topic_names) + 1))
          , top = list(width = 2, color = "#222D32")
          , left = list(width = 2, color = "#222D32")
          , bottom = list(width = 2, color = "#222D32")
          , right = list(width = 2, color = "#222D32"))
      }
      
      rhandsontable(df_review_topic
                    , rowHeaders = NULL
                    , stretchH = 'all'
                    , options = c(filters = T)) %>%
        hot_table(customBorders = borders) %>%
        hot_col(col = "Review ID", readOnly = T, type = "character") %>%
        hot_col(col = "Student Name", readOnly = T) %>%
        hot_cols(type = "dropdown", source = grade_types)  %>%
        hot_cols(renderer = handsontable_renderer())%>%
        hot_context_menu(allowRowEdit = FALSE)

    })

    # Save by Topic ----
    observeEvent(input$saveStudent,{
      df_temp <- hot_to_r(input$review_table_student) %>%
        pivot_longer(cols = c(3:ncol(hot_to_r(input$review_table_student)))) %>%
        left_join(r$df_student, by = c("Student Name" = "name")) %>%
        na.omit()

      r$df_review_grades <- df_temp %>%
        mutate(topic_id = str_split_fixed(df_temp$name, " ", 2)[,2]) %>%
        select(review_id = `Review ID`, topic_id, student_id, grade = value)
      write_rds(r$df_review_grades, "data/df_review_grades.RDS")
      showNotification("Saved in session.")
    })
  })
}
