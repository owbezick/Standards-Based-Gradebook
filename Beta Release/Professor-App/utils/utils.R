max_grade <- function(vec){
  max <- nrow(r$df_grade_scale)
  
  for (x in vec){
    levelInt <- getLevelInt(x)
    
    if (levelInt != -1){
      #'max' goes up to 1, actually finding min
      if (levelInt < max){
        max <- levelInt
      }
    }
  }
  return(rep(getLevelTitle(max), length(vec)))
}

`%notin%` <- Negate(`%in%`)

#Helper function for report generation naming in app.R
make_filename <- function(name){
  if (name == "All Students"){
    return(
      "Class-" %>%
        paste0(Sys.Date()) %>%
        paste0("-Report.html")
      )
  } else {
    return (
      name %>%
      str_replace_all(" ", "-") %>%
        paste0("-") %>%
        paste0(Sys.Date()) %>%
        paste0("-Report.html")
    )
  }
}


save_df_homework_grades <- function(){
  # Save to df_homework_grades ----
  df_homework_grades <- r$df_homework_grades
  
  # Build table from scratch with "NA" for all grades (NEW)
  df_NA_homework_grades <- tibble(
    homework_id = sort(
      rep(
        r$df_homework$id
        , length(r$df_student$student_id)
      )
    )
    , student_id = rep(
      r$df_student$student_id
      , length(r$df_homework$id)
    )
  ) %>%
    mutate(grade = "NA") %>%
    left_join(r$df_student) %>%
    pivot_wider(id_cols = c(homework_id, name)
                , names_from = homework_id
                , names_prefix = "Homework "
                , values_from = grade) %>%
    rename(`Student Name` = name)
  
  # If df_homework_grades has no data
  if(nrow(df_homework_grades) == 0){
    r$df_homework_grades <- df_NA_homework_grades
    write_rds(r$df_homework_grades, "data/df_homework_grades.RDS")
  } else{
    # Merge tables together, keeping values from current and adding values from NA
    current_homework_grade_long <- r$df_homework_grades %>%
      pivot_longer(cols = c(2:ncol(r$df_homework_grades)), names_to = "Homework", values_to = "grade") %>%
      mutate(unique_id = paste(`Student Name`, Homework)) 
    
    NA_homework_grade_long <- df_NA_homework_grades %>%
      pivot_longer(
        cols = c(2:ncol(df_NA_homework_grades))
        , names_to = "Homework", values_to = "grade"
      ) %>%
      mutate(unique_id = paste(`Student Name`, Homework)) 
    
    # filter out deleted homework IDs 
    filtered_current_homework_grade_long <- filter(current_homework_grade_long, unique_id %in% NA_homework_grade_long$unique_id)
    # Filter out existing homework IDs
    new_NA_homework_grade_long <- filter(NA_homework_grade_long, unique_id %notin% filtered_current_homework_grade_long$unique_id)
    # add in new homework ID
    r$df_homework_grades <- rbind(filtered_current_homework_grade_long, new_NA_homework_grade_long) %>%
      arrange(Homework) %>%
      pivot_wider(id_cols = c(`Student Name`, Homework), names_from = Homework, values_from = grade) %>%
      arrange(`Student Name`)
    write_rds(r$df_homework_grades, "data/df_homework_grades.RDS")
  }
  
}

#Function to find fluency (string) title given (integer) level
getLevelTitle <- function(x){
  level <- r$df_grade_scale %>%
    filter(level == x) %>%
    select(title) %>%
    pull()
  
  return (level)
}

#Function to find fluency (int) title given (string) level
getLevelInt <- function(x){
  level <- r$df_grade_scale %>%
    filter(title == x) %>%
    select(level) %>%
    pull()
  
  if (length(level) == 0){
    level <- -1
  }
  
  return (level)
}

save_df_review_grades <- function(){
  df_student <- r$df_student %>%
    select(student_id)
  
  data_from_hot <- r$df_review_table %>%
    pivot_longer(cols = c(5:ncol(r$df_review_table))) %>%
    na.omit()
  
  review_topic_id_hot <- data_from_hot %>%
    filter(value == "TRUE")
  
  review_topic_id_hot <- review_topic_id_hot %>%
    mutate(topic_id = str_split_fixed(review_topic_id_hot$name, " ", 2)[,2]) %>%
    select(review_id = `Review ID`, topic_id)
  
  number_of_topics <- nrow(review_topic_id_hot)
  
  # replicate review_id and topic_id for as many students that are in the class
  new_review_topic_rep <- do.call("rbind", replicate(nrow(df_student), review_topic_id_hot, simplify = FALSE))
  
  # replicate student_id the for as many topics being added
  student_id <- do.call("rbind", replicate(nrow(review_topic_id_hot), df_student, simplify = FALSE)) %>%
    arrange(student_id)
  # browser()
  df_review_grades <- r$df_review_grades  %>%
    filter(grade != "NA") %>%
    mutate(
      filter_id = paste(review_id, topic_id, student_id)
    )
  
  df_new_review_data <- new_review_topic_rep %>%
    mutate(student_id = student_id$student_id
           , grade = rep("NA", nrow(new_review_topic_rep))
           , filter_id = paste(review_id, topic_id, student_id)) %>%
    filter(filter_id %notin% df_review_grades$filter_id) %>%
    rbind(df_review_grades) %>%
    arrange(review_id, topic_id) %>%
    select(-c(filter_id))
  
  
  r$df_review_grades <- df_new_review_data
  write_rds(r$df_review_grades, "data/df_review_grades.RDS")
}

handsontable_renderer <- function(){
  return(
    paste0("
          function(instance, td, row, col, prop, value, cellProperties) {
            Handsontable.renderers.TextRenderer.apply(this, arguments);
            if (value == '", getLevelTitle(1), "'){
              td.style.background = 'lightgreen';
            } else if (value == '", getLevelTitle(2), "'){
              td.style.background = 'lightyellow';
            } else if (value == '", getLevelTitle(3), "'){
              td.style.background = 'pink';
            } else if (value == '", getLevelTitle(4), "'){
              td.style.background = 'lightgrey';
            }
  
            if (col > 1){
              if (!isNaN(value)) {
                td.style.background = 'grey';
              cellProperties.readOnly = true;
              }
            }
  
          }")
  )
}
