grade_max <- function(x){
  max <- "NA"
  for (grade in x){
    if (max != "M" & grade == "M"){
      max <- "M"
    } else if (max != "J" & max != "M" & grade == "J"){
      max <- "J"
    } else if (max != "A" & max != "J" & max != "M" & grade == "A"){
      max <- "A"
    } 
  }
  return(max)
}

`%notin%` <- Negate(`%in%`)


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
  }
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
}
