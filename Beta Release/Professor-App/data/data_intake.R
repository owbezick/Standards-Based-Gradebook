# Initial Data Intake -----
r <- reactiveValues()
r$df_student <- readRDS("data/fresh_data/df_student.RDS")
num_students <- nrow(readRDS("data/fresh_data/df_student.RDS"))
r$df_course_info <- readRDS("data/fresh_data/df_course_info.RDS")
r$df_links <- readRDS("data/fresh_data/df_links.RDS")
r$df_homework <- readRDS("data/fresh_data/df_homework.RDS")
r$df_homework_grades <- readRDS("data/fresh_data/df_homework_grades.RDS")
r$df_topic <- readRDS("data/fresh_data/df_topic.RDS")
r$df_review_table <- readRDS("data/fresh_data/df_review_table.RDS")
r$df_review_grades <-  readRDS("data/fresh_data/df_review_grades.RDS")
r$df_grade_scale <- readRDS("data/fresh_data/df_grade_scale.RDS")

