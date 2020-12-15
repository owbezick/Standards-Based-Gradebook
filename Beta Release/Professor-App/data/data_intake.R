# Initial Data Intake -----
r <- reactiveValues()
r$df_student <- readRDS("data/df_student.RDS")
num_students <- nrow(readRDS("data/df_student.RDS"))
r$df_course_info <- readRDS("data/df_course_info.RDS")
r$df_links <- readRDS("data/df_links.RDS")
r$df_homework <- readRDS("data/df_homework.RDS")
r$df_homework_grades <- readRDS("data/df_homework_grades.RDS")
r$df_topic <- readRDS("data/df_topic.RDS")
r$df_review_table <- readRDS("data/df_review_table.RDS")
r$df_review_grades <-  readRDS("data/df_review_grades.RDS")
r$df_grade_scale <- readRDS("data/df_grade_scale.RDS")

# Reset all values -----
# df_student <- readRDS("data/df_student.RDS")[0,c(1,2)]
# num_students <- nrow(readRDS("data/df_student.RDS")[0,c(1,2)])
# df_course_info <- readRDS("data/df_course_info.RDS")[0,c(1,2)]
# df_links <- readRDS("data/df_links.RDS")
# df_homework <- readRDS("data/df_homework.RDS")[0, c(1:4)]
# df_homework_grades <- readRDS("data/df_homework_grades.RDS")[0,1]
# df_topic <- readRDS("data/df_topic.RDS")[0,c(1,2)]
# df_review_table <- readRDS("data/df_review_table.RDS")[0,c(1:4)]
# df_review_grades <-  readRDS("data/df_review_grades.RDS")[0, c(1:4)]
# df_grade_scale <- readRDS("data/df_grade_scale.RDS")

# write_rds(df_student, "data/df_student.RDS")
# write_rds(df_course_info, "data/df_course_info.RDS")
# write_rds(df_links, "data/df_links.RDS")
# write_rds(df_homework, "data/df_homework.RDS")
# write_rds(df_homework_grades, "data/df_homework_grades.RDS")
# write_rds(df_grade_scale, "data/df_grade_scale.RDS")
# write_rds(df_review_grades, "data/df_review_grades.RDS")
# write_rds(df_review_table, "data/df_review_table.RDS")
# write_rds(df_topic, "data/df_topic.RDS")

# Import New Data -----
# path_to_xlsx <- "data/grade_data.xlsx"
# df_student <- read.xlsx(path_to_xlsx, sheet = "roster")
# df_course_info <- read.xlsx(path_to_xlsx, sheet = "course_info")
# df_homework <- read.xlsx(path_to_xlsx, sheet = "homeworks")
# df_review_table <- read.xlsx(path_to_xlsx, sheet = "reviews")
# df_topic <- read.xlsx(path_to_xlsx, sheet = "topics")
# df_review_grades <- read.xlsx(path_to_xlsx, sheet = "homework_grades")
# df_review_grades <- read.xlsx(path_to_xlsx, sheet = "review_grades")
# df_grade_scale <- read.xlsx(path_to_xlsx, sheet = "grade_scale")
# df_links <- read.xlsx(path_to_xlsx, sheet = "course_links")

# write_rds(df_student, "data/df_student.RDS")
# write_rds(df_course_info, "data/df_course_info.RDS")
# write_rds(df_links, "data/df_links.RDS")
# write_rds(df_homework, "data/df_homework.RDS")
# write_rds(df_homework_grades, "data/df_homework_grades.RDS")
# write_rds(df_grade_scale, "data/df_grade_scale.RDS")
# write_rds(df_review_grades, "data/df_review_grades.RDS")
# write_rds(df_review_table, "data/df_review_table.RDS")
# write_rds(df_topic, "data/df_topic.RDS")
