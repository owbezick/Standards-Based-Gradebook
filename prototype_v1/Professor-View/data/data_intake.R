# df_homework_data <- read.xlsx("example_data.xlsx", sheet = "homework") %>%
#   mutate(homework_date = as.Date(homework_date, origin = "1900-01-01"))
# 
# df_review_data <- read.xlsx("example_data.xlsx", sheet = "review")%>%
#   mutate(review_date = as.Date(review_date, origin = "1900-01-01"))
# 
# df_course_info <- tibble(
#   location = "Chambers 2187"
#   , meeting_times = "Tuesday and Thursday, 9:40am - 10:55am"
#   , office_hours = "Monday and Wednesday, 10:00am - 12:30pm"
#   , link = "https://mastering-shiny.org/"
#   , link_description = "Davidson Website"
# )

library(config)
config <- config::get()

# Initial authentication caching ----
options(gargle_oauth_cache = ".cache") # designate project-specific cache
gargle::gargle_oauth_cache() # check the value of the option
googledrive::drive_auth() # trigger auth on purpose to store a token in the specified cache
cache_directory <- ".secrets/" # can add to config file
list.files(cache_directory) # see your token file in the cache
drive_deauth() # de auth

library(googlesheets4)
library(googledrive)

drive_auth(cache = ".cache",
           email = "caspencer@davidson.edu")

gs4_auth(cache = ".cache",
         email = "caspencer@davidson.edu")


# Read in data ----
read_database_sheet <- function(sheet){
  df <- drive_get(config$database) %>%
    read_sheet(sheet)
}

read_all_database_sheets <- function(sheets){
  data <- lapply(ls_sheets, read_database_sheet)
  names(data) <- ls_sheets
  data
}

ls_sheets <- c("student", "homework", "homework_grade", "exam_to_topic", "topic", "exam")

ls_all_data <- read_all_database_sheets(ls_sheets)

df_student <- ls_all_data$student

df_homework <- ls_all_data$homework

df_homework_grade <- ls_all_data$homework_grade

df_exam_to_topic <- ls_all_data$exam_to_topic

df_topic <- ls_all_data$topic

df_exam <- ls_all_data$exam

#default:
#database: "template_gradebook_database"
