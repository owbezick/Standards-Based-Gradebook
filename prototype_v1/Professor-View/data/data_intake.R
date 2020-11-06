# library(config)
# # Warning in readLines(con) :
# # incomplete final line found on '/Users/owenbezick/Documents/GitHub/Standards-Based-Gradebook/prototype_v1/Professor-View/config.yml'
# config <- config::get()
# 
# # # Initial authentication caching ----
# # options(gargle_oauth_cache = ".cache") # designate project-specific cache
# # gargle::gargle_oauth_cache() # check the value of the option
# # googledrive::drive_auth() # trigger auth on purpose to store a token in the specified cache
# # cache_directory <- ".secrets/" # can add to config file
# # list.files(cache_directory) # see your token file in the cache
# # drive_deauth() # de auth
# 
# library(googlesheets4)
# library(googledrive)
# 
# # Authenticate drive/ sheets ----
# drive_auth(cache = ".cache",
#            email = "caspencer@davidson.edu")
# 
# gs4_auth(cache = ".cache",
#          email = "caspencer@davidson.edu")
# 
# # Read in data ----
# read_database_sheet <- function(sheet){
#   df <- drive_get(config$database) %>%
#     read_sheet(sheet)
# }
# 
# read_all_database_sheets <- function(sheets){
#   data <- lapply(ls_sheets, read_database_sheet)
#   names(data) <- ls_sheets
#   data
# }
# 
# ls_sheets <- c("student", "course_info", "homework", "homework_grades", "topic", "review_table", "review_grades")
# 
# ls_all_data <- read_all_database_sheets(ls_sheets)
# 

r <- reactiveValues()

r$df_student <- readRDS("data/df_student.RDS")[0,]
r$df_course_info <- readRDS("data/df_course_info.RDS")[0,]
r$df_links <- readRDS("data/df_links.RDS")[0,]
r$df_homework <- readRDS("data/df_homework.RDS")[0,]
r$df_homework_grades <- readRDS("data/df_homework_grades.RDS")[0,0]
r$df_topic <- readRDS("data/df_topic.RDS")[0,]
r$df_review_table <- readRDS("data/df_review_table.RDS")[0,1:4]
r$df_review_grades <-  readRDS("data/df_review_grades.RDS")[0,]


