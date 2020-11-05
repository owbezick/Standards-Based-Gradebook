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
r <- reactiveValues(df_student = NULL
                    , df_course_info = NULL
                    , df_homework = NULL
                    , df_homework_grades = NULL
                    , df_topic = NULL
                    , df_review_table = NULL
                    , df_cal_item = NULL
                    , df_links = NULL
)

course_info <- tibble("Type" = c("type")
       , "Value" = c("value"))

links <- tibble("Link Description" = c("Sample link")
                , "Link URL" = c("davidson.edu"))

r$df_student <- readRDS("data/student.RDS")
r$df_course_info <- course_info[0,]
r$df_links <- links[0, ]
r$df_homework <- readRDS("data/homework.RDS")
r$df_homework_grades <- readRDS("data/homework_grades.RDS")[,1]
r$df_topic <- readRDS("data/topic.RDS")
r$df_review_table <- readRDS("data/review_table.RDS")
r$df_review_grades <-  readRDS("data/review_grades.RDS")


