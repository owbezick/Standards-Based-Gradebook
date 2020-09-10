df_homework_data <- read.xlsx("data/example_data.xlsx", sheet = "homework") %>%
  mutate(homework_date = as.Date(homework_date, origin = "1900-01-01"))

df_review_data <- read.xlsx("data/example_data.xlsx", sheet = "review")%>%
  mutate(review_date = as.Date(review_date, origin = "1900-01-01"))

df_course_info <- tibble(
  location = "Chambers 2187"
  , meeting_times = "Tuesday and Thursday, 9:40am - 10:55am"
  , office_hours = "Monday and Wednesday, 10:00am - 12:30pm"
  , link = "https://mastering-shiny.org/"
  , link_description = "Davidson Website"
)