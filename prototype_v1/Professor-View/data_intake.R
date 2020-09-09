df_homework_data <- read.xlsx("example_data.xlsx", sheet = "homework") %>%
  mutate(homework_date = as.Date(homework_date, origin = "1900-01-01"))

df_review_data <- read.xlsx("example_data.xlsx", sheet = "review")%>%
  mutate(review_date = as.Date(review_date, origin = "1900-01-01"))
