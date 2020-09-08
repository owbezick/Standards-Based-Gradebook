# Data Intake for Mastery Gradebook
# Author: Owen Bezick

# Helper Functios ----
# Create SQL table from R dataframe
tbl_create <- function(con, data, name) {
  copy_to(
    dest = con,
    df = data,
    name = name,
    overwrite = TRUE,
    temporary = FALSE
  )
}

# Connect to SQL database
db_connect <- function(
  server = "mydbinstance.c0eoxulijuju.us-east-2.rds.amazonaws.com",
  database = "shiny",
  uid = "datacats",
  pwd = "davidson",
  port = 1433,
  tds_version = 9.0,
  local = Sys.getenv('SHINY_PORT') == ""
) {
  if (local) {
    dbConnect(
      odbc(), 
      Driver = "ODBC Driver 17 for SQL Server",
      Server = server, 
      Database = database, 
      uid = uid, 
      pwd = pwd
    )
  } else {
    dbConnect(
      odbc(),
      Driver   = "libtdsodbc.so", 
      Database = database,
      Uid      = uid,
      Pwd      = pwd,
      Server   = server,
      Port     = port 
    )
  }
}

# Used when appending dataframes to correct quotes for SQL 
quotes <- function(df) {
  for (c in 1:ncol(df))
    if (!class(df[,c]) %in% c("numeric", "integer")){
      df[,c] <- sQuote(df[,c], options(useFancyQuotes = FALSE))
    }
  df
}

# Init Data read in ----
# student_def <- read_excel("proposed_database.xlsx", sheet = "student_def")
# exam_def <- read_excel("proposed_database.xlsx", sheet = "exam_def")
# exam_grade <- read_excel("proposed_database.xlsx", sheet = "exam_grade")
# homework_def <- read_excel("proposed_database.xlsx", sheet = "homework_def")
# homework_grade <- read_excel("proposed_database.xlsx", sheet = "homework_grade")

# Create SQL Tables ----
con <- db_connect()
# 
# tbl_create(con, student_def, "student_def")
# tbl_create(con, exam_def, "exam_def")
# tbl_create(con, exam_grade, "exam_grade")
# tbl_create(con, homework_def, "homework_def")
# tbl_create(con, homework_grade, "homework_grade")

# Pull SQL Tables ----
# Students
sql_query <- 'Select * from Shiny.dbo.student_def' 
student_def <- dbGetQuery(con, sql_query) 
student_def <- student_def %>%
  mutate(firstLast = paste(first, last)) 

# Exams
sql_query <- 'Select * from Shiny.dbo.exam_def' 
exam_def <- dbGetQuery(con, sql_query)

sql_query <- 'Select * from Shiny.dbo.exam_grade' 
exam_grade <- dbGetQuery(con, sql_query)

# Homeworks
sql_query <- 'Select * from Shiny.dbo.homework_def' 
homework_def <- dbGetQuery(con, sql_query)

sql_query <- 'Select * from Shiny.dbo.homework_grade' 
homework_grade <- dbGetQuery(con, sql_query)


# Reactive Values
reactive <- reactiveValues(exam_def = exam_def, exam_grade = exam_grade,homework_def = homework_def, homework_grade = homework_grade)

exam_grades <- reactive({
  merge(reactive$exam_def, reactive$exam_grade) %>% merge(student_def) %>%
    mutate(firstLast = paste(first, last)) %>%
    select(firstLast, exam_id, topic_id, grade)
})

homework_grades <- reactive({
  merge(reactive$homework_def, reactive$homework_grade) %>% merge(student_def) %>%
    mutate(firstLast = paste(first, last)) %>%
    select(firstLast, last, homework_id, grade)
})