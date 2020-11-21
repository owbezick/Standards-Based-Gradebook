# Mastery Gradebook application - Professor View
# Author: Owen Bezick & Calvin Spencer
source("utils/libraries.R")
source("utils/utils.R")
source("data/data_intake.R")
source("modules/wizard.R")
source("modules/course_info.R")
source("modules/course_calendar.R")
source("modules/edit_reviews.R")
source("modules/edit_course_info.R")
source("modules/edit_and_add.R")
source("modules/edit_homework.R")
source("modules/edit_topic.R")
source("modules/edit_roster.R")
source("modules/edit_grades.R")
source("modules/view_calendar_item.R")
source("modules/edit_grade_scale.R")

ui <- dashboardPage(
    skin = "black"
    , dashboardHeader(
        title = "Professor View" 
        , tags$li(class = "dropdown"
                  , tags$img(
                      height = "40px"
                      , src= 'davidson_logo_white.png'
                      , hspace = "4"
                      , vspace = "4"
                  )
        )
    )
    , dashboardSidebar( 
        sidebarMenu(
            menuItem(
                tabName = "home"
                , text = "Home"
                , icon = icon("home")
            )
            , menuItem(
                tabName = "grades"
                , text = "Grades"
                , icon = icon("chalkboard")
            )
            , column(width = 12
                     , fluidRow(
                         downloadButton("report", "Generate Report", class = "download-button")
                         )
                     , br()
                     , fluidRow(
                         downloadButton("downloadData", "Download Data", class = "download-button")
                         
                     )
            )
        )
    )
    , dashboardBody(
        includeCSS("utils/style.css")
        # Initial Data Set Up ----
        , uiOutput("wizard")
        , tabItems(
            tabItem(
                # Home tab UI ----
                tabName = "home"
                , fluidRow(
                    course_info_UI("courseinfo")
                    , course_link_UI("courselink")
                    , edit_and_add_UI("edit_and_add")
                )
                , fluidRow(
                    course_calendar_UI("course_calendar")
                )
                , fluidRow(
                    topics_UI("topics")
                )
            )
            , tabItem(
                # Grades tab UI ----
                tabName = "grades"
                , tabBox(title = "Grades", width = 12
                         , homework_UI("homework")
                         , review_UI("review")
                )
            )
        )
    )
)


# Define server logic 
server <- function(input, output, session) {
    # Data Wizard ----
    output$wizard <- renderUI({
        showModal(modalDialog(title = "Initial Data Input"
                              , size = "l"
                              , footer = "Note: more data can be added and edited later in the app."
                              , easyClose = F
                              , fluidRow(
                                  wizardUI("dataWizard"
                                           , actionBttn(
                                               inputId = "closeWizard"
                                               , label = "Close Wizard"
                                               , style = "material-flat"
                                               , block = T
                                           ) 
                                  ) 
                              )
        )
        )
    })
    
    wizard_server("dataWizard", r)
    
    observeEvent(input$closeWizard, {
        if(nrow(r$df_review_grades) > 0){
            removeModal()
        }else{
            showNotification("Please add a review!", type = "error")
        }
    })
    
    
    # Other server calls ----
    course_calendar_server("course_calendar", r)
    course_info_server("courseinfo", r)
    course_link_server("courselink", r)
    edit_and_add_server("edit_and_add", r)
    topics_server("topics", r)
    view_calendar_hw_Server("calendar_hw", r)
    view_calendar_review_Server("calendar_review", r)
    homework_server("homework", r)
    review_server("review", r)
    
    # Download data ---- 
    output$downloadData <- downloadHandler(
        filename = function(){paste0("grade_data", Sys.Date(), ".xlsx")}
        , content = function(file){
            wb <- createWorkbook(title = "saveMe.xlsx")
            # Create a worksheets
            addWorksheet(wb, sheetName = "roster")
            addWorksheet(wb, sheetName = "homeworks")
            addWorksheet(wb, sheetName = "reviews")
            addWorksheet(wb, sheetName = "topics")
            addWorksheet(wb, sheetName = "homework_grades")
            addWorksheet(wb, sheetName = "review_grades")
            
            # write data to sheets
            writeData(wb , sheet = "roster", r$df_student)
            writeData(wb , sheet = "homeworks", r$df_homework)
            writeData(wb , sheet = "reviews", r$df_review_table)
            writeData(wb , sheet = "topics", r$df_topic)
            writeData(wb , sheet = "homework_grades", r$df_homework_grades)
            writeData(wb , sheet = "review_grades", r$df_review_grades)
            
            # Save workbook - this actually writes the file 'saveMe.xlsx' to disk
            saveWorkbook(wb, file)
            
            # clean up 
            file.remove("saveMe.xlsx")
        }
    )
    
    # Report generation ----
    output$report <- downloadHandler(
        
        # For PDF output, change this to "report.pdf"
        filename = "report.html",
        content = function(file) {
            # Copy the report file to a temporary directory before processing it, in
            # case we don't have write permissions to the current working dir (which
            # can happen when deployed).
            tempReport <- file.path(tempdir(), "report.Rmd")
            file.copy("report.Rmd", tempReport, overwrite = TRUE)
            # Set up parameters to pass to Rmd document
            student_table <- r$df_student %>%
                rename(`Student ID` = student_id, Name = name) %>%
                rhandsontable(rowHeaders = NULL, stretchH = 'all', readOnly = T, height = "100%", width = "100%")
            
            topic_table <- r$df_review_table %>%
                mutate(`Review Start Date` = ymd(`Review Start Date`)
                       , `Review End Date` = ymd(`Review End Date`)
                       , `Review ID` = as.character(`Review ID`)) %>%
                rhandsontable(rowHeaders = NULL, stretchH = 'all', readOnly = T, height = "100%", width = "100%")
            
            homework_table <- r$df_homework %>%
                mutate(id = id
                       , Description = description
                       , `Date Assigned` = ymd(date_assigned)
                       , `Due Date` = ymd(date_due)) %>%
                select(`Homework ID` = id, Description, `Date Assigned`, `Due Date`) %>%
                rhandsontable(rowHeaders = NULL, stretchH = 'all', readOnly = T, height = "100%", width = "100%")
            
            df_review_grades <- r$df_review_grades
            df_student <- r$df_student
            df_review_topic <- df_review_grades %>%
                left_join(df_student, by = "student_id") %>%
                mutate(`topic_id` = as.numeric(`topic_id`)) %>%
                arrange(`topic_id`) %>%
                mutate(`topic_id` = as.character(`topic_id`)) %>%
                pivot_wider(id_cols = c(review_id, name, topic_id)
                            , names_from = topic_id
                            , values_from = grade
                            , names_prefix = "Topic ") %>%
                rename(`Review ID` = review_id, `Student Name` = name)
            
            df_review_topic <- df_review_topic %>%
                group_by(`Student Name`) %>%
                arrange(`Student Name`)
            column_names <- names(df_review_topic)
            topic_names <- column_names[3:length(column_names)]
            grade_types <- factor(c("NA", "Not Completed", "Fluent", "Progressing", "Needs Work"))
            
            review_grades <- rhandsontable(df_review_topic
                                           , rowHeaders = NULL
                                           , stretchH = 'all'
                                           , readOnly = T
                                           , options = c(filters = T)
                                           , height = "100%"
                                           , width = "100%") %>%
                hot_col(col = "Review ID", readOnly = T, type = "character") %>%
                hot_col(col = "Student Name", readOnly = T) %>%
                hot_cols(type = "dropdown", source = grade_types)  %>%
                hot_cols(renderer = handsontable_renderer())
            
            df_homework_grades <- r$df_homework_grades
            homework_grades <- rhandsontable(df_homework_grades
                                             , rowHeaders = NULL
                                             , stretchH = 'all'
                                             , readOnly = T
                                             , height = "100%"
                                             , width = "100%") %>%
                hot_context_menu(allowRowEdit = FALSE) %>%
                hot_col(col = c(2:ncol(df_homework_grades)), type = "numeric")
            
            df_review <- df_review_grades %>%
                left_join(df_student, by = "student_id") %>%
                pivot_wider(id_cols = c(review_id, topic_id), names_from = name, values_from = grade) %>%
                rename(`Review ID` = review_id, `Topic ID` = topic_id)
            
            df_review_summary <- df_review %>%
                group_by(`Topic ID`) %>%
                mutate_at(c(3:(nrow(df_student) + 2)),  max_grade) %>%
                ungroup() %>%
                filter(!duplicated(`Topic ID`)) %>%
                select(-`Review ID`) %>%
                mutate(`Topic ID` = as.numeric(`Topic ID`))
            
            df_review_summary <- df_review_summary[order(df_review_summary$`Topic ID`),] %>%
                mutate(`Topic ID` = as.character(`Topic ID`))
            
            summary_grades <- rhandsontable(df_review_summary
                          , rowHeaders = NULL
                          , stretchH = 'all'
                          , readOnly = T
                          , height = "100%"
                          , width = "100%") %>%
                hot_cols(renderer = handsontable_renderer()) %>%
                hot_context_menu(allowRowEdit = FALSE)
            
            params <- list(student_table = student_table
                           , topic_table = topic_table
                           , homework_table = homework_table
                           , homework_grades = homework_grades
                           , review_grades = review_grades
                           , summary_grades = summary_grades)
            
            
            # Knit the document, passing in the `params` list, and eval it in a
            # child of the global environment (this isolates the code in the document
            # from the code in this app).
            rmarkdown::render(tempReport, output_file = file,
                              params = params,
                              envir = new.env(parent = globalenv())
            )
        }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)