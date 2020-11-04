# UI -----
edit_and_add_UI <- function(id){
  box(width = 4, title = "Edit:", status = "primary", height = "225px"
      , fluidRow(
        column(width = 12
               , actionBttn(inputId = NS(id, "editCourseInfo")
                            , label = "Course Info"
                            , style = "material-flat"
                            , block = T)
        )
      )
      , br()
      , fluidRow(
        column(width = 12
               , actionBttn(inputId = NS(id, "editRoster")
                            , label = "Course Roster"
                            , style = "material-flat"
                            , block = T)
        )
      )
      , br()
      , fluidRow(
        column(width = 6
               , actionBttn(inputId = NS(id, "addTopic")
                            , label = "Topic", style = "material-flat", block = T)
        )
        , column(width = 6
                 , actionBttn(inputId = NS(id, "addHomework")
                              , label = "Homework", style = "material-flat", block = T)
        )
      )
  )
}

edit_and_add_server <- function(id, r){
  moduleServer(id, function(input, output, session) {
    
    observeEvent(NS(id, input$editCourseInfo), {
      course_information_button_UI("course_info_button", r)
    }, ignoreInit = T) 
    
    observeEvent(NS(id, input$addHomework), {
      add_homework_button_UI("add_homework")
    }, ignoreInit = T)
    
    observeEvent(NS(id, input$addTopic), {
      add_topic_button_UI("add_topic")
    }, ignoreInit = T)
    
    observeEvent(NS(id, input$editRoster), {
      edit_roster_button_UI("edit_roster")
    }, ignoreInit = T)
  })
  add_homework_button_Server("add_homework", r)
  course_information_button_Server("course_info_button", r)
  add_topic_button_Server("add_topic", r)
  edit_roster_button_Server("edit_roster", r)
}
