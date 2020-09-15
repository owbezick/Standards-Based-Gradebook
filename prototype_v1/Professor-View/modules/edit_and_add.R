# Edit and add module ----
edit_and_add_UI <- function(id){
  box(width = 4, title = "Edit & Add", status = "primary"
      , column(width = 6
               , actionBttn(inputId = NS(id, "editCourseInfo")
                            , label = "Course Info", style = "material-flat", block = T)
               , br()
               , actionBttn(inputId = NS(id, "addHomework")
                            , label = "Homework", style = "material-flat", block = T)
               , br()
      )
      , column(width = 6
               , actionBttn(inputId = NS(id, "addTopic")
                            , label = "Topic", style = "material-flat", block = T)
               , br()
               , actionBttn(inputId = NS(id, "addReview")
                            , label = "Review", style = "material-flat", block = T)
               , br()
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
    
    observeEvent(NS(id, input$addReview), {
      add_review_button_UI("add_review")
    }, ignoreInit = T)
    
    observeEvent(NS(id, input$addTopic ), {
      add_topic_button_UI("add_topic")
    }, ignoreInit = T)

  })
  
  course_information_button_Server("course_info_button", r)
  add_topic_button_Server("add_topic", r)
  
  add_homework_button_Server("add_homework", r)
  add_review_button_UI("add_review")
}
