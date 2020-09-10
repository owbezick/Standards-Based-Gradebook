edit_and_add_UI <- function(id){
  box(width = 4, title = "Edit & Add", status = "primary"
      , column(width = 12
               , actionBttn(inputId = NS(id, "editCourseInfo"), label = "Course Information", style = "material-flat", block = T)
               , br()
      )
      , column(width = 6
               , actionBttn(inputId = NS(id, "addHomework"), label = "Homework", style = "material-flat", block = T)
      )
      , column(width = 6
               , actionBttn(inputId = NS(id, "addReview"), label = "Review", style = "material-flat", block = T)
      )
  )
}

edit_and_add_server <- function(id){
  moduleServer(id, function(input, output, session) {
    #TODO: wrap into separate modules for each button 
    observeEvent(NS(id, input$editCourseInfo), {
      showModal(
        modalDialog(title = "Edit Course Information", size = "l"
        )
      )
    }, ignoreInit = F) # Maybe set to F for first time only?
    
    
    observeEvent(NS(id, input$addHomework), {
      showModal(
        modalDialog(title = "Add Homework", size = "l")
      )
    }, ignoreInit = T)
    
    observeEvent(NS(id, input$addReview), {
      showModal(
        modalDialog(title = "Add Review", size = "l")
      )
    }, ignoreInit = T)
    
  })
  
}