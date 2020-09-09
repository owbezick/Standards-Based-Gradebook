edit_and_add_UI <- function(id){
   box(width = 4, title = "Edit & Add", status = "primary"
        , column(width = 12
                 , actionBttn(inputId = "courseInfo", label = "Course Information", style = "material-flat", block = T)
                 , br()
        )
        , column(width = 6
                 , actionBttn(inputId = "addHomework", label = "Homework", style = "material-flat", block = T)
        )
        , column(width = 6
                 , actionBttn(inputId = "addReview", label = "Review", style = "material-flat", block = T)
        )
  )
}

edit_and_add_server <- function(id){
  moduleServer(id, function(input,output,session){
    
    
  })

}