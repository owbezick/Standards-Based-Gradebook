
# Course information button module ----
add_homework_button_UI <- function(id) {
  showModal(
    modalDialog(title = "Add Homework", size = "l"
                , fluidRow(
                  column(width = 6
                         , tags$b("Homework Number: ")
                         , numericInput(inputId = "homeworkNumber"
                                     , label = NULL
                                     , value = 0)
                         , br()
                         , tags$b("Homework Description: ")
                         , textAreaInput(inputId = "addHomeworkDescription"
                                         , label = NULL)
                  )
                  , column(width = 6
                           , tags$b("Date Due: ")
                           , dateInput(inputId = "homeworkDateDue"
                                       , label = NULL)
                           , br()
                           , tags$b("Date Assigned: ")
                           , dateInput(inputId = "homeworkDateAssigned"
                                       , label = NULL)
                  )
                )
    )
  )
}

add_homework_button_Server <- function(id){
  moduleServer(id, function(input,output,session){
    #TODO: add to data base once created ----
  })
}