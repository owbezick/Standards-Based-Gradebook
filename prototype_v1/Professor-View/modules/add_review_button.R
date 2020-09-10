
# Course information button module ----
add_review_button_UI <- function(id) {
  showModal(
    modalDialog(title = "Add Review", size = "l"
                , fluidRow(
                  column(width = 6
                         , tags$b("Review Number: ")
                         , numericInput(inputId = "addReviewNumber"
                                        , label = NULL
                                        , value = 0)
                         , br()
                         , tags$b("Review Description: ")
                         , textAreaInput(inputId = "addReviewDescription"
                                         , label = NULL)
                  )
                  , column(width = 6
                           , tags$b("Date: ")
                           , dateInput(inputId = "addReviewDate"
                                       , label = NULL)
                           , br()
                           , tags$b("Select Topics: ")
                           , checkboxGroupInput(inputId = "addReviewTopics"
                                       , label = NULL
                                       , choices = c("One", "Two", "Three")
                                       )
                  )
                )
    )
  )
}

add_review_button_Server <- function(id){
  moduleServer(id, function(input,output,session){
    #TODO: add to data base once created ----
  })
}