
# Course information button module ----
add_topic_button_UI <- function(id) {
  showModal(
    modalDialog(title = "Add Topic", size = "m"
                , fluidRow(
                  column(width = 6
                         , tags$b("Topic Number: ")
                         , numericInput(inputId = "topicNumber"
                                        , label = NULL
                                        , value = 0)

                  )
                  , column(width = 6
                           , tags$b("Topic Description: ")
                           , textAreaInput(inputId = "addHomeworkDescription"
                                           , label = NULL)
                  )
                )
    )
  )
}

add_topic_button_Server <- function(id){
  moduleServer(id, function(input,output,session){
    #TODO: add to data base once created ----
  })
}