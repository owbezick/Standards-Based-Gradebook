
# Course information button module ----
course_information_button_UI <- function(id, r) {
  df <- r$df_course_info
  showModal(
    modalDialog(title = "Edit Course Information & Links", size = "l", easyClose = T
                , fluidRow(
                  column(width = 6
                         , rHandsontableOutput(NS(id, "course_info"))
                  )
                  , column(width = 6
                           , rHandsontableOutput(NS(id, "links"))
                  )
                )
                , footer = fluidRow(
                  column(width = 12
                         , actionBttn(inputId = NS(id,"save")
                                      , label = "Save Information"
                                      , style = "material-flat"
                                      , block = T
                         )
                  )
                )
    )
  )
}

course_information_button_Server <- function(id, r){
  moduleServer(id, function(input,output,session){
    output$course_info <- renderRHandsontable({
      rhandsontable(
        r$df_course_info
        , rowHeaders = NULL
        , stretchH = 'all'
      )
    })
    
    output$links <- renderRHandsontable({
      rhandsontable(
        r$df_links
        , rowHeaders = NULL
        , stretchH = 'all'
      )
    })
    
    
    observeEvent(input$save, {
      r$df_course_info <- hot_to_r(input$course_info)
      r$df_links <- hot_to_r(input$links)
      showNotification("Saved in session.")
      removeModal()
      
    })
    
  })
}