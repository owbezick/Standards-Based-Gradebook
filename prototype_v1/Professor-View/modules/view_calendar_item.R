view_calendar_item_UI <- function(id, title){
  showModal(
    modalDialog(title = title, size = "l", footer = NULL
             , formattableOutput(NS(id, "item_info_table"))
             , actionBttn(
               inputId = NS(id, "close")
               , label = "Close"
               , style = "material-flat"
               , block = T
             )
    )
    
  )
}

view_calendar_item_Server <- function(id, r){
  moduleServer(id, function(input,output,session){
    
    output$item_info_table <- renderFormattable({
      aligns <- c(rep("c", NCOL(r$cal_item)))
      
      formattable(r$cal_item, align = aligns)
      
    })
    
    observeEvent(input$close, {
      removeModal()
    })
  })
}