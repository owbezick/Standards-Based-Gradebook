library(formattable)
view_calendar_item_UI <- function(id, title){
  showModal(
    modalDialog(title = title, size = "l", footer = NULL
             , rHandsontableOutput(NS(id, "item_info_table"))
             , actionBttn(
               inputId = NS(id, "close")
               , label = "Close"
               , style = "material-flat"
               , block = T
             )
    )
    
  )
}

view_calendar_item_server <- function(id, r){
  moduleServer(id, function(input,output,session){
    
    output$item_info_table <- renderRHandsontable({
      #browser()
      #aligns <- c(rep("c", NCOL(r$df_cal_item)))
      
      rhandsontable(
        r$df_cal_item
        , rowHeaders = NULL
        , stretchH = 'all'
      ) %>%
        hot_context_menu(allowComments = F) %>%
        hot_cols(readOnly = T)
      
    })
    
    observeEvent(input$close, {
      removeModal()
    })
  })
}