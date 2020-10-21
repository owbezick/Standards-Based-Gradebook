topics_UI <- function(id){
  box(width = 12, title = "Review Table", status = "primary"
            , rHandsontableOutput(NS(id, "topicTable"))
  )
}

topics_server <- function(id, r){
  moduleServer(id, function(input,output,session){
    output$topicTable <- renderRHandsontable({
      df <- r$df_review_table
      
      df <- df %>%
        mutate(`Review Date` = ymd(`Review Date`)
               , `Review ID` = as.character(`Review ID`))
      rhandsontable(
        df
        , rowHeaders = NULL
        , stretchH = 'all'
      ) %>%
        hot_context_menu(allowComments = F) %>%
        hot_cols(readOnly = T)
    })
    
    observeEvent(input$save, {
      df <- hot_to_r(input$topicTable)
      r$df_review_table <- df
      sheet_write(
        ss =  "https://docs.google.com/spreadsheets/d/1xIC4pGhnnodwxqopHa45KRSHIVcOTxFSfJSEGPbQH20/edit#gid=2102408290"
        , data = df
        , sheet = "review_table"
      )
      showNotification("Saved to remote.")
    })
    
  })
}
