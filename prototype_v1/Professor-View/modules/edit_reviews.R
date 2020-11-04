topics_UI <- function(id){
  box(width = 12, title = "Review Table", status = "primary"

      , rHandsontableOutput(NS(id, "topicTable"))
      , footer = "Right click to add a row to the table."

      , actionBttn(NS(id, "save"), "Save", style = "material-flat", block = T)

  )
}

topics_server <- function(id, r){
  moduleServer(id, function(input,output,session){
    # Topic Table Output ----
    output$topicTable <- renderRHandsontable({
      df <- r$df_review_table
      df <- df %>%
        mutate(`Review Start Date` = ymd(`Review Start Date`)
               , `Review End Date` = ymd(`Review End Date`)
               , `Review ID` = as.character(`Review ID`))
      rhandsontable(
        df
        , rowHeaders = NULL
        , stretchH = 'all'
      ) %>%
        hot_context_menu(allowComments = T)
    })

    # Save topic table -----
    observeEvent(input$save, {
      df_hot <- hot_to_r(input$topicTable)
      
      # Save df_review_table
      r$df_review_table <- df_hot

      # Save df_review_grades
      save_df_review_grades()
      
      showNotification("Saved to remote.")
    })




  })
}
