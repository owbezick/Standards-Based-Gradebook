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
        hot_context_menu(allowComments = T) %>%
        hot_col(col = c("Review ID"), type = "numeric")
    })

    # Save topic table -----
    observeEvent(input$save, {
      df_hot <- hot_to_r(input$topicTable)
      
      #Get column of booleans (contains TRUE if date assigned comes after due date)
      df_date_ranges <- df_hot %>%
        mutate(range = (`Review Start Date` > `Review End Date`)) %>%
        select(range)
      
      #Check if date range is invalid
      if (TRUE %in% df_date_ranges$range) {
        showNotification("Please ensure date assigned comes before due date!", type = "warning")
      } else {
        # Save df_review_table
        r$df_review_table <- df_hot
        write_rds(r$df_review_table, "data/df_review_table.RDS")
        # Save df_review_grades
        save_df_review_grades()
        
        showNotification("Saved to remote.")
      }
    })


  })
}
