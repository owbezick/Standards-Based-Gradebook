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
      
      #browser()
      
      #Get column of booleans (contains TRUE if date assigned comes after due date)
      df_date_ranges <- df_hot %>%
        mutate(range = (`Review Start Date` > `Review End Date`)) %>%
        select(range)
      
      #Check if date range is invalid
      if (TRUE %in% df_date_ranges$range) {
        showNotification("Please ensure date assigned comes before due date!", type = "warning")
      } else {
        
            # Handling the case where a review is deleted from the edit review table
        
        #Find which reviews are left
        ids_remaining <- df_hot$`Review ID`
        
        #Use df_review_grades to compare
        df_review_grades <- r$df_review_grades
        ids_original <- df_review_grades$review_id %>%
          unique()
        
        #See if row was deleted
        if (length(ids_remaining) != length(ids_original)){
          # df_review_grades
          temp <- subset(df_review_grades, review_id %in% ids_remaining)
          r$df_review_grades <- temp
        }
        
        # Save df_review_table
        r$df_review_table <- df_hot
        
        # Save df_review_grades
        save_df_review_grades()
        
        showNotification("Saved to remote.")
      }
    })


  })
}
