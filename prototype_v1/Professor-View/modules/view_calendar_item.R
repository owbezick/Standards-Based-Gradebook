view_calendar_hw_UI <- function(id, title){
  showModal(
    modalDialog(title = title, size = "l"
                , rHandsontableOutput(NS(id, "item_info_table"))
                , footer = fluidRow(
                  column(width = 4
                           , actionBttn(
                             inputId = NS(id, "save")
                             , label = "save"
                             , style = "material-flat"
                             , block = T
                           )
                  )
                  , column(width = 4
                         , actionBttn(
                           inputId = NS(id, "delete")
                           , label = "Delete"
                           , style = "material-flat"
                           , block = T
                         )
                  )
                  , column(width = 4
                           , actionBttn(
                             inputId = NS(id, "close")
                             , label = "Close"
                             , style = "material-flat"
                             , block = T
                           )
                  )
                )
                
    )
    
  )
}

# Homework Item Server ----
view_calendar_hw_Server <- function(id, r){
  moduleServer(id, function(input,output,session){
    
    output$item_info_table <- renderRHandsontable({
      df <- r$cal_item
      
      df <- df %>%
        mutate(`Date Assigned` = ymd(`Date Assigned`)
               , `Date Due` = ymd(`Date Due`))
      
      rhandsontable(
        df
        , rowHeaders = NULL
        , stretchH = 'all'
      ) %>%
      hot_col("Homework Number", readOnly = TRUE)
    })
    
    observeEvent(input$close, {
      removeModal()
    })
    
    observeEvent(input$save, {
      r$cal_item <- hot_to_r(input$item_info_table)
      
      df_item <- r$cal_item %>%
        select(id = `Homework Number`
               , description = Description
               , date_assigned = `Date Assigned`
               , date_due = `Date Due`)
      
      df_hw <- r$df_homework
      
      df_hw[match(df_item$id, df_hw$id), ] <- df_item
      
      r$df_homework <- df_hw
      sheet_write(
        ss = "https://docs.google.com/spreadsheets/d/1xIC4pGhnnodwxqopHa45KRSHIVcOTxFSfJSEGPbQH20/editgid=2102408290"
        , data = r$df_homework
        , sheet = "homework"
      ) 
      removeModal()
      showNotification("Saved to remote.")
    })
    
    observeEvent(input$delete, {
      # Save df_homework_grades ----
      cal_item <- r$cal_item
      drop <- c(paste("Homework", cal_item[1,1]))
      df_homework_grades <- r$df_homework_grades
      temp <- df_homework_grades[,!(names(df_homework_grades) %in% drop)]
      r$df_homework_grades <- temp
      sheet_write(
        ss = "https://docs.google.com/spreadsheets/d/1xIC4pGhnnodwxqopHa45KRSHIVcOTxFSfJSEGPbQH20/editgid=2102408290"
        , data = temp
        , sheet = "homework_grades"
      ) 
      
      
      # Save df_homework ----
      df_homework <- r$df_homework
      hw_id <- as.numeric(cal_item[1,1])
      temp <- subset(df_homework, id != hw_id)
      r$df_homework <- temp
      sheet_write(
        ss = "https://docs.google.com/spreadsheets/d/1xIC4pGhnnodwxqopHa45KRSHIVcOTxFSfJSEGPbQH20/editgid=2102408290"
        , data = temp
        , sheet = "homework"
      ) 
      removeModal()
      showNotification("Saved to remote.")
    })
    
  })
}
view_calendar_review_UI <- function(id, title){
  showModal(
    modalDialog(title = title, size = "l"
                , rHandsontableOutput(NS(id, "item_info_table"))
                , br()
                , footer = fluidRow(
                  column(width = 6
                         , actionBttn(
                           inputId = NS(id, "delete")
                           , label = "Delete"
                           , style = "material-flat"
                           , block = T
                         )
                  )
                  , column(width = 6
                           , actionBttn(
                             inputId = NS(id, "close")
                             , label = "Close"
                             , style = "material-flat"
                             , block = T
                           )
                  )
                )
    )
    
  )
}

# View Review Item Server ----
view_calendar_review_Server <- function(id, r){
  moduleServer(id, function(input,output,session){
    
    output$item_info_table <- renderRHandsontable({
      aligns <- c(rep("c", NCOL(r$cal_item)))
      
      rhandsontable(r$cal_item, rowHeaders = F) %>%
        hot_cols(readOnly = T)
      
    })
    
    observeEvent(input$close, {
      removeModal()
    })
    # Deletion ---- 
    observeEvent(input$delete, {
      cal_item <- r$cal_item
      id <- as.numeric(cal_item[1,2]) 
      
      # df_review_grades
      df_review_grades <- r$df_review_grades
      temp <- subset(df_review_grades, review_id != id)
      r$df_review_grades <- temp
      sheet_write(
        ss = "https://docs.google.com/spreadsheets/d/1xIC4pGhnnodwxqopHa45KRSHIVcOTxFSfJSEGPbQH20/editgid=2102408290"
        , data = temp
        , sheet = "review_grades"
      ) 
      
      # Review Table
      df_review_table <- r$df_review_table
      temp <- subset(df_review_table, `Review ID` != id)
      r$df_review_table <- temp
      sheet_write(
        ss = "https://docs.google.com/spreadsheets/d/1xIC4pGhnnodwxqopHa45KRSHIVcOTxFSfJSEGPbQH20/editgid=2102408290"
        , data = temp
        , sheet = "review_table"
      ) 
      showNotification("Saved to remote.")
      removeModal()
    })
  })
}