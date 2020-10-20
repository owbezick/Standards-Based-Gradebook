view_calendar_hw_UI <- function(id, title){
  showModal(
    modalDialog(title = title, size = "l"
                , formattableOutput(NS(id, "item_info_table"))
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

view_calendar_hw_Server <- function(id, r){
  moduleServer(id, function(input,output,session){
    
    output$item_info_table <- renderFormattable({
      aligns <- c(rep("c", NCOL(r$cal_item)))
      
      formattable(r$cal_item, align = aligns)
      
    })
    
    observeEvent(input$close, {
      removeModal()
    })
    
    observeEvent(input$delete, {
      # Homework_table
      cal_item <- r$cal_item
      drop <- c(paste("Homework", cal_item[1,1]))
      df_homework_table <- r$df_homework_table
      temp <- df_homework_table[,!(names(df_homework_table) %in% drop)]
      r$df_homework_table <- temp
      
      sheet_write(
        ss = "https://docs.google.com/spreadsheets/d/1xIC4pGhnnodwxqopHa45KRSHIVcOTxFSfJSEGPbQH20/editgid=2102408290"
        , data = temp
        , sheet = "homework_table"
      ) 
      
      # df homework
      df_homework <- r$df_homework
      hw_id <- cal_item[1,1] %>%
        pull()
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
    observeEvent(input$delete, {
      cal_item <- r$cal_item
      id <- cal_item[1,2] %>%
        pull()
      
      # Review to Topic
      df_review_to_topic <- r$df_review_to_topic
      temp <- subset(df_review_to_topic, review_id != id)
      r$df_review_to_topic <- temp
      sheet_write(
        ss = "https://docs.google.com/spreadsheets/d/1xIC4pGhnnodwxqopHa45KRSHIVcOTxFSfJSEGPbQH20/editgid=2102408290"
        , data = temp
        , sheet = "review_to_topic"
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