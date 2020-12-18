# View Homework UI ----
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

# View Homework Server ----
view_calendar_hw_Server <- function(id, r){
  moduleServer(id, function(input,output,session){
    
    # OUT table ----
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
    
    # BTN Close ----
    observeEvent(input$close, {
      removeModal()
    })
    
    # BTN SAVE ----
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
      
      #df_homework_grades
      save_df_homework_grades()
      
      removeModal()
      r$cal_item <- NULL
      showNotification("Saved in session")
    })
    
    # BTN Delete ----
    observeEvent(input$delete, {
      
      cal_item <- r$cal_item
      # df_homework_grades
      drop <- c(paste("Homework", cal_item[1,1]))
      df_homework_grades <- r$df_homework_grades
      temp <- df_homework_grades[,!(names(df_homework_grades) %in% drop)]
      r$df_homework_grades <- temp
      
      # df_homework
      df_homework <- r$df_homework
      hw_id <- as.numeric(cal_item[1,1])
      temp <- subset(df_homework, id != hw_id)
      r$df_homework <- temp
      removeModal()
      r$cal_item <- NULL
      showNotification("Saved to remote.")
    })
    
  })
}
# View review UI ----
view_calendar_review_UI <- function(id, title){
  showModal(
    modalDialog(title = title, size = "l"
                , rHandsontableOutput(NS(id, "item_info_table"))
                , br()
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

# View Review server ----
view_calendar_review_Server <- function(id, r){
  moduleServer(id, function(input,output,session){
    # OUT table----
    output$item_info_table <- renderRHandsontable({
      req(r$cal_item)
      aligns <- c(rep("c", NCOL(r$cal_item)))
      
      rhandsontable(r$cal_item
                    , rowHeaders = NULL
                    , stretchH = 'all')
      
    })
    
    # BTN close -----
    observeEvent(input$close, {
      removeModal()
    })
    
    # BTN Save ----
    observeEvent(input$save, {
      r$cal_item <- hot_to_r(input$item_info_table)
      
      df_review_temp <- r$df_review_table
      
      df_review_temp[match(r$cal_item$`Review ID`, r$df_review_table$`Review ID`), ] <- r$cal_item 
    
      r$df_review_table <- df_review_temp
      
      removeModal()
      r$cal_item <- NULL
      showNotification("Saved in session")
    })
    
    # BTN Delete ----
    observeEvent(input$delete, {
      showModal(
        modalDialog(
          title = "Confrim Deletion"
          , size = "s"
          , footer = fluidRow(
            column(width = 6
                   , actionBttn(
                     inputId = NS(id, "hardDelete")
                     , label = "Yes"
                     , block = T
                   )
            )
            , column(width = 6
                     , actionBttn(
                       NS(id, "goBack")
                       , "Go back"
                       , block = T
                     )
            )
          )
        )
      )
    })
  
  observeEvent(input$hardDelete, {
    cal_item <- r$cal_item
    id <- as.numeric(cal_item[1,2]) 
    
    # df_review_grades
    df_review_grades <- r$df_review_grades
    temp <- subset(df_review_grades, review_id != id)
    r$df_review_grades <- temp
    
    # Review Table
    df_review_table <- r$df_review_table
    temp <- subset(df_review_table, `Review ID` != id)
    r$df_review_table <- temp
    
    showNotification("Saved in session.")
    r$cal_item <- NULL
    removeModal()
  })
  
  observeEvent(input$goBack, {
    r$cal_item <- NULL
    removeModal()
  })
  
  })
}